library(tidyverse)
library(ggrepel)
library(stringr)
library(DataExplorer)
library(data.table)
library(janitor)
library(caret)
library(xgboost)
library(ggalluvial)

setwd("C:/Users/dchan/Downloads/titanic")

# clean variable(column name)
train <- fread("train.csv") %>% clean_names()
test <- fread("test.csv") %>% clean_names()

# change empty string as NA
train$cabin[nchar(train$cabin) == 0] <- NA # basic R
tain <- train %>% 
  mutate(cabin = ifelse(cabin %>% nchar() == 0, NA, cabin)) # tidyverse
test$cabin[nchar(train$cabin) == 0] <- NA
test$survived <- NA

titanic <- rbind(train, test)
# colnames col_num identical

# attributes of data
for (i in c(train,test)){
  str(i)
  summary(i)
  glimpse(i)
  dim(i)
}

str(train)
str(test)

missing_clock_plot <-  function(x){
  colSums(is.na(x)) %>% 
    data.frame() %>% 
    rownames_to_column() %>%
    mutate(pct = ./nrow(x)) %>% 
    ggplot(aes(x = rowname, y = .)) +
    geom_text(aes(label = ifelse(pct == 0, "", paste(round(pct,2) * 100,"%")), size = 5), show.legend = F) +
    geom_bar(stat = "identity", fill = "red", alpha = 0.5) +
    coord_polar(theta = "x") +
    labs(x = "Missing number", y = "Variables", title = "Missing Value per Variable") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 10, family = "sans"))
}

missing_clock_plot(train)
missing_clock_plot(test)  
missing_clock_plot(titanic)


train %>% 
  group_by(survived) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n), ymax = cumsum(n), ymin = c(0, head(ymax, -1)), label = paste(round(prop,2)*100,"%"), label_position = (ymax + ymin)/2) %>% 
  ggplot() +
  geom_rect(aes(xmin = 1, xmax = 2, ymin = ymin, ymax = ymax, fill = factor(survived))) +
  geom_label(x = 1.5, aes(color = factor(survived), label = label, y = label_position), size = 7) +
  coord_polar(theta = "y") +
  xlim(c(0,2)) +
  theme_void() +
  scale_color_brewer(palette = 1) +
  scale_fill_brewer(palette=1) +
  theme(legend.position = "none") +
  annotate("text", label = "Unsurvived VS Survived", x = 0, y = 0, size = 8, color = "gray")

train %>% 
  ggplot(aes(x = sex, y = title)) +
  geom_count(aes(size = ..prop..), color = "gray", alpha = .5) +
  geom_count(aes(size = ..prop.., group = 1, color = "red")) +
  geom_text(data = train %>% group_by(sex, title) %>% tally(), aes(x = sex, y = title, label = n)) +
  scale_size_area(max_size = 10, guide = F) +
  guides(color = F) +
  theme_minimal() 

train %>% group_by(sex, age_group, survived) %>% tally() %>% 
  ggplot(data= . , aes(axis1 = sex, axis2 = age_group, axis3 = survived, y = n)) +
  geom_alluvium(aes(fill = survived), width = 0, knot.pos = 0) +
  guides(fill = F) +
  geom_stratum(width = 0.1, infer.label = T) +
  geom_label(stat = "stratum", infer.label = T) +
  scale_x_continuous(breaks = c(1,2,3) , labels = c("sex", "age group", "survived")) +
  theme_minimal()

# Feature Engineering

## Pclass

titanic <- titanic %>% 
  mutate(pclass = pclass %>% factor(levels = c(1,2,3), labels = c("high","mid","low")))

## age group
titanic <- titanic %>% 
  mutate(age_group = age %>% cut(breaks = c(0,20,40,60,80), labels = c("minor","young adult","mid-age","aged")))

## title 
titanic <- titanic %>% 
  mutate(title = name %>% str_extract("[:alpha:]+(?=\\.)"))

table(titanic$title) %>% data.frame() %>% arrange(-Freq)


# Family name 
titanic <- titanic %>% 
  mutate(family = name %>% str_extract("[:alpha:]+(?=,)"))

# Family number
titanic <- titanic %>% add_count(family, name = "fam_count")

# The number of person per room

titanic <- titanic %>% add_count(ticket, name = "related_number")

# family & aline

titanic <-
  titanic %>% 
  mutate(fa = case_when(
    sib_sp + parch == 0 ~  "alone",
    fam_count == sib_sp + parch + 1 ~ family,
    related_number == sib_sp + parch + 1 ~ family,
    TRUE ~ "Unknown"
  ))

titanic$fa[titanic$fa == "Unknown"] <- titanic$name[titanic$fa == "Unknown"] %>% str_extract("[:alpha:]+(?=\\))")
titanic$fa[titanic$fa %>% is.na()] <-  titanic$family[titanic$fa %>% is.na()]

titanic <- titanic %>% 
  mutate(fa = ifelse(fa == "alone", "alone", "family"))

## impute age data

titanic$age[titanic$sib_sp + titanic$parch == 0] <- median(titanic$age[titanic$age_group %in% c("young adult","mid-age","aged")])

sum(is.na(titanic %>% select(-c(survived,cabin))))

titanic <- titanic %>% 
  mutate(age_group = age %>% cut(breaks = c(0,20,40,60,80), labels = c("minor","young adult","mid-age","aged")))

## title revision

titanic <-  titanic %>% 
  mutate(title = case_when(
    title %in% c("Capt","Col","Dr","Rev", "Major") ~ "Officer",
    title %in% c("Don","Dona","Jonkheer","Lady","Countess","Sir") ~ "Noble",
    title %in% c("Miss","Mlle") ~ "Miss",
    title %in% c("Mme", "Mrs", "Ms") ~"Mrs",
    title %in% c("Master") ~ "Master",
    title %in% c("Mr") ~ "Mr"
  ))

for (i in unique(titanic$title)){
  titanic$age[titanic$title == i & is.na(titanic$age)] <- median(titanic$age[titanic$title == i ], na.rm = T)
}

# Model building 

# Feature Selection

# Modeling
ml_df <- titanic %>% filter(!is.na(survived)) %>% select(survived,pclass,sex,sib_sp,parch,fare,embarked,age_group,title,fa) 
# ml_df <- ml_df %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, function(x) {as.integer(x) - 1})

set.seed(1000)
model <- createDataPartition(ml_df$survived, p = 0.7, list = F)
train <- ml_df[model,]
test <- ml_df[-model,]



tr_label <- train$survived 
ts_label <- test$survived 
new_tr <- model.matrix(~.+0, data = train %>% select(-survived), with = F)
new_ts <- model.matrix(~.+0, data = test %>% select(-survived), with = F)

dtrain <- xgb.DMatrix(new_tr, label = tr_label)
dtest <- xgb.DMatrix(new_ts, label = ts_label)

xgb.cv(data = dtrain, label = tr_label, nfold = 20, nround = 20, objective = "binary:logistic")

xgb_model <- xgb.train(data=dtrain, max_depth= ncol(dtrain), eta= 0.1, nthread = 20, nrounds= 20, objective = "binary:logistic")
imp_matrix <- xgb.importance (feature_names = colnames(new_tr),model = xgb_model)
xgb.plot.importance(imp_matrix[1:10])

# Importance of dimensional reduction!
dtrain %>% colnames()
dtest %>% colnames()

test <- test %>% mutate(pred_pct = predict(xgb_model, dtest), pred = ifelse(pred_pct > 0.5, 1, 0))

confusionMatrix(factor(test$pred), factor(test$survived))
