library(tidyverse)
library(ggrepel)
library(stringr)
library(DataExplorer)
library(data.table)
library(janitor)
library(ggalluvial)
library(ggExtra)
library(quantreg)
library(caret)
library(xgboost)
library(doParallel)

setwd("C:/Users/ldch90/Downloads/titanic")

train <- fread("train.csv") %>% clean_names()
test <- fread("test.csv") %>% clean_names()
train$cabin[nchar(train$cabin) == 0] <- NA
test$cabin[nchar(test$cabin) == 0] <- NA
test$survived <- NA
# test %>% mutate(survived = NA)

titanic <- rbind(train, test)

colSums(is.na(titanic))

for (i in c(train,test)){
  str(i)
  summary(i)
  glimpse(i)
  dim(i)
}


# Feature Engineering

## age group

titanic <- titanic %>% 
  mutate(age_group = age %>% cut(breaks = c(0,20,40,60,80), labels = c("minor","young adult","mid-age","aged")))

## Pclass

titanic <- titanic %>% 
  mutate(pclass = pclass %>% factor(levels = c(1,2,3), labels = c("high","mid","low")))


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

# # cabin alphabet
# 
# titanic <- titanic %>% 
#   mutate(cabin_alpha = cabin %>% str_extract("[:alpha:]+"))
# 
# #  occupation
# 
# titanic <- titanic %>% 
#   mutate(occupation = cabin %>% str_count("[:alnum:]+"))
# 
# # raw fare
# 
# titanic <- titanic %>% 
#   mutate(occu_per_fare = fare/occupation)

## impute age data

titanic$age[titanic$sib_sp + titanic$parch == 0 & is.na(titanic$age)] <- median(titanic$age[titanic$age_group %in% c("young adult","mid-age","aged")])


for (i in unique(titanic$title)){
  titanic$age[titanic$title == i & is.na(titanic$age)] <- median(titanic$age[titanic$title == i ], na.rm = T)
}

## age group

titanic <- titanic %>% 
  mutate(age_group = age %>% cut(breaks = c(0,20,40,60,80), labels = c("minor","young adult","mid-age","aged")))


# embarked imputation

unique(titanic$embarked)

titanic %>% group_by(embarked) %>% tally()

titanic$embarked[nchar(titanic$embarked) == 0 ] <- "S"
titanic$embarked <- factor(titanic$embarked)

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

titanic <- titanic %>% 
  mutate(age_group = age %>% cut(breaks = c(0,20,40,60,80), labels = c("minor","young adult","mid-age","aged")))


# checking missing part again

colSums(is.na(titanic %>% select(-c(survived,cabin))))

# colSums(is.na(titanic)) %>% data.frame()

sum(is.na(titanic %>% select(-c(survived,cabin)))) 

where_na(titanic %>% select(-c(survived,cabin)))

# Model building 

# Feature Selection


# Modeling
ml_df <- titanic %>% filter(!is.na(survived)) %>% select(survived,pclass,age,sex,sib_sp,parch,fare,embarked,age_group,title,fa) 
# ml_df <- ml_df %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, function(x) {as.integer(x) - 1})

set.seed(1000)
model <- createDataPartition(ml_df$survived, p = 0.7, list = F)
# createFolds(train$survived, 10, list = F)
train <- ml_df[model,]
test <- ml_df[-model,]



tr_label <- train$survived 
ts_label <- test$survived 
total_label <- ml_df$survived
new_tr <- model.matrix(~.+0, data = train %>% select(-survived), with = F)
new_ts <- model.matrix(~.+0, data = test %>% select(-survived), with = F)
new_total <- model.matrix(~.+0, data = ml_df %>% select(-survived), with = F)

dtrain <- xgb.DMatrix(new_tr, label = tr_label)
dtest <- xgb.DMatrix(new_ts, label = ts_label)
dtotal <- xgb.DMatrix(new_total, label = total_label)

# cl <- makeCluster(3)
# registerDoParallel(cl)

xgb_grid <- expand.grid(
  nrounds = 500,
  max_depth = c(3,6,9),
  eta = c(0.1, 0.01, 0.001),
  gamma = c(1),
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = c(1, 10, 15, 20),
  subsample = c(0.5, 0.75, 1)
)

# grid_search = foreach(i = 1:nrow(xgb_grid), .combine = rbind, .packages = c("xgboost", "caret")) %dopar% {
#   model = xgb.cv(data = new_tr,
#                  label = tr_label,
#                  objective = "binary:logistic", 
#                  nrounds = 500, 
#                  nfold = 10, metrics = "mae", 
#                  early_stopping_rounds = 100,
#                  params = xgb_grid[i,])
#   data.frame(train_mae_last = unlist(model$evaluation_log[,2] %>% last),
#              test_mae_last = unlist(model$evaluation_log[,4] %>% last))
# }
# 
# stopCluster(cl)



cv_matrix <- xgb.cv(data = dtrain, objective = "binary:logistic", nrounds = 500, nfold = 10, metrics = list("mae","error", "auc"), early_stopping_rounds = 100)


xgb_trcontrol <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.2,
  gamma = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1,
  eval_metric = "mae", # rmse, rmsle, error, mae, auc
  scale_pos_weight = 549/342
)

watchlist <- list(train = dtrain, eval = dtest)

xgb_model <- xgb.train(params = params, data=dtrain, nrounds = 500, trControl = xgb_trcontrol, watchlist = watchlist, early_stopping_rounds = 100)
xgb_model2 <- xgboost(data = new_tr, verbosity = 2, label = tr_label, objective = "binary:logistic", nrounds = 500, verbose = 2, early_stopping_rounds = 100, trControl = xgb_trcontrol)
xgb_model3 <- train(x= new_tr, y = factor(tr_label), method = "xgbTree", trControl = xgb_trcontrol)

xgb_model3$bestTune

imp_matrix <- xgb.importance (feature_names = colnames(new_tr),model = xgb_model)
imp_matrix2 <- xgb.importance (feature_names = colnames(new_tr),model = xgb_model2)
xgb.plot.importance(imp_matrix[1:10])
xgb.plot.importance(imp_matrix2[1:10])
xgb.plot.tree(model = xgb_model2, trees = 0, show_node_id = TRUE)


# Importance of dimensional reduction!
dtrain %>% colnames()
dtest %>% colnames()

test <- test %>% mutate(pred_pct = predict(xgb_model, dtest), pred = ifelse(pred_pct > 0.5, 1, 0))
test <- test %>% mutate(pred_pct2 = predict(xgb_model2, dtest), pred2 = ifelse(pred_pct > 0.5, 1, 0))
test <- test %>% mutate(pred3 = predict(xgb_model3, new_ts))

confusionMatrix(factor(test$pred), factor(test$survived))
confusionMatrix(factor(test$pred2), factor(test$survived))
confusionMatrix(test$pred3, factor(test$survived))

ml_df <- ml_df %>% mutate(pred_pct = predict(xgb_model, dtotal), pred = ifelse(pred_pct > 0.5, 1, 0))
ml_df <- ml_df %>% mutate(pred_pct2 = predict(xgb_model2, dtotal), pred2 = ifelse(pred_pct2 > 0.5, 1, 0))
ml_df <- ml_df %>% mutate(pred3 = predict(xgb_model3, new_total))

confusionMatrix(factor(ml_df$pred), factor(ml_df$survived)) # 91.69%
confusionMatrix(factor(ml_df$pred2), factor(ml_df$survived)) # 91%
confusionMatrix(ml_df$pred3, factor(ml_df$survived)) # 86%

# Visualization

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


titanic %>% 
  ggplot(aes(x = sex, y = title)) +
  geom_count(aes(size = ..prop..), color = "gray", alpha = .5) +
  geom_count(aes(size = ..prop.., group = 1, color = "red")) +
  geom_text(data = train %>% group_by(sex, title) %>% tally(), aes(x = sex, y = title, label = n)) +
  scale_size_area(max_size = 25, guide = F) +
  guides(color = F) +
  theme_minimal() 


titanic %>% group_by(sex, age_group, survived) %>% tally() %>% 
  ggplot(data= . , aes(axis1 = sex, axis2 = age_group, axis3 = survived, y = n)) +
  geom_alluvium(aes(fill = survived), width = 0, knot.pos = 0) +
  guides(fill = F) +
  geom_stratum(width = 0.1, infer.label = T) +
  geom_label(stat = "stratum", infer.label = T) +
  scale_x_continuous(breaks = c(1,2,3) , labels = c("sex", "age group", "survived")) +
  theme_minimal()



titanic %>% ggplot(aes(x = factor(pclass), y = fare)) +
  geom_jitter(aes(color = factor(pclass)), alpha = 0.5) +
  geom_boxplot(aes(color = factor(pclass)), width = 0.1) +
  geom_violin(aes(fill = factor(pclass))) +
  theme_minimal() +
  scale_color_brewer(palette = 3) +
  scale_fill_brewer(palette = 3)