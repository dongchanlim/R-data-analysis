library(tidyverse)
library(ggrepel)
library(stringr)
library(DataExplorer)
library(data.table)
library(janitor)
library(ggalluvial)
library(ggExtra)

setwd("C:/Users/ldch90/Downloads/titanic")

train <- fread("train.csv") %>% clean_names()
test <- fread("test.csv") %>% clean_names()
train$Cabin[nchar(train$Cabin) == 0] <- NA
test$Cabin[nchar(train$Cabin) == 0] <- NA
test$Survived <- NA

titanic <- rbind(train, test)

for (i in c(train,test)){
  str(i)
  summary(i)
  glimpse(i)
  dim(i)
}

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

train <- train %>% 
  mutate(title = name %>% str_extract("[:alpha:]{2,}(?=\\.)")) 



train %>% 
  ggplot(aes(x = sex, y = title)) +
  geom_count(aes(size = ..prop..), color = "gray", alpha = .5) +
  geom_count(aes(size = ..prop.., group = 1, color = "red")) +
  geom_text(data = train %>% group_by(sex, title) %>% tally(), aes(x = sex, y = title, label = n)) +
  scale_size_area(max_size = 10, guide = F) +
  guides(color = F) +
  theme_minimal() 


train$age %>% quantile(na.rm = T)

train$age[is.na(train$age)] <- median(train$age, na.rm = T)

train <- train %>% mutate(age_group = cut(age,4, breaks = c(0,20,30,50,80), labels = c("child","young adult","mid","aged")),
                          accompanied = sib_sp + parch,
                          embarked = as.factor(embarked))



  
train %>% group_by(sex, age_group, survived) %>% tally() %>% 
  ggplot(data= . , aes(axis1 = sex, axis2 = age_group, axis3 = survived, y = n)) +
  geom_alluvium(aes(fill = survived), width = 0, knot.pos = 0) +
  guides(fill = F) +
  geom_stratum(width = 0.3, infer.label = T) +
  geom_label(stat = "stratum", infer.label = T) +
  scale_x_continuous(breaks = c(1,2,3) , labels = c("sex, age group", "survived")) +
  theme_minimal()
            