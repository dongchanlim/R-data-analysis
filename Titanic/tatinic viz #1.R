library(tidyverse)
library(ggrepel)
library(stringr)
library(DataExplorer)
library(data.table)
library(janitor)
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
  group_by(Survived) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n), ymax = cumsum(n), ymin = c(0, head(ymax, -1)), label = paste(round(prop,2)*100,"%"), label_position = (ymax + ymin)/2) %>% 
  ggplot() +
  geom_rect(aes(xmin = 1, xmax = 2, ymin = ymin, ymax = ymax, fill = factor(Survived))) +
  geom_label(x = 1.5, aes(color = factor(Survived), label = label, y = label_position), size = 7) +
  coord_polar(theta = "y") +
  xlim(c(0,2)) +
  theme_void() +
  scale_color_brewer(palette = 1) +
  scale_fill_brewer(palette=1) +
  theme(legend.position = "none")

            