---
title: "BYU-I career fair data"
author: "Dongchan Lim"
date: "4월 11, 2019"
output:
  html_document:
    theme: cerulean 
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---






```r
# Use this R-Chunk to import all your datasets!
careerfair <- read_csv("BYUI_FallCareerFair.csv")
```

## Background

This is a summary of BYU-I career fair data

## Data Wrangling


```r
# Use this R-Chunk to clean & wrangle your data!
careerfair_1 <- careerfair %>% mutate(State = str_to_upper(State) %>% fct_relevel(state.abb), City =  as.factor(str_to_upper(City))) %>% filter(`Organization Type` == 'Employer', !is.na(State), !is.na(City)) 

cities <-
us_cities() %>% mutate(City = city %>% str_to_upper()) %>% rename(State = state_abbr)

samecols_city <-
intersect(careerfair_1$City %>% unique(), cities$City %>% unique()) 

samecols_state <-
  intersect(careerfair_1$State %>% unique(), cities$State %>% unique()) 


states <-
us_states() %>% 
  filter(!(state_name %in% c('Alaska','Hawaii','Puerto Rico'))) %>% mutate(flag =  ifelse(state_abbr %in% samecols_state, "job", NA))


careerfair_geom <-
  careerfair_1 %>% merge(cities, by = c("City","State")) %>%   filter(!(state_name %in% c('Alaska','Hawaii','Puerto Rico')))

careerfair_2 <- 
  careerfair_1 %>% 
  unite(list,`College Majors Ag/Life Sciences`:`College Majors Found/Interd Studies`, sep = ",") %>% 
  mutate(list = list %>% str_to_lower(), data_related = list %>% str_detect("data|analytics|computer|web|software"), data_exact = list %>% str_detect("data")) 

data_waffle <-
careerfair_2 %>% 
  group_by(data_related,data_exact) %>% 
  count()

state_waffle <-
careerfair_1 %>%
  group_by(State) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(State = fct_other(State, keep = c("ID","UT"))) %>% 
  group_by(State) %>% 
  summarise(n = sum(n))

data_state_waffle <-
  careerfair_2 %>% 
  mutate(State = fct_other(State, keep = c("ID","UT"))) %>%
  group_by(State, data_exact) %>% 
  summarise(n = n())
```

## Data Visualization


```r
# Use this R-Chunk to plot & visualize your data

ggplot(data = data_state_waffle) +
  geom_bar(aes(x = factor(1), y = n, fill = data_exact),stat = "identity", position = 'fill') +
  facet_wrap( ~ State) +
  coord_polar("y") +
  theme_void() +
  labs(fill = "Data vs Non-Data Job", x = "", y = "") +
  guides(fill = guide_legend(title.position = "top")) +
  theme(legend.position = "bottom") 
```

![](Career_Fair_Project_files/figure-html/plot_data-1.png)<!-- -->

```r
waffle(c("Non-data company" = data_waffle$n[1], "data-related company" = data_waffle$n[2], "Data company" = data_waffle$n[3]), row = 7, size = 2, keep = T) 
```

![](Career_Fair_Project_files/figure-html/plot_data-2.png)<!-- -->

```r
waffle(c("Idaho Company" = state_waffle$n[1], "Utah Company" = state_waffle$n[2], "Other state company" = state_waffle$n[3]), row = 7, size = 2, keep = T) 
```

![](Career_Fair_Project_files/figure-html/plot_data-3.png)<!-- -->


```r
ggplot() +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = careerfair_geom, aes(geometry = geometry)) +
  geom_text_repel(data = careerfair_geom, aes(geometry = geometry, label = ifelse(State %in% c("UT","ID"),NA, city)), stat = "sf_coordinates", size = 3) +
  theme_void() 
```

![](Career_Fair_Project_files/figure-html/plot_data2-1.png)<!-- -->

## Conclusions
