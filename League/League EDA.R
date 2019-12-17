library(tidyverse)
library(data.table)
library(R.utils)

# Sinc Working Directory with Data Stored Directory
getwd()
setwd(paste0(getwd(),"/../Downloads/GoldSummData2016.csv"))

# Compress the file for loading into Github
# save(play, file="play.RData", compress="xz")

# Data Preparation
# Download "play" data from below
# https://github.com/dongchanlim/R-data-analysis/blob/master/League/GoldSummData2016.csv/play.RData


# Import data
champ <- read_csv(file = "https://raw.githubusercontent.com/dongchanlim/R-data-analysis/master/League/GoldSummData2016.csv/ChampId2Name.csv")
champ$ChampId = as.character(champ$ChampId)
summoner_tier <- read_csv(file = "https://raw.githubusercontent.com/dongchanlim/R-data-analysis/master/League/GoldSummData2016.csv/SummIds2016.csv")


# Feature Engineering

count_position_play <- function(x) (ifelse(str_length(x) == 0, 0, str_split(x, pattern = " ", simplify = F) %>% unlist() %>% c(.) %>% length()))
most_played_champ_position <- function(x) (names(which.max(table(str_split(x, pattern = " ", simplify = F)))))
most_played_champ_count <- function(x) (ifelse(str_length(x) == 0, 0, max(table(str_split(x, pattern = " ", simplify = F)))))

vars <- c("position_play_count" = "1","most_position_champ" = '2',"most_champ_count" = "3")

most_played_champ <-
  play %>% 
  group_by(SummonerId) %>% 
  mutate_at(.vars = vars(Top:Adc), .funs = list(count_position_play, most_played_champ_position, most_played_champ_count)) %>% 
  ungroup() %>% 
  select(-c(Top:Adc)) %>% 
  mutate_at(.vars = vars(contains("_fn1")), .funs = function(x)(as.integer(x)))%>% 
  pivot_longer(
    cols = contains("fn"),
    names_to = c("position",".value"),
    names_pattern = "(.*)_fn(.)"
  ) %>% 
  rename(!!vars)

league_df <- most_played_champ %>% 
  left_join(champ, by = c("most_position_champ" = "ChampId")) %>% 
  select(-c(most_position_champ)) %>% 
  rename(most_position_champ = ChampName) %>% 
  mutate(most_champ_play_percent =  round(most_champ_count/position_play_count, 2) * 100)

# Visualization

league_df %>% 
  ggplot(aes(x = reorder(position, position_play_count) , y = position_play_count, label = position_play_count)) +
  geom_bar(aes(fill = position), stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = function(x)(format(paste0(x/1000,"k"), scientific = F))) +
  labs(title = "Which position is the most played for Gold tier?", x = "Position", y = "Count(1000)") +
  theme(legend.position = "none") +
  coord_flip()

league_play <- league_df %>% 
  group_by(SummonerId) %>% 
  summarise(total_play = sum(position_play_count)) 

five_summary <- quantile(league_play$total_play, probs = c(0, 0.25,0.5,0.75,1)) %>% as.data.frame() %>% rownames_to_column("percentile")

league_play %>% 
  ggplot(aes(x =  as.factor(1), y = total_play)) +
  geom_violin(fill = "orange") +
  theme_minimal() +
  geom_hline(yintercept = five_summary %>% as.list() %>% unlist(), linetype="dashed") +
  labs(x = "Game", y = "The number of play", title = "How many games takes for reaching to Gold Tier?") +
  theme(axis.text.y = element_blank()) +
  coord_flip()
  