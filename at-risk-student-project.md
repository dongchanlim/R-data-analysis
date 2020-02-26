---
title: "At-risk Student Project"
author: "Richard Lim"
date: "26 February, 2020"
output:
  html_document:
    theme: cerulean
    keep_md: true
    toc: true
    toc_width: 10
    toc_float: true
    code_folding: hide
---

# 1. Objective of analysis

- Objective: Support academic advising center by building prediction models for who to advise.

- $advsing\_new.csv$ & $student\_new.csv$ are separated but relational datasets which have joining keys.

- Implement Data Mining to figure out which predictor variables to forcast whether student are graduated or not

- Examine if intervention from advising center will help the sucess of students (Graduation)






# 2. EDA (Expolatory Data Analysis)

## a. Scales of data


```r
# ncol() | nrow()
cat("The student_new has", dim(student_new)[1], "rows and", dim(student_new)[2], "columns.\nThe advisng_new has",dim(advising_new)[1], "rows and", dim(advising_new)[2], "columns.")
```

```
## The student_new has 59390 rows and 41 columns.
## The advisng_new has 150002 rows and 12 columns.
```

There are 52 variables to inspect if it might be a good source to predict who is going to sucess on school or not.


## b. Type of variables 


```r
lapply(c(student_new, advising_new), typeof) %>% data.frame() %>%  t() %>%  as.data.frame() 
```

```
##                                            V1
## identifier                            integer
## mission_enter_date                  character
## mission_release_date                character
## pathway_id                            integer
## served_mission                        logical
## gender                              character
## high_school_gpa                        double
## act_composite                         integer
## act_english                           integer
## act_math                              integer
## act_reading                           integer
## act_science                           integer
## sum_of_cumulative_gpa                  double
## sum_of_cumulative_attempted_credits    double
## sum_of_cumulative_earned_credits       double
## transfer_earned_credits                double
## academic_status                     character
## classification_begin                character
## classification_end                  character
## cumulative_earned_credits              double
## cumulative_gpa                         double
## deferred_term                       character
## deferred_term_session_order           integer
## gpa                                    double
## resident_attempted_credits             double
## resident_earned_credits                double
## subprogram                          character
## enrolled_current_semester             logical
## registered_credits                     double
## cumulative_attempted_credits           double
## track_begin                         character
## session_order                         integer
## term_code                           character
## track_override                      character
## admissions_track                    character
## track                               character
## first_attended_term_code            character
## birth_date                          character
## major                               character
## enrollment_status                   character
## major_code                            integer
## Identifier                            integer
## Advising.Date                       character
## Total.Meeting.Duration                integer
## Advisor.Role                        character
## Visit.Type                          character
## Term.Code                           character
## Advising.Reason                     character
## Title                               character
## Session.Order                         integer
## Level                               character
## Classification.Begin                character
## Admissions.Track                    character
```



## c. How many missing values for each dataset?

There are observations which are represented as $NA$, but it is recorded as empty string instead of it. Therefore, it needs to be corrected and Identified as missing value.


```r
student_new <- sapply(student_new, function(x) {ifelse(is.character(x) & nchar(x) == 0, NA, x)}) %>% data.frame(stringsAsFactors = F) # Careful to not use as.data.frame()
advising_new <- sapply(advising_new, function(x) {ifelse(is.character(x) & nchar(x) == 0, NA, x)}) %>% data.frame(stringsAsFactors = F)
```


```r
missing_student <- plot_missing(student_new, missing_only = T, theme_config = list(legend.position = "right"), ggtheme = theme_light(), title = "student_new missing") %>% ggplotGrob()
```

```r
missing_advising <- plot_missing(advising_new, theme_config = list(legend.position = "right"), ggtheme = theme_light(), title = "advising_new missing") %>% ggplotGrob()
```


```r
grid.arrange(missing_student, missing_advising, ncol = 1)
```

![](at-risk-student-project_files/figure-html/missing_result-1.png)<!-- -->

- It appears that $pathway\_id$, $deferred\_term$, $deferred\_term\_session\_order$, $track\_override$ are the variables which is not suitable for prediction modeling.

- The missing plot per variable can be created with $naniar$ Package as well below

- There is $gg\_miss\_var$ plot which indicates how much proportion of missing values in $nanair$ pacakge below. 


```r
gmv1 <- naniar::gg_miss_var(student_new, show_pct = T)
gmv2 <- naniar::gg_miss_var(advising_new, show_pct = T)
grid.arrange(gmv1, gmv2, nrow = 1)
```

![](at-risk-student-project_files/figure-html/nanair_1-1.png)<!-- -->

- Also, it can be visualized with other way shown below.


```r
m1 <- naniar::vis_miss(student_new, warn_large_data = F) + labs(title = "Student_new missing")
m2 <- naniar::vis_miss(advising_new, warn_large_data = F) + labs(title = "Advsing_new missing")
grid.arrange(m1, m2, ncol = 1)
```

![](at-risk-student-project_files/figure-html/nanair_2-1.png)<!-- -->

### - Cleansing the column (Variable) names with Janitor Package

In order to access column names more conviniently, I used $Janitor$ Package to cleanse the name of columns to code in R-friendly way.


```r
# Clean the name of variables (lower.case & '_' connection)
student_new <- student_new %>% clean_names()
advising_new <- advising_new %>% clean_names()
```


### - Switching original continous features to numeric value

In the process of changing non-typed observations into NA, all variables' type has been changed into $character$. GPA, ACT Score, duration and Credits related variables must be treated as Numeric values. 


```r
student_new <- student_new %>% mutate_at(colnames(student_new) %>% grep("gpa|act|credits", ., value =T), as.numeric)
advising_new <- advising_new %>% mutate(total_meeting_duration = total_meeting_duration %>% as.numeric())

t1 <- student_new %>% select_if(is.numeric) %>% colnames() %>% data.frame() 
t2 <- advising_new %>% select_if(is.numeric) %>% colnames() %>% data.frame()
rbind(t1, t2) %>% mutate(type = "Numeric")
```

```
##                                      .    type
## 1                      high_school_gpa Numeric
## 2                        act_composite Numeric
## 3                          act_english Numeric
## 4                             act_math Numeric
## 5                          act_reading Numeric
## 6                          act_science Numeric
## 7                sum_of_cumulative_gpa Numeric
## 8  sum_of_cumulative_attempted_credits Numeric
## 9     sum_of_cumulative_earned_credits Numeric
## 10             transfer_earned_credits Numeric
## 11           cumulative_earned_credits Numeric
## 12                      cumulative_gpa Numeric
## 13                                 gpa Numeric
## 14          resident_attempted_credits Numeric
## 15             resident_earned_credits Numeric
## 16                  registered_credits Numeric
## 17        cumulative_attempted_credits Numeric
## 18              total_meeting_duration Numeric
```

### - DataExplorer Package 


```r
# DataExplor package - numeric variable & categorical variable & row & column
t1 <- DataExplorer::introduce(student_new) %>% t() %>% as.data.frame() %>% rownames_to_column("attribute")
t2 <- DataExplorer::introduce(advising_new) %>% t() %>% as.data.frame() %>% rownames_to_column("attribute")
p1 <- DataExplorer::plot_intro(student_new, title = "student data", ggtheme = theme_bw())
```

```r
p2 <- DataExplorer::plot_intro(advising_new, title = "advising data", ggtheme = theme_bw())
```


```r
grid.arrange(p1, tableGrob(t1, rows=NULL), tableGrob(t2, rows=NULL), p2,  ncol = 2, top = "Attributes of datasets")
```

![](at-risk-student-project_files/figure-html/DE_result-1.png)<!-- -->



# 3. Feature Exploration

## a. The number of student for each dataset


```r
# The number of student
cat("1. There are ",student_new %>% distinct(identifier) %>% nrow(), "students for student_new dataset.\n2. There are ",advising_new %>% distinct(identifier) %>% nrow(), "students for advising_new dataset.\n3. There are ", student_new %>% distinct(identifier) %>% nrow() - advising_new %>% distinct(identifier) %>% nrow(), "students who has not visited advising center for the periods")
```

```
## 1. There are  12711 students for student_new dataset.
## 2. There are  10268 students for advising_new dataset.
## 3. There are  2443 students who has not visited advising center for the periods
```

```r
# unique(student_new$identifier) %>% length()

df <- data.frame(type = c("advising","not advising"), number = c(10268,2443)) %>% mutate(ymax = cumsum(number), ymin = cumsum(number) - number, share = number/ sum(number))

df %>% ggplot(aes(x = 3.5, y = ymax)) +
  geom_rect(aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = type)) +
  geom_label(aes(label = paste0(round(share * 100),"%"))) +
  coord_polar(theta = "y") +
  xlim(1,4) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("orange","red")) +
  labs(caption = "")
```

![](at-risk-student-project_files/figure-html/num_student-1.png)<!-- -->

## b. The number of student per semester


```r
# Semester period 
student_semester<- student_new %>% distinct(identifier, .keep_all = T) %>% group_by(term_code) %>% tally() %>%  arrange(parse_number(term_code))
advising_semester <- advising_new %>% distinct(identifier, .keep_all = T) %>% group_by(term_code) %>% tally() %>% arrange(parse_number(term_code))

semester_table <- left_join(student_semester, advising_semester, by = "term_code", suffix = c("_student","_advising")) %>% gather(key = "type", value = "number", -c(term_code))

semester_table %>% ggplot(aes(x = term_code, y = number)) +
  geom_col(aes(fill = type), position = "dodge") +
  geom_label(aes(label = number, color = type, group = type), size = 4, position = position_dodge(width = 1)) +
  facet_wrap(parse_number(term_code) ~ ., scales = "free", ncol = 3) +
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "Count") +
  labs(caption = "")
```

![](at-risk-student-project_files/figure-html/semester-1.png)<!-- -->

- On yearly average, Winter Semester has more student than Spring & Fall semesters.
- The students are less likely to visit the adivising center for advising over time.

## c. First attended Semster distribution


```r
# First attended semster

first_attended <- student_new %>% group_by(first_attended_term_code) %>% tally() %>%  arrange(parse_number(first_attended_term_code))

first_attended %>% ggplot(aes(x = first_attended_term_code, y = n)) +
  geom_col(aes(fill = reorder(first_attended_term_code, -parse_number(first_attended_term_code)))) +
  geom_label(aes(label = paste0(n," -",round(n/sum(n),2) * 100, "%"))) +
  labs(title = "Attended Semester", x= "Semster", y = "Count") + 
  theme_light() +
  theme(legend.position = "none") +
  coord_flip()
```

![](at-risk-student-project_files/figure-html/first_attended-1.png)<!-- -->

- We can see that this is the record of students who attended BYU-I on 2013 to 2014 season. 

# 4. Feature Engineering

Since this analysis aims to know the effect of advising on the student's retention or withdrawal rate, I would like to combine two dataset with joining key to see the data at once. We will use $left\_join$ syntax to contains unmatched observations which stands for __the student who has not been advised by advising center__.

## - Student_new & Advising_new Joining key


```r
joining_key <- student_new[,which(colnames(student_new) %in% colnames(advising_new))] %>% colnames()

advising_student <- 
  left_join(advising_new, student_new, by = joining_key)

# datatable(advising_student %>% head(100), options = list(scrollX = T))
```

## - Enrollment Status (Target)

- Since the objective of this analysis is to predict if the student graduated or not, "Withdrawn" & "No show" are relabeled as "Not Graduated". 

__Before__

```r
advising_student$enrollment_status %>% unique()
```

```
## [1] "Graduated" "No-Show"   "Withdrawn" NA
```

__After__

```r
advising_student <- advising_student %>% mutate(enrollment_status = ifelse(str_detect(enrollment_status,"Graduated"), "Graduated", "Not Graduated"))

advising_student$enrollment_status %>% unique()
```

```
## [1] "Graduated"     "Not Graduated" NA
```

## - Get the freshman & sophomore record of each student

In terms of Data Science, Prediction is the process of estimating the consequences (Target) out of past data. The data might be biased if data has not been collected objectively or there could be confounding variables which can change the result dramatically. For predicting graduation of each student, The cohort analysis is required in terms of time series. I need the records of student within the first half to predict their last consequences. There are steps to configure the range of data below.   


1. Identify the classification of student by measuring the credits taken at BYU-I only


```r
advising_student <- advising_student %>% 
  mutate(sum_of_cumulative_campus_credit = as.numeric(sum_of_cumulative_earned_credits) - as.numeric(transfer_earned_credits))
```

Even if there is a $sum\_of\_cumulative_earned\_credits$ in the dataset, it conatains the transferred credits from other institution.
It needs to be modified by calculating the credits taken at BYU-I only.

2. divide the data set with two different level A(Assoicate)/B(Bachelor)


```r
associate_st_ad_f <- advising_student %>% 
  filter(level == "A" & sum_of_cumulative_campus_credit <= 30)

bachelor_st_ad_fs <- advising_student %>% 
  filter(level == "B" & sum_of_cumulative_campus_credit <= 60)
```

Depending on the __level__ *(Bachleor vs Associate)*, I assume that 30 credits and 60 credits are half of credits for graudation of each degree. In student manual, it is stated that 60 credits are required for getting associate, 120 credits are required for getting bachelor degree. 

3. bind two datasets to one dataset


```r
advising_student_fs <- rbind(associate_st_ad_f, bachelor_st_ad_fs)
```

4. Identify the proportion of target variable


```r
table(advising_student_fs$enrollment_status) %>% data.frame() %>% mutate(Pct = round(Freq / sum(Freq) * 100, 2))
```

```
##            Var1  Freq   Pct
## 1     Graduated  7419 21.36
## 2 Not Graduated 27308 78.64
```


5. Categorize the Freshman ~ Senior with campus earned credits


```r
advising_student_fs <- 
advising_student_fs %>% 
  mutate(classification = case_when(
    sum_of_cumulative_campus_credit <= 30 ~ "Fresh",
    sum_of_cumulative_campus_credit <= 60 ~ "Sopho",
    sum_of_cumulative_campus_credit <= 90 ~ "Junior",
    TRUE ~ "Senior"
  ))
```

## Sum of missing credits which were taken at BYU-I


```r
advising_student_fs <-
advising_student_fs %>% 
  mutate(sum_of_cumulative_missing_credit = as.numeric(sum_of_cumulative_attempted_credits) + as.numeric(transfer_earned_credits) - as.numeric(sum_of_cumulative_earned_credits))
```

# Mean of missing Credits by enrollment status


```r
t1 <- advising_student_fs %>% 
  group_by(enrollment_status) %>% 
  summarise(mean = mean(sum_of_cumulative_missing_credit, na.rm = T)) 

advising_student_fs %>% 
  ggplot(aes(x = enrollment_status, y = sum_of_cumulative_missing_credit)) +
  geom_jitter(alpha = 0.3, color = "gray") +
  geom_violin(aes(fill = enrollment_status)) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  annotation_custom(tableGrob(t1, rows = NULL), xmin= 0.5, xmax=  1 , ymin= 50 , ymax= 60)
```

![](at-risk-student-project_files/figure-html/missing_credits-1.png)<!-- -->

## - Mission_date 
### (Lubridate::%--% = as.interval())


```r
fix_year <- function(x, year=1919){ #since this year is 2019, if year digits are over 19, it is regared as 1900 century.
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

advising_student_fs <-
advising_student_fs %>% 
  mutate(mission_enter_date = mdy(mission_enter_date) %>% fix_year(), mission_release_date = mdy(mission_release_date) %>% fix_year()) %>% 
  mutate(served_mission = mission_enter_date %--% mission_release_date %>% as.duration()/ ddays(1)) %>% 
  mutate(served_mission = case_when(
    is.na(served_mission) ~ "N",
    served_mission < 100 ~ "H",
    TRUE ~ "F"
  ))
```

- There are three categories for $served\_mission$ 
  * N represents for those who have not served mission yet
  * H represents for those who served mission but not completed in full period
    (if missionary came back within 100 days, I identify them as H)
  * F represents for those who served mission in full time

## - Act Score

I will only use $act\_composite$ for analysis so that we can reduce the dimesions of variable.

## - Birth Date switching to age and age group


```r
advising_student_fs <- 
advising_student_fs %>% 
  mutate(age = round(mdy(birth_date) %>% fix_year() %--% ymd(paste0(20,parse_number(term_code)), truncated = 2L) %>% as.duration() / dyears(1))) %>% 
  mutate(age_group = case_when(
    age > 10 & age < 20 ~ "Teen",
    age < 40 ~  "Young Adult",
    age < 60 ~  "Mid Age",
    TRUE ~ "Aged"
  ))

t1 <- table(advising_student_fs$age_group) %>% data.frame() %>% tableGrob(rows=NULL)

advising_student_fs %>% ggplot(aes(x = age_group, y = age)) +
  geom_jitter(alpha = 0.5, color = "gray") +
  geom_violin(aes(fill = age_group), show.legend = FALSE) +
  annotation_custom(t1, ymin = 50, ymax = 80, xmin = 3.5, xmax =4.5) +
  scale_y_continuous(breaks = seq(10,80,10)) +
  theme_minimal()
```

![](at-risk-student-project_files/figure-html/age-1.png)<!-- -->

## - Subprogram (Dayschool VS Online)


```r
advising_student_fs <-
advising_student_fs %>% 
  mutate(subprogram = substring(subprogram,1,1))
```

- Students can be classified into in-school student (D) or online student. (O)

## - Check if student transferred from other institution


```r
advising_student_fs <-
advising_student_fs %>% 
  mutate(transfer = ifelse(transfer_earned_credits > 0, 1, 0) %>% as.factor())

advising_student_fs %>% group_by(transfer) %>% count()
```

```
## # A tibble: 2 x 2
## # Groups:   transfer [2]
##   transfer     n
##   <fct>    <int>
## 1 0        10884
## 2 1        23843
```


## - Top 5 Major by level(degree) 


```r
major_fs <- advising_student_fs %>% group_by(level, major) %>% tally(sort = T)

major_fs %>% group_by(level) %>% top_n(5) %>% ungroup() %>% 
  ggplot(aes(x = parse_number(major) %>% as.factor() %>% reorder(n), y = n)) +
  geom_col(aes(fill = level)) +
  geom_label(aes(label = str_extract(major, "[:alpha:]+"))) +
  facet_wrap(level ~., scales = "free", ncol = 1) +
  coord_flip() +
  theme_bw() +
  labs(x = "Major", y = "Count", title = "Top 5 Major by degree") +
  scale_fill_discrete(label = c("Assocaite", "Bachelor"))
```

```
## Selecting by n
```

![](at-risk-student-project_files/figure-html/major-1.png)<!-- -->

## - Top 5 Advising Reason by level(degree)


```r
advising_reason_fs <-
  advising_student_fs %>% group_by(level, advising_reason, enrollment_status) %>% count() %>% ungroup() %>% group_by(level, enrollment_status) %>% top_n(5, n)

advising_reason_fs %>% ggplot(aes(x = reorder(advising_reason, -n), y = n)) +
  geom_col(aes(fill = level), position = "dodge") +
  facet_wrap( level ~ enrollment_status, scales = "free") +
  theme_light() +
  coord_flip() +
  theme(legend.position = "top") +
  labs(x = "", y = "") +
  scale_fill_discrete(labels = c("Associate","Bachelor")) +
  labs(caption = "We can see that Graduated Students are more likely to visit advising center with the reason of Glad Planner & Class Planning compared to others")
```

![](at-risk-student-project_files/figure-html/advising_reason-1.png)<!-- -->


## - Aggregate the data group by student id (Identifier)

After feature engineering, I aggregated the data by distinct Identifier to manage the individual data of each student.


```r
advising_student_final <- 
advising_student_fs %>% 
  group_by(identifier) %>% 
  arrange(cumulative_earned_credits) %>% 
  summarise(total_meeting_duration = sum(total_meeting_duration),
            number_of_semester = length(enrolled_current_semester[unique(term_code)] %>% str_detect("T")) ,
            classification_begin = head(classification_begin,1),
            served_mission = head(served_mission,1),
            subprogram = head(subprogram,1),
            gender = head(gender, 1),
            act_composite = mean(act_composite),
            high_school_gpa = mean(high_school_gpa),
            level = head(level, 1),
            cumulative_gpa = tail(cumulative_gpa, 1),
            transfer = head(transfer, 1),
            age = round(mean(age)),
            cumulative_missing_credit = round(max(cumulative_attempted_credits) - (max(cumulative_earned_credits) - head(transfer_earned_credits,1)),2),
            cumulative_campus_earned_credit = max(cumulative_earned_credits) - head(transfer_earned_credits,1),
            transfer_earned_credits = mean(transfer_earned_credits, na.rm = T),
            # target variable
            enrollment_status = head(enrollment_status, 1)) %>% 
  ungroup()

# datatable(advising_student_final,)
```

# 5. Modeling


## 1. Target Distribution


```r
advising_student_final %>% group_by(enrollment_status) %>% count() %>% ungroup() %>%  mutate(pct = round(n/sum(n),2))
```

```
## # A tibble: 2 x 3
##   enrollment_status     n   pct
##   <chr>             <int> <dbl>
## 1 Graduated           511  0.12
## 2 Not Graduated      3611  0.88
```

- 88% of students in the data are composed of not graudated.
- 12% of students are gradudated

## 2. Identify the missing variables and impute the part (median)


```r
naniar::gg_miss_var(advising_student_final, show_pct = T)
```

![](at-risk-student-project_files/figure-html/missing_impute-1.png)<!-- -->

```r
naniar::gg_miss_fct(advising_student_final, enrollment_status)
```

![](at-risk-student-project_files/figure-html/missing_impute-2.png)<!-- -->

```r
advising_student_final %>% group_by(enrollment_status) %>% summarise(act_median = median(act_composite, na.rm = T), hs_gpa_median = median(high_school_gpa, na.rm = T))
```

```
## # A tibble: 2 x 3
##   enrollment_status act_median hs_gpa_median
##   <chr>                  <dbl>         <dbl>
## 1 Graduated                 23          3.69
## 2 Not Graduated             22          3.23
```

```r
advising_student_final[advising_student_final$enrollment_status == "Graduated" & is.na(advising_student_final$act_composite),"act_composite"] <- 23
advising_student_final[advising_student_final$enrollment_status == "Not Graduated" & is.na(advising_student_final$act_composite),"act_composite"] <- 22
advising_student_final[advising_student_final$enrollment_status == "Graduated" & is.na(advising_student_final$high_school_gpa),"high_school_gpa"] <- 3.69
advising_student_final[advising_student_final$enrollment_status == "Not Graduated" & is.na(advising_student_final$high_school_gpa),"high_school_gpa"] <- 3.23


advising_student_final <- advising_student_final %>% mutate_if(is.character, as.factor) %>% mutate_if(is.logical, as.factor) %>% select(-c(identifier))

sapply(advising_student_final, class)
```

```
##          total_meeting_duration              number_of_semester 
##                       "numeric"                       "integer" 
##            classification_begin                  served_mission 
##                        "factor"                        "factor" 
##                      subprogram                          gender 
##                        "factor"                        "factor" 
##                   act_composite                 high_school_gpa 
##                       "numeric"                       "numeric" 
##                           level                  cumulative_gpa 
##                        "factor"                       "numeric" 
##                        transfer                             age 
##                        "factor"                       "numeric" 
##       cumulative_missing_credit cumulative_campus_earned_credit 
##                       "numeric"                       "numeric" 
##         transfer_earned_credits               enrollment_status 
##                       "numeric"                        "factor"
```

```r
naniar::gg_miss_var(advising_student_final, show_pct = T)
```

![](at-risk-student-project_files/figure-html/missing_impute-3.png)<!-- -->


```r
set.seed(1000)
InTraining <- caret::createDataPartition(advising_student_final$enrollment_status,p = .3, list = F)
Train <- advising_student_final %>% slice(InTraining)
Test <- advising_student_final %>% slice(-InTraining)

ModelFit_1 <- train(enrollment_status ~ ., data = Train, method = "rf", importance = T)
ModelFit_2 <- train(enrollment_status ~ ., data = Train, method = "rpart")
```


```r
varimp1 <- ggplot(varImp(ModelFit_1)) + geom_label(aes(label = paste0(round(Importance),"%"))) + theme_bw() + labs(title = "Random Forest") + theme(plot.title = element_text(hjust = 0.5))
varimp2 <- ggplot(varImp(ModelFit_1)) + geom_label(aes(label = paste0(round(Importance),"%"))) + theme_bw() + labs(title = "RPart") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(varimp1, varimp2, ncol = 1)
```

![](at-risk-student-project_files/figure-html/modeling_2-1.png)<!-- -->


```r
caret::confusionMatrix(Test$enrollment_status ,predict(ModelFit_1, Test))
```

```
## Confusion Matrix and Statistics
## 
##                Reference
## Prediction      Graduated Not Graduated
##   Graduated           275            82
##   Not Graduated        15          2512
##                                           
##                Accuracy : 0.9664          
##                  95% CI : (0.9591, 0.9726)
##     No Information Rate : 0.8994          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.8314          
##                                           
##  Mcnemar's Test P-Value : 2.066e-11       
##                                           
##             Sensitivity : 0.94828         
##             Specificity : 0.96839         
##          Pos Pred Value : 0.77031         
##          Neg Pred Value : 0.99406         
##              Prevalence : 0.10055         
##          Detection Rate : 0.09535         
##    Detection Prevalence : 0.12379         
##       Balanced Accuracy : 0.95833         
##                                           
##        'Positive' Class : Graduated       
## 
```

```r
caret::confusionMatrix(Test$enrollment_status ,predict(ModelFit_2, Test))
```

```
## Confusion Matrix and Statistics
## 
##                Reference
## Prediction      Graduated Not Graduated
##   Graduated           267            90
##   Not Graduated        37          2490
##                                           
##                Accuracy : 0.956           
##                  95% CI : (0.9478, 0.9632)
##     No Information Rate : 0.8946          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7832          
##                                           
##  Mcnemar's Test P-Value : 3.945e-06       
##                                           
##             Sensitivity : 0.87829         
##             Specificity : 0.96512         
##          Pos Pred Value : 0.74790         
##          Neg Pred Value : 0.98536         
##              Prevalence : 0.10541         
##          Detection Rate : 0.09258         
##    Detection Prevalence : 0.12379         
##       Balanced Accuracy : 0.92170         
##                                           
##        'Positive' Class : Graduated       
## 
```



```r
fancyRpartPlot(ModelFit_2$finalModel)
```

![](at-risk-student-project_files/figure-html/modeling_4-1.png)<!-- -->
