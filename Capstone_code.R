library("readxl")
library(dplyr)
library(ggplot2)
library(reshape2)
library(fmsb)
library(tidyr)

#import data
file_path <- "/Users/jenni/Desktop/Capstone/mmc2.xlsx"
d <- read_excel(file_path)

# explore data
summary(d)
tail(d)
colnames(d)

### Data cleaning
# removing timestamp
d <- d[, -1]
## reassign column names 
sns_names <- paste0("sns_q", 1:20)
demo_names <- paste0("demo_q", 1:12)
lone_names <- paste0("lone_q", 1:8)
phq_names <- paste0("phq_q", 1:9)
gad_names <- paste0("gad_q", 1:7)
sleep_names <- paste0("sleep_q", 1:19)

new_col_names <- c(sns_names, demo_names, lone_names, phq_names, gad_names, sleep_names)

colnames(d) <- new_col_names

# Check missing data
any_missing <- any(is.na(d))
missing_count <- colSums(is.na(d))
if (any_missing) {
  print("missing")
  print(missing_count)
} else {
  print("no missing")
} ##demo_q5 missing 2

rows_w_missing <- which(is.na(d$demo_q5))
print(d[rows_w_missing,])

#deal with missing data
d<- d[-rows_w_missing,]


##clean test values
# Edit data values for lone_names columns
lone_columns <- grep("^lone_q", colnames(d))
d[, lone_columns] <- lapply(d[, lone_columns], function(x) {
  ifelse(x == "Never", 0,
         ifelse(x == "Rarely", 1,
                ifelse(x == "Sometimes", 2,
                       ifelse(x == "Often", 3, x))))})
d[, lone_columns] <- lapply(d[, lone_columns], as.numeric)
d$lone_q8 #test

# Edit data values for phq_names columns
phq_columns <- grep("^phq_q", colnames(d))
d[, phq_columns] <- lapply(d[, phq_columns], function(x) {
  ifelse(x == "Not at all", 0,
         ifelse(x == "Several days", 1,
                ifelse(x == "Half days", 2,
                       ifelse(x == "Nearly everyday", 3, x))))})
d[, phq_columns] <- lapply(d[, phq_columns], as.numeric)
d$phq_q5 #test

# Edit data values for gad_names columns
gad_columns <- grep("^gad_q", colnames(d))
d[, gad_columns] <- lapply(d[, gad_columns], function(x) {
  ifelse(x == "Not at all", 0,
         ifelse(x == "Several days", 1,
                ifelse(x == "Half days", 2,
                       ifelse(x == "Nearly everyday", 3, x))))})
d[, gad_columns] <- lapply(d[, gad_columns], as.numeric)
d$gad_q2 #test

# Edit data values for sleep_names columns
sleep_columns <- grep("^sleep_q", colnames(d))
d[, sleep_columns] <- lapply(d[, sleep_columns], function(x) {
  ifelse(x == "Not during last month", 0,
         ifelse(x == "Less then once a week", 1,
                ifelse(x == "Once or twice a week", 2,
                       ifelse(x == "Three or more in week", 3,
                              ifelse(x == "Very good", 0,
                                     ifelse(x == "Farely good", 1,
                                            ifelse(x == "Farely bad", 2,
                                                   ifelse(x == "Very bad", 3, x))))))))})
sleep_columns <- paste0("sleep_q", 6:19)
d[, sleep_columns] <- lapply(d[, sleep_columns], as.numeric)

d$sleep_q18 #test #1-5 are non-numerical

##Calculate test values
d$lone_score <- rowSums(d[, grep("^lone_q", colnames(d))])
d$phq_score <- rowSums(d[, grep("^phq_q", colnames(d))])
d$gad_score <- rowSums(d[, grep("^gad_q", colnames(d))])

sleep_columns <- grep("^sleep_q", colnames(d))
sleep_indices <- sleep_columns[grep("sleep_q1[6-9]|sleep_q1[0-9]", colnames(d)[sleep_columns])]
d$sleep_score <- rowSums(d[, sleep_indices])

d$total_score <- rowSums(d[, c("lone_score", "phq_score", "gad_score", "sleep_score")])

###Visualization
##boxplot of  total_score vs. sns_q1
d$sns_q7 <- factor(d$sns_q7, levels = c("Less than 1 hour", "1-3 hours", "3-5 hours", "More than 5 hours"))

ggplot(d, aes(x = sns_q7, y = total_score, fill = sns_q7)) +
  geom_boxplot() +
  labs(x = "Daily time spent on social media", y = "Psychological distress indicators", title = "Correlation between Psychological Distress and Social Media Daily Usage ")

## Scatter plot of total_score vs. demo_q1
# Drop rows
d <- d[!(d$demo_q1 %in% c("01856420137", "2701710148674")), ]
# Update rows
d$demo_q1 <- as.numeric(d$demo_q1)
rows_to_change <- d$demo_q1 %in% c(1982, 1994, 1995, 1996)
d$demo_q1[rows_to_change] <- 2021 - d$demo_q1[rows_to_change]

ggplot(d, aes(x = demo_q1, y = total_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "Age", y = "Psychological distress indicators", title = "Correlation between Psychological Distress and Age")




