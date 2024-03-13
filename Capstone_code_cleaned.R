library("readxl")
library(dplyr)
library(ggplot2)
library(reshape2)
library(fmsb)
library(tidyr)
library(car)

#import data
file_path <- "/Users/jenni/Desktop/Capstone/mmc2.xlsx"
d <- read_excel(file_path)

# explore data
summary(d)
tail(d)
colnames(d)

############################## Data cleaning ################################
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

##Fix age
# Drop rows
d <- d[!(d$demo_q1 %in% c("01856420137", "2701710148674")), ]
# Update rows
d$demo_q1 <- as.numeric(d$demo_q1)
rows_to_change <- d$demo_q1 %in% c(1982, 1994, 1995, 1996)
d$demo_q1[rows_to_change] <- 2021 - d$demo_q1[rows_to_change]

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
                ifelse(x == "Half of days", 2,
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

##Extra cleaning
#Convert for demo_q4
column_name <- "demo_q4"
d[[column_name]] <- ifelse(d[[column_name]] == "christan", "Christian",
                            ifelse(d[[column_name]] == "Christan", "Christian",
                                   ifelse(d[[column_name]] == "buddho", "Buddho", 
                                   		ifelse(d[[column_name]] == "Christianity", "Christian",
                                   		d[[column_name]]))))
d$demo_q4 #test
####################### Visualization 1 #############################
# Scatter plot for total_score vs. sns_q7
d$sns_q7 <- factor(d$sns_q7, levels = c("Less than 1 hour", "1-3 hours", "3-5 hours", "More than 5 hours"))

# Plot box plot for total_score vs. sns_q7
ggplot(d, aes(x = sns_q7, y = total_score)) +
  geom_boxplot(fill = "dodgerblue4", color = "black", alpha = 0.7) +
  labs(x = "Daily time spent on social media", y = "Psychological distress indicators", title = "Correlation between Overall Psychological Distress and Social Media Daily Usage ")
  
# Define colors for each level of sns_q7
#color_palette <- c("Less than 1 hour" = "#2ca02c", 
#                   "1-3 hours" = "#FFD700", 
#                   "3-5 hours" = "orange", 
#                   "More than 5 hours" = "red")

#ggplot(d, aes(x = sns_q7, y = total_score, color = sns_q7)) +
# geom_jitter(alpha = 0.7, width = 0.3) +
#  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "#FF0000", size = 0.7) +  
#  scale_color_manual(values = color_palette) + 
 # labs(x = "Daily time spent on social media", y = "Psychological distress indicators", title = "Correlation between Overall Psychological Distress and Social Media Daily Usage ")

ggplot(d, aes(x = sns_q7, y = total_score)) +
  geom_jitter(alpha = 0.7, width = 0.3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "#FF0000", size = 0.7) +  
  scale_color_manual(values = color_palette) + 
  labs(x = "Daily time spent on social media", y = "Psychological distress indicators", title = "Correlation between Overall Psychological Distress and Social Media Daily Usage ") 
####################### Visualization 2 ############################# 
# Interaction Plot for gender
ggplot(d, aes(x = sns_q7, y = total_score, color = demo_q2)) +
  geom_line(stat = "summary", fun = "mean", aes(group = demo_q2)) +
  labs(x = "Daily time spent on social media", y = "Mean Total Score",
       color = "Gender") +
  ggtitle("Interaction Plot: Overall Psychological Distress by Social Media Daily Usage and Gender") +
  theme_minimal()
  
# Plot scatterplot
ggplot(data = d, aes(x = demo_q10, y = total_score, color = factor(sns_q7))) +
  geom_point() +
  labs(x = "Covariate (demo_q10)", y = "Outcome (total_score)", color = "SNS Usage (sns_q7)") +
  ggtitle("Scatterplot of total_score vs demo_q10 with SNS Usage") +
  theme_minimal()

ggplot(data = d, aes(x = demo_q10, y = total_score, color = factor(sns_q7))) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Covariate (demo_q10)", y = "Outcome (total_score)", color = "SNS Usage (sns_q7)") +
  ggtitle("Smoothed Conditional Means of total_score by demo_q10 and SNS Usage") +
  theme_minimal()
  
# Interaction Plot for smoking habit
ggplot(d, aes(x = sns_q7, y = total_score, color = demo_q12)) +
  geom_line(stat = "summary", fun = "mean", aes(group = demo_q12)) +
  labs(x = "Daily time spent on social media", y = "Mean Total Score",
       color = "Smoking Status") +
  ggtitle("Interaction Plot: Overall Psychological Distress by Social Media Daily Usage and Smoking Status") +
  theme_minimal()
####################### Quantify SNS Usage #############################
##Quantify the independent variable

# SNS Opinions (14-20) Quantifiable: 14-19
# Convert "Yes" to 0 and "No" to 2
sns_columns <- c("sns_q14", "sns_q15", "sns_q16", "sns_q19")
d[, sns_columns] <- lapply(d[, sns_columns], function(x) {
  ifelse(x == "Yes", 0,
         ifelse(x == "No", 2, x))
})
d[, sns_columns] <- lapply(d[, sns_columns], as.numeric)
d$sns_q19 #test

#Convert for sns_q17
column_name <- "sns_q17"
d[[column_name]] <- ifelse(d[[column_name]] == "Always", 2,
                            ifelse(d[[column_name]] == "Sometimes", 1,
                                   ifelse(d[[column_name]] == "Not at all", 0, d[[column_name]])))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q17 #test

#Convert for sns_q18
column_name <- "sns_q18"
d[[column_name]] <- ifelse(d[[column_name]] == "All the times", 2,
                            ifelse(d[[column_name]] == "Most of the times", 1,
                                   ifelse(d[[column_name]] == "Never", 0, d[[column_name]])))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q18 #test

# Calculate SNS Opinions (new col)
d$sns_opp <- rowSums(d[, c("sns_q14", "sns_q15", "sns_q17", "sns_q18")])

# SNS Facts/Usage (1-13) Quantifiable: 5,6,7,9
# Q11 in the dataset has invalid data, entry error?
#Convert for sns_q5
column_name <- "sns_q5"
d[[column_name]] <- ifelse(d[[column_name]] == "More than 10 years", 4,
                            ifelse(d[[column_name]] == "5-10 years", 3,
                                   ifelse(d[[column_name]] == "2-5 years", 2, 
                                   		ifelse(d[[column_name]] == "Less than 2-year",1,
                                   		d[[column_name]]))))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q5 #test

#Convert for sns_q6
column_name <- "sns_q6"
d[[column_name]] <- ifelse(d[[column_name]] == "More than 5 per day", 4,
                            ifelse(d[[column_name]] == "3-5 per day", 3,
                                   ifelse(d[[column_name]] == "1-2 per day", 2, 
                                   		ifelse(d[[column_name]] == "Less than 1 per day",1,
                                   		d[[column_name]]))))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q6 #test

#Convert for sns_q7
column_name <- "sns_q7"
d[[column_name]] <- ifelse(d[[column_name]] == "More than 5 hours", 4,
                            ifelse(d[[column_name]] == "3-5 hours", 3,
                                   ifelse(d[[column_name]] == "1-3 hours", 2, 
                                   		ifelse(d[[column_name]] == "Less than 1 hour",1,
                                   		d[[column_name]]))))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q7 #test

#Convert for sns_q9
column_name <- "sns_q9"
d[[column_name]] <- ifelse(d[[column_name]] == "More than 4000", 4,
                            ifelse(d[[column_name]] == "2000-4000", 3,
                                   ifelse(d[[column_name]] == "500-2000", 2, 
                                   		ifelse(d[[column_name]] == "Less than 500",1,
                                   		d[[column_name]]))))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q9 #test

#Convert for sns_q10
column_name <- "sns_q10"
d[[column_name]] <- ifelse(d[[column_name]] == "Few of them", 4,
                            ifelse(d[[column_name]] == "Many of them", 3,
                                   ifelse(d[[column_name]] == "Most of them", 2, 
                                   		ifelse(d[[column_name]] == "All of them",1,
                                   		d[[column_name]]))))
d[[column_name]] <- as.numeric(d[[column_name]])
d$sns_q10 #test

# Calculate SNS Facts/Usage (new col)
d$sns_use <- rowSums(d[, c("sns_q5", "sns_q6", "sns_q7", "sns_q9")])
###################### Visualization 3 ################################
# Select relevant columns
heatmap_data <- d[, c("lone_score", "phq_score", "gad_score", "sleep_score", "sns_opp", "sns_use")]

# Create an empty data frame to store correlations
correlation_data <- data.frame(variable = character(), sns = character(), correlation = numeric(), stringsAsFactors = FALSE)

# Calculate correlations for sns_opp and sns_use
for (var in c("lone_score", "phq_score", "gad_score", "sleep_score")) {
  # Correlation with sns_opp
  cor_opp <- cor(heatmap_data[[var]], heatmap_data$sns_opp)
  correlation_data <- rbind(correlation_data, data.frame(variable = var, sns = "sns_opp", correlation = cor_opp))
  
  # Correlation with sns_use
  cor_use <- cor(heatmap_data[[var]], heatmap_data$sns_use)
  correlation_data <- rbind(correlation_data, data.frame(variable = var, sns = "sns_use", correlation = cor_use))
}

# Plot heatmap
ggplot(correlation_data, aes(x = variable, y = sns, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(high = "dodgerblue4", low = "white", midpoint = 0,
                       limits = range(correlation_data$correlation),
                       name = "Correlation") +
  labs(x = "Outcome Variables", y = "SNS Usage",
       title = "Correlation Heatmap between Outcome Variables and SNS Usage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Select relevant columns
heatmap_data <- d[, c("lone_score", "phq_score", "gad_score", "sleep_score", "sns_q5", "sns_q6", "sns_q7", "sns_q9", "sns_q10","sns_q14", "sns_q15", "sns_q16", "sns_q17", "sns_q18", "sns_q19")]

# Create an empty data frame to store correlations
correlation_data <- data.frame(variable = character(), sns = character(), correlation = numeric(), stringsAsFactors = FALSE)

# Calculate correlations for each combination of variables
for (var in c("lone_score", "phq_score", "gad_score", "sleep_score")) {
  for (sns_var in c("sns_q5", "sns_q6", "sns_q7", "sns_q9", "sns_q10","sns_q14", "sns_q15", "sns_q16", "sns_q17", "sns_q18", "sns_q19")) {
    cor_val <- cor(heatmap_data[[var]], heatmap_data[[sns_var]], use = "complete.obs")  # Use complete observations
    correlation_data <- rbind(correlation_data, data.frame(variable = var, sns = sns_var, correlation = cor_val))
  }
}

# Convert sns to factor with reversed order
correlation_data$sns <- factor(correlation_data$sns, levels = rev(c("sns_q5", "sns_q6", "sns_q7", "sns_q9", "sns_q10", "sns_q14", "sns_q15", "sns_q16", "sns_q17", "sns_q18", "sns_q19")))

# Plot heatmap
ggplot(correlation_data, aes(x = variable, y = sns, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", mid="orange",low = "white", midpoint = 0,
                       limits = range(correlation_data$correlation, na.rm = TRUE),  # Exclude NA values
                       name = "Correlation") +
  labs(x = "Subsections of Psychological Distress Indicator", y = "SNS Quantifiable Variables",
       title = "Correlation Heatmap between Four Mental Aseessment Scores and SNS Variables") +
         scale_x_discrete(labels = c("lone_score" = "Loneliness indicator", 
                              "phq_score" = "Anxiety indicator", 
                              "gad_score" = "Depression indicator", 
                              "sleep_score" = "Sleep quality indicator")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


######################### LOESS Regression #######################
# Nonparametric regression between sns_opp and total_score using LOESS
loess_opp <- loess(total_score ~ sns_opp, data = d)

# Scatter plot and LOESS curve for sns_opp and total_score
scatterplot_sns_opp_loess <- ggplot(d, aes(x = sns_opp, y = total_score)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "#FF0000") +
  labs(x = "Social media impressionableness indicator", y = "Psychological distress indicators", title = "Correlation between Overall Psychological Distress and Social Media Impressionableness")

# Summary of nonparametric regression models
# summary(loess_use)
summary(loess_opp)

# Predicted values and residuals
predicted <- predict(loess_opp)
residuals <- resid(loess_opp)

# Residuals vs. Predicted plot
residuals_vs_predicted <- ggplot(data.frame(predicted = predicted, residuals = residuals), aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted values", y = "Residuals", title = "Residuals vs. Predicted values")

# Histogram of residuals
histogram_residuals <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals")


loess_total <- loess(total_score ~ sns_total, data = d)

# Scatter plot and LOESS curve for sns_total and total_score
scatterplot_sns_total_loess <- ggplot(d, aes(x = sns_total, y = total_score)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "#FF0000") +
  labs(x = "Social media behavior", y = "Psychological distress indicators", title = "Correlation between Overall Psychological Distress and Social Media behavior")
########################### Linear Regression ############################
# Linear regression between sns_q7 and total_score
lm_use_new <- lm(total_score ~ sns_q7, data = d)

# Scatter plot and trend line for sns_use and total_score
scatterplot_sns_use_new <- ggplot(d, aes(x = sns_q7, y = total_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "sns_q7", y = "total_score", title = "Linear Regression: sns_q7 vs. total_score")

## Regression Assumptions ####
# Residual plot
plot(resid(lm_use_new)~fitted(lm_use_new), main = "Residual Plot for SNS use vs total score",
     ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0)

# Q-Q plot
qqnorm(resid(lm_use_new))
qqline(resid(lm_use_new), col = "red")

# Summary of linear regression models
summary(lm_use_new)

# Linear regression between sns_opp and total_score
lm_opp <- lm(total_score ~ sns_opp, data = d)

# Scatter plot and trend line for sns_opp and total_score
scatterplot_sns_opp <- ggplot(d, aes(x = sns_opp, y = total_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "sns_opp", y = "total_score", title = "Linear Regression: sns_opp vs. total_score")

# Summary of linear regression models
summary(lm_opp)

d$sns_total <- rowSums(d[, c("sns_q7", "sns_opp")])
lm_snstotal <- lm(total_score ~ sns_total, data = d)
summary(lm_snstotal)

# Scatter plot and trend line for sns_opp and total_score
scatterplot_sns_total <- ggplot(d, aes(x = sns_total, y = total_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "sns_total", y = "total_score", title = "Linear Regression: sns_total vs. total_score")
########################## Multiple Linear Regression ##################
# Define variables
DV <- c("lone_score", "phq_score", "gad_score", "sleep_score") #total_score
IV <- c("sns_use", "sns_opp")

# Convert categorical variables to factors
d$sns_q5 <- as.factor(d$sns_q5)
d$sns_q7 <- as.factor(d$sns_q7)
d$sns_q14 <- as.factor(d$sns_q14)
d$sns_q16 <- as.factor(d$sns_q16)
d$sns_q17 <- as.factor(d$sns_q17)
d$sns_q18 <- as.factor(d$sns_q18)
d$sns_q19 <- as.factor(d$sns_q19)

# Run multiple linear regression to pick predictor
mlm <- lm(cbind(total_score) ~ demo_q1 + demo_q2 + demo_q3 + demo_q4 + demo_q5 + demo_q6 +demo_q7 + demo_q8 + demo_q9 +demo_q10 + demo_q11 + demo_q12, data = d)
summary(mlm)

# overall MLM -- best
mlm2 <- lm(cbind(total_score) ~ sns_q5 + sns_q7 + sns_q16 + sns_q14 + sns_q17 + sns_q18 + sns_q19 + demo_q2 + demo_q10 + demo_q12, data = d)
summary(mlm2)

# Extract residuals and predicted values from the multiple linear regression model
residuals <- resid(mlm_demo)
predicted <- fitted(mlm_demo)

# Plot residuals vs. predicted values
plot(predicted, residuals, xlab = "Predicted values", ylab = "Residuals", main = "Residuals vs. Predicted values")

# Add a line with slope 1 to visualize linear relationship
abline(a = 0, b = 1, col = "red")

## Regression Assumptions ####
# Residual plot
plot(resid(mlm2)~fitted(mlm2), main = "Residual Plot for Best MLM",
     ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0)

# Q-Q plot
qqnorm(resid(mlm2))
qqline(resid(mlm2), col = "red")

# Check for multicollinearity using VIF
vif_values <- vif(mlm2)
vif_values

