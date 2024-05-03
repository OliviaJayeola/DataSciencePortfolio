library(tidyverse)
library(plotrix)
library(dplyr)
library(lubridate)
library(vcd)
library(exact2x2)
library(BayesFactor)
library(brms)
library(fitdistrplus)


data <- read.csv("../ab_data.csv")

data <- data %>%
  mutate(action = if_else(click_yes != 0 | click_no != 0, 1, 0)) %>%
  filter(if_all(c(days_since_account_creation, num_downloads, num_searches), ~ . >= 0)) %>%
  mutate(subscription = ifelse(subscription == "paid", 1, 0)) %>%
  mutate(group = ifelse(group == "popup_1", 1, 2))

str(data)
summary(data)


# overall counts ----------------------------------------------------------

# overall dataframe
overall  <- data %>%
  group_by(group) %>%
  summarise(actions = sum(action), no_actions = sum(!action))


# chi squared  ----------------------------------------------------------

observed_overall <- matrix(overall$actions, nrow = 2, byrow = TRUE)

# Add the no_actions column to the contingency table
observed_overall <- cbind(observed_overall, overall$no_actions)

# Add row and column names
colnames(observed_overall) <- c("actions", "no_actions")
rownames(observed_overall) <- c("1", "2")

# Print the contingency table
print(observed_overall)

chisq_result <- chisq.test(observed_overall)

test_statistic <- chisq_result$statistic
p_value <- chisq_result$p.value

cat("Chi-Squared Test Results:\n")
cat("Test Statistic:", test_statistic, "\n")
cat("p-value:", p_value, "\n")

# Determine the significance level (e.g., 0.05)
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("The test result is statistically significant.\n")
  cat("There is evidence to suggest an association between group and click response.\n")
} else {
  cat("The test result is not statistically significant.\n")
  cat("There is no strong evidence to suggest an association between group and click response.\n")
}

# Create a contingency table from the observed counts
cont_table <- chisq_result$observed

# Calculate Cramer's V
effect_size <- assocstats(cont_table)$cramer
effect_size

group1_total <- (observed_overall[1,1] + observed_overall[1,2])
group2_total <- (observed_overall[2,1] + observed_overall[2,2])

ci_action_group1 <- binom.test(x = observed_overall[1, 1], n = group1_total, conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_overall[1, 2], n = group1_total, conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_action_group2 <- binom.test(x = observed_overall[2, 1], n = group2_total, conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_overall[2, 2], n = group2_total, conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Action - Group 1: ", ci_action_group1[1], " - ", ci_action_group1[2], "\n")
cat("Action - Group 2: ", ci_action_group2[1], " - ", ci_action_group2[2], "\n")
cat("No Action - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("No Action - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


# ------ country

response_by_country <- data %>%
  dplyr::select(user_id, group, country, date, action)

clean_data_country <- response_by_country %>%
  filter(!is.na(country) & nzchar(country))

print(unique(clean_data_country$country))

user_counts_by_country <- clean_data_country %>%
  group_by(group,country) %>%
  summarise(actions = sum(action),
            no_actions = sum(!action),
            user_count = n_distinct(user_id),
            conversion_rate_action = (no_actions / user_count),
            conversion_rate_no = (actions / user_count))

selected_countries <- c("Wales")

users_selected_england <- clean_data_country %>%
  filter(country %in% selected_countries)

# --- chi sq countries

overall  <- users_selected_england %>%
  group_by(group) %>%
  summarise(actions = sum(action), no_actions = sum(!action))

observed_overall <- matrix(overall$actions, nrow = 2, byrow = TRUE)

# Add the no_actions column to the contingency table
observed_overall <- cbind(observed_overall, overall$no_actions)

# Add row and column names
colnames(observed_overall) <- c("actions", "no_actions")
rownames(observed_overall) <- c("1", "2")

# Print the contingency table
print(observed_overall)

chisq_result <- chisq.test(observed_overall)

test_statistic <- chisq_result$statistic
p_value <- chisq_result$p.value

cat("Chi-Squared Test Results:\n")
cat("Test Statistic:", test_statistic, "\n")
cat("p-value:", p_value, "\n")

# Determine the significance level (e.g., 0.05)
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("The test result is statistically significant.\n")
  cat("There is evidence to suggest an association between group and click response.\n")
} else {
  cat("The test result is not statistically significant.\n")
  cat("There is no strong evidence to suggest an association between group and click response.\n")
}

# Create a contingency table from the observed counts
cont_table <- chisq_result$observed

# Calculate Cramer's V
effect_size <- assocstats(cont_table)$cramer
effect_size

group1_total <- (observed_overall[1,1] + observed_overall[1,2])
group2_total <- (observed_overall[2,1] + observed_overall[2,2])

ci_action_group1 <- binom.test(x = observed_overall[1, 1], n = group1_total, conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_overall[1, 2], n = group1_total, conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_action_group2 <- binom.test(x = observed_overall[2, 1], n = group2_total, conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_overall[2, 2], n = group2_total, conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Action - Group 1: ", ci_action_group1[1], " - ", ci_action_group1[2], "\n")
cat("Action - Group 2: ", ci_action_group2[1], " - ", ci_action_group2[2], "\n")
cat("No Action - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("No Action - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


# response by career ------------------------------------------------------

response_by_career <- data %>%
  dplyr::select(user_id,group, career,date, action)

clean_data_career <- response_by_career %>%
  filter(!is.na(career) & nzchar(career))

print(unique(clean_data_career$career))


user_counts_by_career <- clean_data_career %>%
  group_by(group,career) %>%
  summarise(actions = sum(action),
            no_actions = sum(!action),
            user_count = n_distinct(user_id),
            conversion_rate_action = (no_actions / user_count),
            conversion_rate_no = (actions / user_count))

selected_career <- c("KS3")

users_selected_KS1 <- clean_data_career %>%
  filter(career %in% selected_career)


# chi squared  ----------------------------------------------------------

overall  <- users_selected_KS1 %>%
  group_by(group) %>%
  summarise(actions = sum(action), no_actions = sum(!action))

observed_overall <- matrix(overall$actions, nrow = 2, byrow = TRUE)

# Add the no_actions column to the contingency table
observed_overall <- cbind(observed_overall, overall$no_actions)

# Add row and column names
colnames(observed_overall) <- c("actions", "no_actions")
rownames(observed_overall) <- c("1", "2")

# Print the contingency table
print(observed_overall)

chisq_result <- chisq.test(observed_overall)

test_statistic <- chisq_result$statistic
p_value <- chisq_result$p.value

cat("Chi-Squared Test Results:\n")
cat("Test Statistic:", test_statistic, "\n")
cat("p-value:", p_value, "\n")

# Determine the significance level (e.g., 0.05)
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("The test result is statistically significant.\n")
  cat("There is evidence to suggest an association between group and click response.\n")
} else {
  cat("The test result is not statistically significant.\n")
  cat("There is no strong evidence to suggest an association between group and click response.\n")
}

# Create a contingency table from the observed counts
cont_table <- chisq_result$observed

# Calculate Cramer's V
effect_size <- assocstats(cont_table)$cramer
effect_size

group1_total <- (observed_overall[1,1] + observed_overall[1,2])
group2_total <- (observed_overall[2,1] + observed_overall[2,2])

ci_action_group1 <- binom.test(x = observed_overall[1, 1], n = group1_total, conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_overall[1, 2], n = group1_total, conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_action_group2 <- binom.test(x = observed_overall[2, 1], n = group2_total, conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_overall[2, 2], n = group2_total, conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Action - Group 1: ", ci_action_group1[1], " - ", ci_action_group1[2], "\n")
cat("Action - Group 2: ", ci_action_group2[1], " - ", ci_action_group2[2], "\n")
cat("No Action - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("No Action - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


# searches -------


searches <- data %>%
  dplyr::select(user_id,action,num_searches,date) %>%
  mutate(searches_category = case_when(
    num_searches == 0 ~ 0,
    num_searches <= 50 ~ 50,
    num_searches <= 250 ~ 250,
    num_searches <= 500 ~ 500,
    num_searches <= 750 ~ 750,
    num_searches <= 1000 ~ 1000,
    num_searches <= 1250 ~ 1250,
    num_searches <= 1500 ~ 1500,
    num_searches <= 1750 ~ 1750,
    num_searches <= 2000 ~ 2000,
    num_searches <= 2250 ~ 2250,
    num_searches <= 2500 ~ 2500,
    num_searches <= 2750 ~ 2750,
    num_searches > 3000 ~ 3000)) 


user_counts_by_searches <- searches %>%
  group_by(searches_category)  %>%
  summarise(actions = sum(action),
            no_actions = sum(!action),
            conversion_rate_action = (no_actions / user_count),
            conversion_rate_no = (actions / user_count))

#group_by(searches_category) %>%
#summarise(user_count = n_distinct(user_id))

group50 <- subset(searches, searches_category == 50)
shuffled_50group <- group50[sample(nrow(group50)), ]

# Take a random sample of rows from the group 50 to make samples balanced
sample_size <- 2500
sample <- shuffled_50group[1:sample_size, ]

searches_sampled <- searches[searches$searches_category != 50, ]  
searches_sampled <- rbind(searches_sampled, sample) 

searches <- searches_sampled  %>%
  filter(searches_category == 0 | searches_category == 50 )

searches50 <- user_counts_by_searches %>%
  filter(searches_category == 50)

# chi squared  ----------------------------------------------------------

overall  <- searches %>%
  group_by(searches_category) %>%
  summarise(actions = sum(action), no_actions = sum(!action))
  

observed_overall <- matrix(overall$actions, nrow = 2, byrow = TRUE)

# Add the no_actions column to the contingency table
observed_overall <- cbind(observed_overall, overall$no_actions)

# Add row and column names
colnames(observed_overall) <- c("actions", "no_actions")
rownames(observed_overall) <- c("1", "2")

# Print the contingency table
print(observed_overall)

chisq_result <- chisq.test(observed_overall)

test_statistic <- chisq_result$statistic
p_value <- chisq_result$p.value

cat("Chi-Squared Test Results:\n")
cat("Test Statistic:", test_statistic, "\n")
cat("p-value:", p_value, "\n")

# Determine the significance level (e.g., 0.05)
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("The test result is statistically significant.\n")
  cat("There is evidence to suggest an association between group and click response.\n")
} else {
  cat("The test result is not statistically significant.\n")
  cat("There is no strong evidence to suggest an association between group and click response.\n")
}

# Create a contingency table from the observed counts
cont_table <- chisq_result$observed

# Calculate Cramer's V
effect_size <- assocstats(cont_table)$cramer
effect_size

group1_total <- (observed_overall[1,1] + observed_overall[1,2])
group2_total <- (observed_overall[2,1] + observed_overall[2,2])

ci_action_group1 <- binom.test(x = observed_overall[1, 1], n = group1_total, conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_overall[1, 2], n = group1_total, conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_action_group2 <- binom.test(x = observed_overall[2, 1], n = group2_total, conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_overall[2, 2], n = group2_total, conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Action - Group 1: ", ci_action_group1[1], " - ", ci_action_group1[2], "\n")
cat("Action - Group 2: ", ci_action_group2[1], " - ", ci_action_group2[2], "\n")
cat("No Action - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("No Action - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


# Response rate by user account duration ----------------------------------

response_by_account_age <- data %>%
  dplyr::select(user_id,group, action, days_since_account_creation) %>%
  mutate(account_age_category = case_when(
    days_since_account_creation == 0 ~ 0,
    days_since_account_creation <= 180 ~ 16,
    days_since_account_creation <= 365 ~ 61,
    days_since_account_creation <= 730 ~ 12,
    days_since_account_creation <= 1095 ~ 23,
    days_since_account_creation <= 1460 ~ 34,
    days_since_account_creation <= 1825 ~ 45,
    days_since_account_creation <= 2190 ~ 56,
    days_since_account_creation <= 2555 ~ 67,
    days_since_account_creation <= 2920 ~ 78,
    days_since_account_creation <= 3285 ~ 89,
    days_since_account_creation <= 3650 ~ 910,
    days_since_account_creation > 3650 ~ 10
  ))

#group_by(account_age_category) %>%
#summarise(count = n())

invalid_rows <- data[data$days_since_account_creation < 0, ]

na_values_days <- data %>%
  select(days_since_account_creation) %>%
  filter(is.na(days_since_account_creation))


user_counts_by_age <- response_by_account_age %>%
  group_by(group,account_age_category) %>%
  summarise(actions = sum(action),
            no_actions = sum(!action),
            user_count = n_distinct(user_id),
            conversion_rate_action = (no_actions / user_count),
            conversion_rate_no = (actions / user_count))

# Impact of time of day on response rate ----------------------------------

response_by_hour <- data %>%
  dplyr::select(user_id,group, action, hour) %>%
  mutate(time_of_day = case_when(
    hour >= 6 & hour < 12 ~ "Morning",
    hour >= 12 & hour < 18 ~ "Afternoon",
    hour >= 18 | hour < 6 ~ "Night" ))


users_selected_hour <- response_by_hour %>%
  group_by(group,time_of_day) %>%
  summarise(actions = sum(action),
            no_actions = sum(!action),
            user_count = n_distinct(user_id),
            conversion_rate_action = (no_actions / user_count),
            conversion_rate_no = (actions / user_count))

afternoon_group <- subset(response_by_hour, group == 2 & time_of_day == "Afternoon")
shuffled_afternoon_group <- afternoon_group[sample(nrow(afternoon_group)), ]

# Take a random sample of rows from the afternoon_group
sample_size <- 1220  
sample <- shuffled_afternoon_group[1:sample_size, ]

hours_sampled <- response_by_hour[response_by_hour$group != 2 | response_by_hour$time_of_day != "Afternoon", ]  
hours_sampled <- rbind(hours_sampled, sample) 

# logistic regression ----------------------------------------------------------

# interaction effect
model_inter <- glm(formula = action ~ group * time_of_day, family = binomial, data = hours_sampled)


summary(model_inter)

# individual variables effect
model_ind <- glm(action ~ group + time_of_day, data = hours_sampled, family = binomial)


summary(model_ind)

coeff <- coef(summary(model_ind))["group", "Estimate"]

odds_ratio <- exp(coeff)

# Calculate the change in odds
change_in_odds <- odds_ratio - 1

# Convert the change in odds to a change in probability
probability_change <- change_in_odds / (1 + change_in_odds)

# Print the probability change
print(probability_change)



