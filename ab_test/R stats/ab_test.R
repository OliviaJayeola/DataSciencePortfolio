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
  filter(if_all(c(days_since_account_creation, num_downloads, num_searches), ~ . >= 0)) %>%
  mutate(no_action = ifelse(click_yes == 0 & click_no == 0, 1, 0)) %>%
  mutate(subscription = ifelse(subscription == "paid", 1, 0)) %>%
  mutate(group = ifelse(group == "popup_1", 1, 2))

str(data)
summary(data)

na_values <- data %>%
  select(group,click_yes,click_no) %>%
  filter(is.na(group) | is.na(click_yes) | is.na(click_no))

clean_data_group <- data %>%
  filter(!is.na(group) & nzchar(as.character(group)))

count_people <- data %>%
  group_by(group) %>%
  summarise(people_count = n_distinct(user_id))

first_date <- min(data$date)  
last_date <- max(data$date) 

days_between <- as.integer(difftime(last_date, first_date, units = "days")) 

days_between

# overall counts ----------------------------------------------------------

# overall dataframe
overall <- data %>%
  select(user_id, group, click_yes, click_no, date) %>%
  filter(click_no==1 | click_yes==1)


# analysis starts

# for click yes and no

filtered_overall <- overall[overall$click_no == 1 | overall$click_yes == 1, ]
observed_overall <- table(filtered_overall$group, filtered_overall$click_yes, filtered_overall$click_no)

observed_overall

contingency_table_overall <- observed_overall %>%
  apply(c(1, 2), sum) %>%
  matrix(nrow = 2, ncol = 2, byrow = TRUE) %>%
  t()

rownames(contingency_table_overall) <- c("Group 1", "Group 2") 
colnames(contingency_table_overall) <- c("Click No", "Click Yes")

contingency_table_overall

chisq_result <- chisq.test(contingency_table_overall)

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

effect_size <- assocstats(contingency_table_overall)$cramer
effect_size



ci_yes_group1 <- binom.test(x = observed_overall[1, 2, 1], n = group1_total, conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_overall[1, 1, 2], n = group1_total, conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_yes_group2 <- binom.test(x = observed_overall[2, 2, 1], n = group2_total, conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_overall[2, 1, 2], n = group2_total, conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Click Yes - Group 1: ", ci_yes_group1[1], " - ", ci_yes_group1[2], "\n")
cat("Click Yes - Group 2: ", ci_yes_group2[1], " - ", ci_yes_group2[2], "\n")
cat("Click No - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("Click No - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")

bayes_result <- contingencyTableBF(contingency_table,sampleType = "indepMulti", fixedMargin='cols')
bayes_result

model <- generalTestBF(click_yes ~ group, data = overall_yes)
bf_result <- generalTestBF(model)
bf_result

# response by country -----------------------------------------------------

response_by_country <- data %>%
  select(user_id, group, click_yes, click_no, country, date) %>%
  filter(click_yes != 0 | click_no != 0)

clean_data_country <- response_by_country %>%
  filter(!is.na(country) & nzchar(country))

print(unique(clean_data_country$country))

user_counts_by_country <- clean_data_country %>%
  group_by(group,country) %>%
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))

selected_countries <- c("England")

users_selected_england <- clean_data_country %>%
  filter(country %in% selected_countries)
 

observed_england <- table(users_selected_england$group, users_selected_england$click_yes, users_selected_england$click_no)

observed_england

contingency_table_england <- observed_england %>%
  apply(c(1, 2), sum) %>%
  matrix(nrow = 2, ncol = 2, byrow = TRUE) %>%
  t()

rownames(contingency_table_england) <- c("Group 1", "Group 2") 
colnames(contingency_table_england) <- c("Click No", "Click Yes")

contingency_table_england

chisq_result <- chisq.test(contingency_table_england)

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

effect_size <- assocstats(contingency_table_england)$cramer
effect_size



ci_yes_group1 <- binom.test(x = observed_england[1, 2, 1], n =  as.numeric(user_counts_by_country[1,5]), conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_england[1, 1, 2], n =   as.numeric(user_counts_by_country[1,5]), conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_yes_group2 <- binom.test(x = observed_england[2, 2, 1], n = as.numeric(user_counts_by_country[6,5]), conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_england[2, 1, 2], n = as.numeric(user_counts_by_country[6,5]), conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Click Yes - Group 1: ", ci_yes_group1[1], " - ", ci_yes_group1[2], "\n")
cat("Click Yes - Group 2: ", ci_yes_group2[1], " - ", ci_yes_group2[2], "\n")
cat("Click No - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("Click No - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


# response by career ------------------------------------------------------

response_by_career <- data %>%
  select(user_id,group, click_yes, click_no, career,date) %>%
  filter(click_yes != 0 | click_no != 0)

clean_data_career <- response_by_career %>%
  filter(!is.na(career) & nzchar(career))

print(unique(clean_data_career$career))

user_counts_by_career <- clean_data_career %>%
  group_by(group,career) %>%
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))

selected_career <- c("KS1")

users_selected_KS1 <- clean_data_career %>%
  filter(career %in% selected_career)


observed_KS1 <- table(users_selected_KS1$group, users_selected_KS1$click_yes, users_selected_KS1$click_no)

observed_KS1

contingency_table_KS1 <- observed_KS1 %>%
  apply(c(1, 2), sum) %>%
  matrix(nrow = 2, ncol = 2, byrow = TRUE) %>%
  t()

rownames(contingency_table_KS1) <- c("Group 1", "Group 2") 
colnames(contingency_table_KS1) <- c("Click No", "Click Yes")

contingency_table_KS1

chisq_result <- chisq.test(contingency_table_KS1)

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

effect_size <- assocstats(contingency_table_KS1)$cramer
effect_size



ci_yes_group1 <- binom.test(x = observed_KS1[1, 2, 1], n =  as.numeric(user_counts_by_career[2,5]), conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_KS1[1, 1, 2], n =   as.numeric(user_counts_by_career[2,5]), conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_yes_group2 <- binom.test(x = observed_KS1[2, 2, 1], n = as.numeric(user_counts_by_career[9,5]), conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_KS1[2, 1, 2], n = as.numeric(user_counts_by_career[9,5]), conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Click Yes - Group 1: ", ci_yes_group1[1], " - ", ci_yes_group1[2], "\n")
cat("Click Yes - Group 2: ", ci_yes_group2[1], " - ", ci_yes_group2[2], "\n")
cat("Click No - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("Click No - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


# Impact of subscription on response rate ---------------------------------

subscription_response <- data %>%
  select(user_id, group, click_yes, click_no, subscription, date) %>%
  filter(click_yes != 0 | click_no != 0)

user_counts_by_subscription <- subscription_response %>%
  group_by(group,subscription) %>%
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))

users_subscribed <- subscription_response %>%
  filter(subscription == 1)

users_not_subscribed <- subscription_response %>%
  filter(subscription == 0)

observed_subscribed <- table(users_subscribed$group, users_subscribed$click_yes, users_subscribed$click_no)

observed_subscribed

contingency_table_subscribed <- observed_subscribed %>%
  apply(c(1, 2), sum) %>%
  matrix(nrow = 2, ncol = 2, byrow = TRUE) %>%
  t()

rownames(contingency_table_subscribed) <- c("Group 1", "Group 2") 
colnames(contingency_table_subscribed) <- c("Click No", "Click Yes")

contingency_table_subscribed

chisq_result <- chisq.test(contingency_table_subscribed)

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

effect_size <- assocstats(contingency_table_subscribed)$cramer
effect_size



ci_yes_group1 <- binom.test(x = observed_subscribed[1, 2, 1], n =  as.numeric(user_counts_by_subscription[2,5]), conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_subscribed[1, 1, 2], n =   as.numeric(user_counts_by_subscription[2,5]), conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_yes_group2 <- binom.test(x = observed_subscribed[2, 2, 1], n = as.numeric(user_counts_by_subscription[4,5]), conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_subscribed[2, 1, 2], n = as.numeric(user_counts_by_subscription[4,5]), conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Click Yes - Group 1: ", ci_yes_group1[1], " - ", ci_yes_group1[2], "\n")
cat("Click Yes - Group 2: ", ci_yes_group2[1], " - ", ci_yes_group2[2], "\n")
cat("Click No - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("Click No - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")

# Impact of time of day on response rate ----------------------------------

response_by_hour <- data %>%
  select(user_id,group, click_yes, click_no, hour) %>%
  filter(click_yes != 0 | click_no != 0) %>%
  mutate(time_of_day = case_when(
    hour >= 6 & hour < 12 ~ "Morning",
    hour >= 12 & hour < 18 ~ "Afternoon",
    hour >= 18 | hour < 6 ~ "Night" ))


users_selected_hour <- response_by_hour %>%
  group_by(group,time_of_day) %>%
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))

afternoon_group <- subset(response_by_hour, group == 2 & time_of_day == "Afternoon")
shuffled_afternoon_group <- afternoon_group[sample(nrow(afternoon_group)), ]

# Take a random sample of rows from the afternoon_group
sample_size <- 220  # Specify the desired sample size
sample <- shuffled_afternoon_group[1:sample_size, ]

hours_sampled <- response_by_hour
hours_sampled <- hours_sampled[hours_sampled$group != 2 | hours_sampled$time_of_day != "Afternoon", ]  
hours_sampled <- rbind(hours_sampled, sample) 


model_inter <- glm(formula = click_yes ~ group * time_of_day, family = binomial, data = hours_sampled)

# Get the model summary
summary(model_inter)

model_ind <- glm(click_yes ~ group + time_of_day, data = hours_sampled, family = binomial)

# Get the model summary
summary(model_ind)



# Response rate by user account duration ----------------------------------


response_by_account_age <- data %>%
  select(user_id,group, click_yes, click_no, days_since_account_creation) %>%
  filter(click_yes != 0 | click_no != 0) %>%
  mutate(account_age_category = case_when(
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
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))

# Group effect on searches and downloads ------------------------------------

downloads <- data %>%
  select(user_id,group,click_yes,click_no,num_downloads,date) %>%
  filter(click_yes != 0 | click_no != 0) %>%
  mutate(downloads_category = case_when(
    num_downloads == 0 ~ 0,
    num_downloads <= 50 ~ 50,
    num_downloads <= 250 ~ 250,
    num_downloads <= 500 ~ 500,
    num_downloads <= 750 ~ 750,
    num_downloads <= 1000 ~ 1000,
    num_downloads <= 1250 ~ 1250,
    num_downloads <= 1500 ~ 1500,
    num_downloads <= 1750 ~ 1750,
    num_downloads <= 2000 ~ 2000,
    num_downloads <= 2250 ~ 2250,
    num_downloads <= 2500 ~ 2500,
    num_downloads <= 2750 ~ 2750,
    num_downloads > 3000 ~ 3000))

user_counts_by_downloads <- downloads %>%
  group_by(group, downloads_category) %>%
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))
  
downloads0 <- downloads %>%
  filter(num_downloads == 0)


observed_downloads <- table(downloads0$group, downloads0$click_yes, downloads0$click_no)

observed_downloads

contingency_table_downloads <- observed_downloads %>%
  apply(c(1, 2), sum) %>%
  matrix(nrow = 2, ncol = 2, byrow = TRUE) %>%
  t()

rownames(contingency_table_downloads) <- c("Group 1", "Group 2") 
colnames(contingency_table_downloads) <- c("Click No", "Click Yes")

contingency_table_downloads

chisq_result <- chisq.test(contingency_table_downloads)

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

effect_size <- assocstats(contingency_table_downloads)$cramer
effect_size



ci_yes_group1 <- binom.test(x = observed_downloads[1, 2, 1], n =  as.numeric(user_counts_by_career[2,5]), conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_downloads[1, 1, 2], n =   as.numeric(user_counts_by_career[2,5]), conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_yes_group2 <- binom.test(x = observed_downloads[2, 2, 1], n = as.numeric(user_counts_by_career[9,5]), conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_downloads[2, 1, 2], n = as.numeric(user_counts_by_career[9,5]), conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Click Yes - Group 1: ", ci_yes_group1[1], " - ", ci_yes_group1[2], "\n")
cat("Click Yes - Group 2: ", ci_yes_group2[1], " - ", ci_yes_group2[2], "\n")
cat("Click No - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("Click No - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")


searches <- data %>%
  select(user_id,click_yes,click_no,num_searches,date) %>%
  filter(click_yes != 0 | click_no != 0) %>%
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
  group_by(searches_category) %>%
  summarise(click_yes_count = sum(click_yes),
            click_no_count = sum(click_no),
            user_count = n_distinct(user_id),
            conversion_rate_yes = (click_yes_count / user_count),
            conversion_rate_no = (click_no_count / user_count))

#group_by(searches_category) %>%
  #summarise(user_count = n_distinct(user_id))

searches0 <- searches %>%
  filter(searches_category == 0 | searches_category == 50)

searches50 <- searches %>%
  filter(searches_category == 50)

searches0$date <- as.Date(searches0$date)





model_inter <- glm(formula = click_yes ~ group * searches_category, family = binomial, data = searches0)

# Get the model summary
summary(model_inter)

model_ind <- glm(click_yes ~ group + searches_category, data = searches0, family = binomial)

# Get the model summary
summary(model_ind)


observed_searches <- table(searches50$click_yes, searches50$click_no)

observed_searches

observed_searches <- table(searches0$searches_category, searches0$click_yes, searches0$click_no)

# Print the contingency table
print(contingency_table)

contingency_table_searches <- observed_searches %>%
  apply(c(1, 2), sum) %>%
  matrix(nrow = 2, ncol = 2, byrow = TRUE) %>%
  t()

#rownames(contingency_table_searches) <- c("0", "50") 
colnames(contingency_table_searches) <- c("Click No", "Click Yes")

contingency_table_searches
contingency_table <- matrix(c(320, 261), nrow = 1, ncol = 2, byrow = TRUE)

# Perform the chi-squared test
chi_squared <- chisq.test(contingency_table)

# Print the test results
print(chi_squared)

chisq_result <- chisq.test(contingency_table_searches)

test_statistic <- chisq_result$statistic
p_value <- chisq_result$p.value

cat("Chi-Squared Test Results:\n")
cat("Test Statistic:", test_statistic, "\n")
cat("p-value:", p_value, "\n")


effect_size <- assocstats(contingency_table_searches)$cramer
effect_size



ci_yes_group1 <- binom.test(x = observed_searches[1, 2, 1], n =  as.numeric(user_counts_by_searches[1,4]), conf.level = 0.95)$conf.int
ci_no_group1 <- binom.test(x = observed_searches[1, 1, 2], n =   as.numeric(user_counts_by_searches[1,4]), conf.level = 0.95)$conf.int

# Calculate the confidence intervals for click yes and click no in Group 2
ci_yes_group2 <- binom.test(x = observed_searches[2, 2, 1], n = as.numeric(user_counts_by_searches[2,4]), conf.level = 0.95)$conf.int
ci_no_group2 <- binom.test(x = observed_searches[2, 1, 2], n = as.numeric(user_counts_by_searches[2,4]), conf.level = 0.95)$conf.int

# Print the results
cat("95% Confidence Intervals:\n")
cat("Click Yes - Group 1: ", ci_yes_group1[1], " - ", ci_yes_group1[2], "\n")
cat("Click Yes - Group 2: ", ci_yes_group2[1], " - ", ci_yes_group2[2], "\n")
cat("Click No - Group 1: ", ci_no_group1[1], " - ", ci_no_group1[2], "\n")
cat("Click No - Group 2: ", ci_no_group2[1], " - ", ci_no_group2[2], "\n")

  
  