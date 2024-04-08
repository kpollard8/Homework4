
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

final.data<- read_rds("data/output/final_ma_data.rds")

library(dplyr)
library(ggplot2)

# Question 1
# Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). 
# Provide a box and whisker plot showing the distribution of plan counts by county over time. 
# Do you think that the number of plans is sufficient, too few, or too many?

# Group data by county and year, and summarize plan counts
summary_data <- final.data %>%
  group_by(county, year) %>%
  summarize(planid = n())

# Create a boxplot to visualize the distribution of plan counts by county over time
question1<- ggplot(summary_data, aes(x = factor(year), y = planid)) +
  geom_boxplot() +
  labs(x = "Year", y = "Plan Counts", title = "Distribution of Plan Counts by County Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

question1


#Question 2 
# Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
#How has this distribution changed over time?

library(ggplot2)

# Filter the data for the years 2010, 2012, and 2015
filtered_data <- final.data %>%
  filter(year %in% c(2010, 2012, 2015))

# Count the occurrences of each star rating for each year
rating_counts <- filtered_data %>%
  group_by(year, Star_Rating) %>%
  summarise(count = n())

# Create bar graphs for each year's distribution of star ratings
question2<- ggplot(rating_counts, aes(x = factor(Star_Rating), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, scales = "free") +
  labs(x = "Star Rating", y = "Count", title = "Distribution of Star Ratings Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

question2


#Question 3
# Plot the average benchmark payment over time from 2010 through 2015. 
#How much has the average benchmark payment risen over the years?

# Filter the data for years 2010 to 2015
filtered_data <- final.data %>%
  filter(year >= 2010 & year <= 2015)

# Group the data by year and calculate the average benchmark payment for each year
average_benchmark_payments <- filtered_data %>%
  group_by(year) %>%
  summarize(average_benchmark_payment = mean(ma_rate, na.rm = TRUE))

# Print or view the result
print(average_benchmark_payments)

library(ggplot2)

# Assuming average_benchmark_payments is the data frame containing average benchmark payments for each year

# Plotting the average benchmark payments over time
question3<- ggplot(average_benchmark_payments, aes(x = year, y = average_benchmark_payment)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Benchmark Payment Over Time (2010-2015)",
       x = "Year",
       y = "Average Benchmark Payment") +
  theme_minimal()

question3

#Question 4
# Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
#Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?

ma.penetration<- read_rds("data/output/ma_penetration.rds")
ffs.costs<- read_rds("data/output/ffs_costs.rds")

enrollment_summary <- ffs.costs %>%
  group_by(year) %>%
  summarize(total_parta_enroll = sum(parta_enroll, na.rm = TRUE))

# Print or view the enrollment summary
print(enrollment_summary)

ma_eligibles <- ma.penetration %>%
  group_by(year) %>%
  summarize(ma_eligibles = sum(avg_eligibles, na.rm = TRUE))

# Print or view the result
print(ma_eligibles)

# Join the enrollment summary with MA eligibles data by year
total_eligibles <- enrollment_summary %>%
  left_join(ma_eligibles, by = "year") %>%
  # Calculate total Medicare eligibles by adding MA eligibles and total Part A enrollments
  mutate(total_medicare_eligibles = ma_eligibles + total_parta_enroll)

# Print or view the result
print(total_eligibles)

# Assuming your data spans from 2010 through 2015, you can filter the total_eligibles dataframe accordingly
filtered_data <- total_eligibles %>%
  filter(year >= 2010 & year <= 2015)

# Calculate the average share of Medicare Advantage relative to all Medicare eligibles
filtered_data <- filtered_data %>%
  mutate(average_ma_share = ma_eligibles / total_medicare_eligibles)

# Plot the data
question4<- ggplot(filtered_data, aes(x = year, y = average_ma_share)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Share of Medicare Advantage", title = "Average Share of Medicare Advantage Over Time (2010-2015)")

question4

#Question 5
# Calculate the running variable underlying the star rating. 
# Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

ma.data <- read_rds("data/output/final_ma_data.rds")

ma.data.clean <- ma.data %>%
  filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) 

ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess
          ,nodelays,carequickly,
          overallrating_care,overallrating_plan,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,bloodpressure,ra_manage,
          copd_test,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate)

# Round the running variable to the nearest 0.5 to determine the star rating
ma.data.clean <- ma.data.clean %>%
  mutate(star_rating = round(raw_rating * 2) / 2)

# Count the number of plans in each star rating category
star_rating_counts <- ma.data.clean %>%
  group_by(star_rating) %>%
  summarize(number_of_plans = n())

# Print or view the table
print(star_rating_counts)


#Question 6 
# Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
#Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.

# Install the 'rdrobust' package if not already installed
if (!require("rdrobust")) install.packages("rdrobust")

# Load the 'rdrobust' package
library(rdrobust)

# Estimate the effect of receiving a 3-star versus a 2.5-star rating
ma.rd3 <- ma.data.clean %>%
  filter(Star_Rating==2.5 | Star_Rating==3) %>%
  mutate(score = raw_rating - 3,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est3 <- rdrobust(y=ma.rd3$mkt_share, x=ma.rd3$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")

summary(est3)


# Estimate the effect of receiving a 3.5-star rating
ma.rd35 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.5,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est35 <- rdrobust(y=ma.rd35$mkt_share, x=ma.rd35$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est35)


#Question 7
#Repeat your results for bandwidths of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars).
#Show all results in a graph. 
#How sensitive are your findings to the choice of bandwidth?

# Define the bandwidths
bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

# Initialize an empty data frame to store the results
results <- data.frame()

# Loop over the bandwidths
for (h in bandwidths) {
  # Estimate the effect for 3-star rating
  est3 <- rdrobust(y=ma.rd3$mkt_share, x=ma.rd3$score, c=0, h=h, p=1, kernel="uniform", vce="hc0", masspoints="off")
  results <- rbind(results, data.frame(Bandwidth=h, Star_Rating=3, Estimate=est3$coef[1]))

  # Estimate the effect for 3.5-star rating
  est35 <- rdrobust(y=ma.rd35$mkt_share, x=ma.rd35$score, c=0, h=h, p=1, kernel="uniform", vce="hc0", masspoints="off")
  results <- rbind(results, data.frame(Bandwidth=h, Star_Rating=3.5, Estimate=est35$coef[1]))
}

# Plot the results
Q7 <- ggplot(results, aes(x=Bandwidth, y=Estimate, color=factor(Star_Rating))) +
  geom_line() +
  labs(x="Bandwidth", y="Estimate", color="Star Rating") +
  theme_minimal()

print(Q7)

#Question 8
# Examine (graphically) whether contracts appear to manipulate the running variable. 
#In other words, look at the distribution of the running variable before and after the relevent threshold values. What do you find?

install.packages("rddensity")
library(rddensity)
library(rdrobust)

# Create density plots for the scores around the threshold of 3 stars
dens3 <- rddensity(ma.rd3$score, c=0)
rdplotdensity(dens3, ma.rd3$score)

# Create density plots for the scores around the threshold of 3.5 stars
dens35 <- rddensity(ma.rd35$score, c=0)
rdplotdensity(dens35, ma.rd35$score)

#Question 9 
#Similar to question 4, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. 
#Use HMO and Part D status as your plan characteristics



rm(list=c("final.data", "summary_data", "filtered_data", "rating_counts", "ma.penetration", "ffs.costs", "enrollment_summary", "ma.data", "ma.data.clean","ma.rd3", "ma.rd35"))
save.image("submission 1/Hw4_workspace.Rdata")
