
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

# Question 1
# Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). 
# Provide a box and whisker plot showing the distribution of plan counts by county over time. 
# Do you think that the number of plans is sufficient, too few, or too many?

#Question 2 
# Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
#How has this distribution changed over time?

#Question 3
# Plot the average benchmark payment over time from 2010 through 2015. 
#How much has the average benchmark payment risen over the years?

#Question 4
# Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
#Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?

#Question 5
# Calculate the running variable underlying the star rating. 
# Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

#Question 6 
# Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
#Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.

#Question 7 
#Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars). 
# Show all of the results in a graph. How sensitive are your findings to the choice of bandwidth?

#Question 8
# Examine (graphically) whether contracts appear to manipulate the running variable. 
#In other words, look at the distribution of the running variable before and after the relevent threshold values. What do you find?

#Question 9 
#Similar to question 4, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. 
#Use HMO and Part D status as your plan characteristics

