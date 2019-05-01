####################################################
#### 15.570 - Digital Marketing                 ####          
#### Recitation #2 - Propensity Matching and IV #### 
#### Dec 1, 2017                                ####
####################################################

# Install packages (only need to run these once)
install.packages("MatchIt")  # Install package for propensity score matching 
install.packages("AER")  # Install package for instrumental variables

# Clear existing data
#rm(list=ls())
# Load dataset
highnote = read.csv("High_Note_data_csv.csv", stringsAsFactors =FALSE)
# To learn how to read and summarize data, please see the Recitation #1 or watch a tutorial video 
# such as this one: https://www.youtube.com/watch?v=hGkPgYfgapY
# Here is a video about basic plotting: https://www.youtube.com/watch?v=SjcUlHh3UJg

####################################
## Linear and Logistic Regression ##
####################################

# Build a Linear Regression model
# R automatically ignores all NA values while running a regression
# The model below takes 'songsListened' as the dependent variable, and a bunch of other variables
# as the independent variable. You can play around and try different combinations of variables
LinearRegModel = lm(songsListened ~ age + friend_cnt + lovedTracks + posts + playlists + 
                      shouts, data = highnote)

# View a summary of the results of the model
summary(LinearRegModel)
# R tells you how significant each variable is by putting asterisks (stars) next to each variable

# Build a Logistic Model. You can play around with the variables you use
# A Logistic Model is used when the dependent variable has a limited number of possible
# values (e.g., adopter or not adopter)
LogisticModel = glm(adopter ~ tenure + good_country + songsListened + male + friend_cnt + shouts + 
                      posts + playlists, data = highnote, family = binomial)

# View a summary of the results of the model
summary(LogisticModel)

# Calculate the Odds Ratio for each independent variable. The odds ratio helps you 
# understand the increase in odds of success of the dependent variable, given a unit
# increase in the independent variable.
exp(l;(LogisticModel))


###############################
## Propensity Score Matching ##
###############################

# For now, setting all NA values to 0 (because propensity score matching won't work with 
# missing values)
# May want to think about other ways to deal with these NAs (e.g. replacing them w/ other 
# numbers or excluding certain variables or observations)
write.csv(highnote,"data.csv",na="0")
highnote=read.csv("data.csv")

# Load package for propensity score matching
library(MatchIt)

# For demonstrative purposes, I'm running my analysis on a sample of 10,000 observations
# from the dataset, since propensity matching takes a while (10 mins) on the overall dataset
mysample <- highnote[sample(1:nrow(highnote), 10000, replace=FALSE),]

# R Function for propensity score matching
# 1 to 1 match (because ratio = 1); can change to ratio = 2 for 1 to 2 match 
matchprocess = matchit(adopter ~ lovedTracks + posts + playlists + shouts + tenure,
                data = mysample, method = "nearest", ratio = 1)

# View summary statistics on 'treatment'/'control' groups, before and after matching
# See how the mean difference between the 2 groups decreases significantly after matching
summary(matchprocess)

# Export the matched observations to a new dataframe
matchdata<- match.data(matchprocess) 

# See how this new 'matched' dataset now has half adopters and half non adopters
table(matchdata$adopter)
View(matchdata)


########################################################
## Demonstrative Example of Instrumental Variable Reg ##
########################################################

# Load package for instrumental variables
library(AER)

# Simultaneity bias between playlists and songs listened.
regmodel <- lm(playlists ~ songsListened + age, data = highnote)
summary(regmodel)

# Assume you have determined that lovedTracks is a good instrumental variable for 
# songs listened. Then you can redo the regression as follows:
ivmodel<- ivreg(playlists ~ age + songsListened | age + lovedTracks, data = highnote)
summary(ivmodel)

