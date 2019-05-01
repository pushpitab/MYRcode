#############################################
#### 15.570 - Digital Marketing          ####          
#### Recitation #1 - Introduction to R   #### 
#### Nov 17, 2017                        ####
#############################################

#############################
# Part 1: Loading CSV Files #
#############################
# Step 1: R reads CSV files best, so as a first step you should save the .xlsx file as .csv so you can read it into R
# Step 2: Navigate to the directory on your computer where the data file High_note_data_csv.csv is saved

# MAC: Misc->Change Working Directory
# PC: File->Change dir
#install.packages(ggplot2)
# Nothing should have happened in R, but if you type
install.packages("proto")
install.packages('ggplot2', dep = TRUE) 
install.packages("corrplot")
install.packages("Hmisc")
getwd()
# you should see the path to the folder you selected.

# you can also reset your working directory
setwd("C:/Users/Pushpita/Documents/R")

# Now, read in the data file by typing:
highnote = read.csv("High_Note_data_csv.csv", stringsAsFactors =FALSE)

# or equivalently use "assign" <-
highnote <- read.csv("High_Note_data_csv.csv", stringsAsFactors =FALSE)
# an easy way to get help for a function is using ?
?read.csv()

# first take a quick look of the data
dim(highnote)
head(highnote)
tail(highnote,20)
# This command reads in the data set, and saves it in a new variable called highnote in R


#############################
# Part 2: Data Exploration  #
#############################
# You can view the Structure of the newly loaded dataset to make sure it loaded correctly
# R converts all the blank values into NA
str(highnote)

# Statistical summary of the data:
# Summary of the entire dataset
summary(highnote$good_country)

highnote_goodcntry  = subset(highnote, good_country > 0)
dim(highnote_goodcntry)

# making reference to a subset of data

# row
new_data <- highnote[10,]
highnote[1:10,]
highnote[c(1:10,20),]

# columns 
highnote[,10]
highnote[,1:10]
highnote[,c(1:10,20)]

# Summary of a particular column
summary(highnote$friend_cnt)
# Mean of a particular column. You add na.rm = TRUE to ignore the NA values
friend_cnt_mean=mean(highnote$avg_friend_age,na.rm = TRUE)
#find standard deviation of new column, ignoring any NA values
friend_cnt_sd=sd(highnote$avg_friend_age, na.rm=TRUE)
#writting data to the file 
highnote_sub  = subset(highnote, adopter > 0)
highnote_goodcntry_sub  = subset(highnote_sub, good_country > 0)
dim(highnote_goodcntry_sub)

highnote_non_sub = subset(highnote, adopter < 1)

dim(highnote)

dim(highnote_sub)
dim (highnote_non_sub)


highnote_sub_male= subset(highnote_sub, male > 0)
dim(highnote_sub_male)
highnote_non_sub_male=subset(highnote_non_sub, male > 0)
dim(highnote_non_sub_male)
#highnote_sub <-highnote_non_sub



# Age summary 
summary(highnote_sub$age)
summary(highnote_non_sub$age)



#Omit NA and Age greater than 12 count
nonsub_highnote_age<-na.omit(highnote_non_sub$age)
nonsub_highnote_age_gtr12<-subset(highnote_non_sub, age > 12)
dim(nonsub_highnote_age_gtr12)

#Age

sub_highnote_age <- na.omit(highnote_sub$age)
age_sd=sd(sub_highnote_age, na.rm=TRUE)
seq1_age <- c(summary(sub_highnote_age), sd= sd(highnote_sub$age, na.rm=TRUE) )
dim(sub_highnote_age)

#friend count 

summary(highnote_sub$friend_cnt)
sub_friend_cnt <-na.omit(highnote_sub$friend_cnt)
seq_frnd_cnt<-c(summary(sub_friend_cnt), sd=sd(highnote_sub$friend_cnt, na.rm=TRUE))
seq_frnd_cnt

summary(highnote_sub$friend_country_cnt)
friend_country_cnt <-na.omit(highnote_sub$friend_country_cnt)
seq2_frnd_cntry<-c(summary(friend_country_cnt), sd=sd(highnote_sub$friend_country_cnt, na.rm=TRUE))
seq2_frnd_cntry

summary(highnote_sub$songsListened)
seq_songsListened<-c(summary(highnote_sub$songsListened), sd=sd(highnote_sub$songsListened, na.rm=TRUE))
seq_songsListened


summary(highnote_sub$shouts)
shouts <- na.omit(highnote_sub$shouts)
seq_shouts<-c(summary(shouts), sd=sd(highnote_sub$shouts, na.rm=TRUE))
seq_shouts


summary(highnote_sub$playlists)
sub_playlists <- na.omit(highnote_sub$playlists)
seq_playlists<-c(summary(sub_playlists), sd=sd(highnote_sub$playlists, na.rm=TRUE))
seq_playlists


summary(highnote_sub$lovedTracks)
sub_lovedTracks <- na.omit(highnote_sub$lovedTracks)
seq_lovedTracks<-c(summary(sub_lovedTracks), sd=sd(highnote_sub$lovedTracks, na.rm=TRUE))
seq_lovedTracks

summary(highnote_sub$posts)
sub_posts <- na.omit(highnote_sub$posts)
seq_posts<-c(summary(sub_posts), sd=sd(highnote_sub$posts, na.rm=TRUE))
seq_posts


summary(highnote_sub$subscriber_friend_cnt)
sub_sub_friend_cnt <- na.omit(highnote_sub$subscriber_friend_cnt)
seq_sub_friend_cnt<-c(summary(sub_sub_friend_cnt), sd=sd(highnote_sub$subscriber_friend_cnt, na.rm=TRUE))
seq_sub_friend_cnt


summary(highnote_sub$tenure)
sub_tenure <- na.omit(highnote_sub$tenure)
seq_tenure_mon<-c(summary(sub_tenure), sd=sd(highnote_sub$tenure, na.rm=TRUE))
seq_tenure_mon
 
bind <- rbind(seq1_age,seq_frnd_cnt,seq_songsListened,seq_shouts,seq2_frnd_cntry,seq_playlists,seq_lovedTracks,seq_posts,seq_sub_friend_cnt,seq_tenure_mon)
write.csv(bind,file = "highnote_summary_non_sub_stat.csv", append = TRUE)






#delta1 friend count

summary(highnote$delta1_friend_cnt)

delta1_friend_cnt_gtr0<-subset(highnote, delta1_friend_cnt > 0)

summary(delta1_friend_cnt_gtr0$delta1_friend_cnt)
seq_delta1_friend_cn<-c(summary(delta1_friend_cnt_gtr0$delta1_friend_cnt), sd=sd(delta1_friend_cnt_gtr0$delta1_friend_cnt, na.rm=TRUE))
seq_delta1_friend_cn


summary(highnote$delta1_lovedTracks)

delta1_lovedTracks_gtr0<-subset(highnote, delta1_lovedTracks > 0)
summary(delta1_lovedTracks_gtr0$delta1_lovedTracks)
seq_delta1_lovedTracks<-c(summary(delta1_lovedTracks_gtr0$delta1_lovedTracks), sd=sd(delta1_lovedTracks_gtr0$delta1_lovedTracks, na.rm=TRUE))
seq_delta1_lovedTracks


summary(highnote$delta1_posts)
delta1_posts_gtr0<-subset(highnote, delta1_posts > 0)
summary(delta1_posts_gtr0$delta1_posts)
seq_delta1_post<-c(summary(delta1_posts_gtr0$delta1_posts), sd=sd(delta1_posts_gtr0$delta1_posts, na.rm=TRUE))
seq_delta1_post


summary(highnote$delta1_shouts)
delta1_shouts_gtr0<-subset(highnote, delta1_shouts > 0)
summary(delta1_shouts_gtr0$delta1_shouts)
seq_delta1_post<-c(summary(delta1_shouts_gtr0$delta1_shouts), sd=sd(delta1_shouts_gtr0$delta1_shouts, na.rm=TRUE))
seq_delta1_post

summary(highnote$delta1_songsListened)
delta1_songsListened_gtr0<-subset(highnote, delta1_songsListened > 0)
summary(delta1_songsListened_gtr0$delta1_songsListened)
seq_delta1_songsListened_gtr0<-c(summary(delta1_songsListened_gtr0$delta1_songsListened), sd=sd(delta1_songsListened_gtr0$delta1_songsListened, na.rm=TRUE))
seq_delta1_songsListened_gtr0
dim(seq_delta1_songsListened_gtr0)

#######################################
# Part 3: Data Manipulation/Cleaning  #
#######################################

# CREATING NEW VARIABLES
# This statement adds a new variable 'log_avg_friend_age' which contains the log of 'avg_friend_age'
highnote$log_avg_friend_age = log(highnote$avg_friend_age)



# DELETING NEW VARIABLES
highnote$log_avg_friend_age = NULL

# CONVERT NEGATIVE VALUES INTO NA
# If a variable has values that you think might be an error (e.g. negative age values), you can convert those questionable entries to NA, so that R will ignore them when doing calculations/analyses. 
# In the example below, we felt that delta1_avg_friend_age should not have very low negative values. We made an assumption that if delta1_avg_friend_age was < -2, then we would replace the value with NA. In the original dataset, the lowest value for this column is -50.
# Create a new variable
highnote$delta1_avg_friend_age_2 = highnote$delta1_avg_friend_age
summary(highnote$delta1_avg_friend_age_2)
# Convert all values < -2 to NA


highnote$delta1_avg_friend_age_2[highnote$delta1_avg_friend_age_2 < 12] = NA
summary(highnote$delta1_avg_friend_age_2)

# Now all values < -2 have been converted to NA
# You can use this strategy to convert all noisy data into NA


highnote$subfriendcount_treat=highnote$subscriber_friend_cnt
highnote$subfriendcount_treat[highnote$subfriendcount_treat > 0] = 1
highnote$subfriendcount_treat[highnote$subfriendcount_treat <= 0] = 0
summary(highnote$subfriendcount_treat)
highnote_subfriendcount_treat1 = subset(highnote, subfriendcount_treat > 0)
dim(highnote_subfriendcount_treat1)

#CREATING A SUBSET OF THE MAIN DATASET 
#Create a newdataset that is a subset of the larger highnote dataset
highnote_songs1000 = subset(highnote, songsListened > 1000)
#rm(highnote_songs1000)

###########################
# Part 4: Exporting Data  #
###########################
#EXPORTING DATASETS INTO EXCEL
#this takes the dataset highnote_songs1000 and writes it to a csv file called "highnote_songs1000.csv", saved in
#in your active directory
write.csv(highnote_songs1000, file = "highnote_songs1000.csv")
colnames(highnote)

#################################
# Part 5: Simple Graphing in R  #
#################################
# Creating scatter plots of any two variables
plot(highnote$age,highnote$friend_cnt)

# Creating histograms
hist(highnote$avg_friend_age)

quantile(highnote$songsListened)

songslistened_100 <- highnote$songsListened[highnote$songsListened>=100]
hist(songslistened_100,breaks=seq(100,100000))
class(songslistened_100)

hist(highnote$songsListened)
summary(highnote$friend_cnt)
table(highnote$friend_cnt)
# Since friend_cnt does not have a normal distribution, the histogram is not very helpful. In such a case, it is helpful to take the log of this variable and plot the histogram
highnote$log_friend_cnt = log(highnote$friend_cnt)

# Creating boxplots
boxplot(age~male, data = highnote, xlab="Gender (0 = Female, 1 = Male)", ylab="Member Age")
#upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
#lower whisker = max(min(x), Q_1 – 1.5 * IQR)
#where IQR = Q_3 – Q_1, the box length.

# CORRELATIONS
# You can calculate correlation between pairs of variables. 
#To get rid of NA values while calculating correlation, add the phase:
# 'use = 'pairwise.complete.obs'
cor(highnote$tenure, highnote$songsListened, use='pairwise.complete.obs')


highnote_my_data <- highnote[, c(1,2,3,4,5,6,7,8,9,10,11,12,24,25,23)]
head(highnote_my_data, 6)
highnote_mydata_cor <- cor(highnote_my_data, use='pairwise.complete.obs')
highnote_mydata_cor<- round(highnote_mydata_cor,2)
highnote_mydata_cor
#correlation plotting

cor_bind <- rbind(highnote_mydata_cor)
write.csv(cor_bind, file = "highnote_cor_matrix.csv")
#table count  summary of a variable
table(highnote$age)
table(highnote$songsListened)



summary(highnote$subscriber_friend_cnt)
#cleaning up data
highnote_clean <- subset(highnote, age > 0)
dim(highnote_clean)
highnote_clean2 <- subset(highnote_clean, male >= 0)
dim(highnote_clean2)
highnote_clean3 <- subset(highnote_clean2, good_country >= 0)
dim(highnote_clean3)
highnote_clean4 <- subset(highnote_clean3, delta1_good_country >= 0)
dim(highnote_clean4)
highnote_clean5 <- subset(highnote_clean4, delta2_good_country >= 0)
dim(highnote_clean5)

highnote_clean5$delta1_avg_friend_age <- NULL
dim(highnote_clean5)
highnote_clean5$delta2_avg_friend_age <- NULL
dim(highnote_clean5)

highnote_clean6 <- subset(highnote_clean5, delta1_songsListened >= 0)
dim(highnote_clean6)

highnote_clean7 <- subset(highnote_clean6, delta2_songsListened >= 0)
dim(highnote_clean7)

highnote_clean8 <- subset(highnote_clean7, delta1_friend_cnt >= 0)
dim(highnote_clean8)


highnote_clean9 <- subset(highnote_clean8, delta1_subscriber_friend_cnt>= 0)
dim(highnote_clean9)

highnote_clean10 <- subset(highnote_clean9, delta1_lovedTracks>= 0)
dim(highnote_clean10)

highnote_clean11 <- subset(highnote_clean10, delta1_posts>= 0)
dim(highnote_clean11)

highnote_clean12 <- subset(highnote_clean11, delta1_playlists>= 0)
dim(highnote_clean12)


highnote_clean13 <- subset(highnote_clean12, delta1_shouts>= 0)
dim(highnote_clean13)



highnote_clean14 <- subset(highnote_clean13, delta2_lovedTracks>= 0)
dim(highnote_clean14)

highnote_clean15 <- subset(highnote_clean14,delta2_posts>= 0)
dim(highnote_clean15)


highnote_clean16 <- subset(highnote_clean15,delta2_subscriber_friend_cnt>= 0)
dim(highnote_clean16)

#highnote_clean17 <- subset(highnote_clean16,adopter> 0)
#dim(highnote_clean17)

#cleanned ###
write.csv(highnote_clean16, file = "highnote_clean.csv")
colnames(highnote_clean16)

#cor age, gender,tenure, songs list, playlist, lovdtrack,frn cnt,sub frnd cnt,post,shouts,good country
highnote_my_data <- highnote_clean16[, c(1,2,3,6,7,8,9,10,11,12,24,23)]
head(highnote_my_data, 6)
highnote_mydata_cor <- cor(highnote_my_data, use='pairwise.complete.obs')
highnote_mydata_cor<- round(highnote_mydata_cor,2)
highnote_mydata_cor
#correlation plotting

cor_bind <- rbind(highnote_mydata_cor)
write.csv(cor_bind, file = "highnote_cleaned_cor_matrix.csv")


summary(highnote_clean16$subscriber_friend_cnt)

#  summary 

highnote_clean17 <- subset(highnote_clean16,adopter == 0)
dim(highnote_clean17)

highnote_sub<-highnote_clean17
dim(highnote_sub)



summary(highnote_clean17$subscriber_friend_cnt)

#Age

summary(highnote_sub$age)
sub_highnote_age <-na.omit(highnote_sub$age)

age_sd=sd(sub_highnote_age, na.rm=TRUE)
seq1_age <- c(summary(sub_highnote_age), sd= sd(highnote_sub$age, na.rm=TRUE) )
seq1_age

#friend count 

summary(highnote_sub$friend_cnt)
sub_friend_cnt <-na.omit(highnote_sub$friend_cnt)
seq_frnd_cnt<-c(summary(sub_friend_cnt), sd=sd(highnote_sub$friend_cnt, na.rm=TRUE))
seq_frnd_cnt


#song listned
summary(highnote_sub$songsListened)
seq_songsListened<-c(summary(highnote_sub$songsListened), sd=sd(highnote_sub$songsListened, na.rm=TRUE))
seq_songsListened


#shouts

summary(highnote_sub$shouts)
seq_shouts<-c(summary(highnote_sub$shouts), sd=sd(highnote_sub$shouts, na.rm=TRUE))
seq_shouts

####


summary(highnote_sub$playlists)
sub_playlists <- na.omit(highnote_sub$playlists)
seq_playlists<-c(summary(sub_playlists), sd=sd(highnote_sub$playlists, na.rm=TRUE))
seq_playlists


summary(highnote_sub$lovedTracks)
sub_lovedTracks <- na.omit(highnote_sub$lovedTracks)
seq_lovedTracks<-c(summary(sub_lovedTracks), sd=sd(highnote_sub$lovedTracks, na.rm=TRUE))
seq_lovedTracks

summary(highnote_sub$posts)
sub_posts <- na.omit(highnote_sub$posts)
seq_posts<-c(summary(sub_posts), sd=sd(highnote_sub$posts, na.rm=TRUE))
seq_posts


summary(highnote_sub$subscriber_friend_cnt)
sub_sub_friend_cnt <- na.omit(highnote_sub$subscriber_friend_cnt)
seq_sub_friend_cnt<-c(summary(sub_sub_friend_cnt), sd=sd(highnote_sub$subscriber_friend_cnt, na.rm=TRUE))
seq_sub_friend_cnt


summary(highnote_sub$tenure)
sub_tenure <- na.omit(highnote_sub$tenure)
seq_tenure_mon<-c(summary(sub_tenure), sd=sd(highnote_sub$tenure, na.rm=TRUE))
seq_tenure_mon

bind <- rbind(seq1_age,seq_frnd_cnt,seq_songsListened,seq_shouts,seq_playlists,
              seq_lovedTracks,seq_posts,seq_sub_friend_cnt,seq_tenure_mon)

write.csv(bind,file = "highnote_clean_summary_corected_Adopter_stat.csv", append = TRUE)

################### delta1

summary(highnote_sub$delta1_playlists)
delta1_playlists <- na.omit(highnote_sub$delta1_playlists)
seq_delta1_playlists<-c(summary(delta1_playlists), sd=sd(highnote_sub$delta1_playlists, na.rm=TRUE))
seq_delta1_playlists

dim(subset(highnote_sub,delta1_playlists == 0))
ggplot(highnote_sub, aes(x = delta1_playlists)) +
  geom_histogram(breaks=seq(-1, 100, by=5), col="black", fill="green")

summary(highnote_sub$delta1_lovedTracks)
delta1_lovedTracks <- na.omit(highnote_sub$delta1_lovedTracks)
seq_delta1_lovedTracks<-c(summary(delta1_lovedTracks), sd=sd(highnote_sub$delta1_lovedTracks, na.rm=TRUE))
seq_delta1_lovedTracks



summary(highnote_sub$delta1_posts)
delta1_posts <- na.omit(highnote_sub$delta1_posts)
seq_delta1_post<-c(summary(delta1_posts), sd=sd(highnote_sub$delta1_posts, na.rm=TRUE))
seq_delta1_post


summary(highnote_sub$delta1_friend_cnt)
delta1_friend_cnt <- na.omit(highnote_sub$delta1_friend_cnt)
seq_delta1_friend_cnt<-c(summary(delta1_friend_cnt), sd=sd(highnote_sub$delta1_friend_cnt, na.rm=TRUE))
seq_delta1_friend_cnt

summary(highnote_sub$delta1_songsListened)
delta1_songsListened <- na.omit(highnote_sub$delta1_songsListened)
seq_delta1_songsListened<-c(summary(delta1_songsListened), sd=sd(highnote_sub$delta1_songsListened, na.rm=TRUE))
seq_delta1_songsListened


summary(highnote_sub$delta1_subscriber_friend_cnt)
delta1_subscriber_friend_cnt <- na.omit(highnote_sub$delta1_subscriber_friend_cnt)
seq_delta1_subscriber_friend_cnt<-c(summary(delta1_subscriber_friend_cnt), sd=sd(highnote_sub$delta1_subscriber_friend_cnt, na.rm=TRUE))
seq_delta1_subscriber_friend_cnt


bind <- rbind(seq_delta1_playlists,seq_delta1_lovedTracks,
              seq_delta1_post,seq_delta1_friend_cnt,seq_delta1_songsListened,seq_delta1_subscriber_friend_cnt)

write.csv(bind,file = "HN_clean_summary_corected_DELTA1_Adopter_stat.csv", append = TRUE)



##########Delta2
summary(highnote_sub$delta2_friend_cnt)
delta2_friend_cnt <- na.omit(highnote_sub$delta2_friend_cnt)
seq_delta2_friend_cnt<-c(summary(delta2_friend_cnt), sd=sd(highnote_sub$delta2_friend_cnt, na.rm=TRUE))
seq_delta2_friend_cnt

summary(highnote_sub$delta2_lovedTracks)
delta2_lovedTracks <- na.omit(highnote_sub$delta2_lovedTracks)
seq_delta2_lovedTracks<-c(summary(delta2_lovedTracks), sd=sd(highnote_sub$delta2_lovedTracks, na.rm=TRUE))
seq_delta2_lovedTracks


summary(highnote_sub$delta2_posts)
delta2_posts <- na.omit(highnote_sub$delta2_posts)
seq_delta2_posts<-c(summary(delta2_posts), sd=sd(highnote_sub$delta2_posts, na.rm=TRUE))
seq_delta2_posts


summary(highnote_sub$delta2_songsListened)
delta2_songsListened <- na.omit(highnote_sub$delta2_songsListened)
seq_delta2_songsListened<-c(summary(delta2_songsListened), sd=sd(highnote_sub$delta2_songsListened, na.rm=TRUE))
seq_delta2_songsListened


summary(highnote_sub$delta2_subscriber_friend_cnt)
delta2_subscriber_friend_cnt <- na.omit(highnote_sub$delta2_subscriber_friend_cnt)
seq_delta2_subscriber_friend_cnt<-c(summary(delta2_subscriber_friend_cnt), sd=sd(highnote_sub$delta2_subscriber_friend_cnt, na.rm=TRUE))
seq_delta2_subscriber_friend_cnt


summary(highnote_sub$delta2_playlists)
delta2_playlists <- na.omit(highnote_sub$delta2_playlists)
seq_delta2_playlists<-c(summary(delta2_playlists), sd=sd(highnote_sub$delta2_playlists, na.rm=TRUE))
seq_delta2_playlists
################################
bind <- rbind(seq1_age,seq_frnd_cnt,seq_songsListened,seq_shouts,seq_playlists,seq_lovedTracks,seq_posts,seq_sub_friend_cnt
   ,seq_tenure_mon,seq_delta1_playlists,seq_delta1_lovedTracks,seq_delta1_post,seq_delta1_friend_cnt,seq_delta1_songsListened,
   seq_delta1_subscriber_friend_cnt,seq_delta2_friend_cnt,seq_delta2_lovedTracks,seq_delta2_posts,seq_delta2_songsListened,seq_delta2_subscriber_friend_cnt,seq_delta2_playlists)


write.csv(bind,file = "highnote_clean_summary_NON_Adopter_stat.csv", append = TRUE)




###################Treatment###############

highnote_clean16$subfriendcount_treat=highnote_clean16$subscriber_friend_cnt
highnote_clean16$subfriendcount_treat[highnote_clean16$subfriendcount_treat > 0] = 1
highnote_clean16$subfriendcount_treat[highnote_clean16$subfriendcount_treat <= 0] = 0
summary(highnote_clean16$subfriendcount_treat)
highnote_subfriendcount_treat1 = subset(highnote_clean16, subfriendcount_treat > 0)
dim(highnote_subfriendcount_treat1)

write.csv(highnote_clean16,file = "highnote_psm_subfriend.csv", append = TRUE)

#######################
highnote_clean16$tenure_treat= highnote_clean16$tenure
summary(highnote_clean16$tenure_treat)


highnote_clean16$tenure_treat[highnote_clean16$tenure_treat <45 ] = 1
highnote_clean16$tenure_treat[highnote_clean16$tenure_treat > 44] = 0

highnote_tenure_treat1 = subset(highnote_clean16, tenure_treat >0)
dim(highnote_tenure_treat1)
#write.csv(highnote_clean16,file = "highnote_psm_tenure_treat.csv", append = TRUE)


###########changing treatment logic
highnote_clean16$age_treat=highnote_clean16$age
summary(highnote_clean16$age_treat)

highnote_clean16$age_treat[highnote_clean16$age_treat < 23 ] = 0
highnote_clean16$age_treat[highnote_clean16$age_treat > 35 ] = 0
highnote_clean16$age_treat[highnote_clean16$age_treat > 22] = 1

summary(highnote_clean16$age_treat)

highnote_age_treat1 = subset(highnote_clean16, age_treat == 1)
dim(highnote_age_treat1)


#write.csv(highnote_clean16,file = "highnote_psm_age_NEW_treat.csv", append = TRUE)

########################### TENURE

write.csv(highnote_clean16,file = "highnote_PSM_AGE_tenure_updated_treat.csv", append = TRUE)

#ggplot
library(ggplot2)
ggplot(highnote_clean16, aes(x = subscriber_friend_cnt)) +
  geom_histogram(breaks=seq(0, 5, by=1), col="black", fill="green")


ggplot(highnote_clean16, aes(x = good_country)) +
  geom_histogram(breaks=seq(-1, 2, by=1), col="black", fill="green")

ggplot(highnote_clean16, aes(x = tenure)) +
  geom_histogram(breaks=seq(0, 150, by=10),col="red", fill="green")

ggplot(highnote_clean16, aes(x = age)) +
  geom_histogram(breaks=seq(0, 100, by=5),col="red", fill="green")

ggplot(highnote_clean16, aes(x = friend_cnt)) +
  geom_histogram(breaks=seq(0, 1000, by=10),col="red", fill="green")

ggplot(highnote_clean16, aes(x = playlists)) +
  geom_histogram(breaks=seq(0, 5, by=1),col="red", fill="green")






##########################################################

ggplot(highnote_clean16, aes(x = songsListened)) +
  geom_histogram(breaks=seq(0, 50000, by=500),limits=c(0,50000), col="black", fill="green")


ggplot(highnote_clean16, aes(x = delta1_songsListened)) +
  geom_histogram(breaks=seq(0, 10, by=1),limits=c(0, 100), col="black", fill="green")


ggplot(highnote, aes(x = friend_cnt)) +
  geom_histogram(breaks=seq(0, 1000, by=50),col="red", fill="green")



ggplot(highnote, aes(x = posts)) +
  geom_histogram(breaks=seq(0, 10, by=1),col="red", fill="green")



ggplot(highnote, aes(x = lovedTracks)) +
  geom_histogram(breaks=seq(0, 1500, by=100),col="red", fill="green")

ggplot(highnote, aes(x = playlists)) +
  geom_histogram(breaks=seq(0, 5, by=1),col="red", fill="green")

ggplot(highnote, aes(x = shouts)) +
  geom_histogram(breaks=seq(0, 20, by=1),col="red", fill="green")


ggplot(highnote, aes(x = delta1_playlists)) +
  geom_histogram(breaks=seq(0, 5, by=1),col="red", fill="green")

ggplot(highnote, aes(x = delta1_shouts)) +
  geom_histogram(breaks=seq(0, 20, by=1),col="red", fill="green") 
