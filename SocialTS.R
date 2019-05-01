setwd("C:/Users/Pushpita/Documents/R")
#getwd()
# install.packages("ggplot")
# install.packages("plotly")
#install.packages("plot3D")
#install.packages("magick")
#install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
library("plot3D")

library(plotly)

library(ggplot2)

social_param=read.csv("social_param.csv",header=TRUE,check.names=FALSE,stringsAsFactors =FALSE, na.strings=c("", "NA"))
social_param

tt <- c(social_param$Travel_Time)
tt<- na.omit(tt)
tt
sr <- c(social_param$Safety_Relibility)
sr
sr <- na.omit(sr)
sr

nw<-c(social_param$NV_Wthr)
nw<- na.omit(nw)
nw

es<-c(social_param$Energy_Source)
es
es<- na.omit(es)
es


er<-c(social_param$Expected_Range)
er<- na.omit(er)
er 

th<-c(social_param$Training_Hours)
th
th <- na.omit(th)

ft<-c(social_param$Flight_Technology)
ft <- na.omit(ft)

psg <-c(social_param$Passengers)
psg <- na.omit(psg)
psg

cost<-c(social_param$Cost)
cost <- na.omit(cost)
cost
dr<-c(social_param$Driving_Range)
dr <- na.omit(dr)

ndb<-c(social_param$Noise_db)
ndb <- na.omit(ndb)

dr<-c(social_param$Driving_Range)
dr
dr <- na.omit(ndb)


choise_grid<-expand.grid(tt,es,nw,ft,sr,er,psg,cost,ndb,th)
colnames(choise_grid) <- c("tt","es","nw","ft","sr","er","psg","cost","ndb","th")

dim(choise_grid)
choise_grid[1,]


calc.energysource.util <- function(es, returntype) {
  
  if(es == "Electric") {
    social_energysource_wt <- 1
    cv_energysource_wt<-0.5
    cost_ES_wt<-1
  }
  if(es == "Hybrid") {
   social_energysource_wt <- 0.7
   cv_energysource_wt<-0.8
   cost_ES_wt<-0.9
  }
  if(es == "Gas") {
    social_energysource_wt <- 0.2
    cv_energysource_wt<-1
    cost_ES_wt<-0.7
  }
 # social_energysource_wt
 # cv_energysource_wt
 # cost_ES_wt
  if(returntype=="cost")
  {
    return(cost_ES_wt)
  }
  else if (returntype=="cv"){
    return (cv_energysource_wt)
  }else{
    return (social_energysource_wt)
  }
  
}




choise_grid$cost_ES_wt<-mapply(calc.energysource.util,choise_grid$es,"cost")

summary(choise_grid$cost_ES_wt)


choise_grid$cv_ES_wt<-mapply(calc.energysource.util,choise_grid$es,"cv")

summary(choise_grid$cv_ES_wt)

choise_grid$social_ES_wt<-mapply(calc.energysource.util,choise_grid$es,"")

summary(choise_grid$social_ES_wt)
choise_grid[1,]


calc.NW.util <- function(nightvision_weather,type ) {
  
  if(nightvision_weather == "Independent") {
    social_nv_wt <- 1
    cv_nv_wt<-0.2
    cost_nv_wt<-1
  }
  
  if(nightvision_weather == "Dependent") {
    social_nv_wt <- 0
    cv_nv_wt<-0.9
    cost_nv_wt<-0.5
  }
 
 
  if(type=="cost")
  {
    return(cost_nv_wt)
  }
  else if (type=="cv"){
    return (cv_nv_wt)
  }else{
    return (social_nv_wt)
  }
  
}


choise_grid$cost_NW_wt<-mapply(calc.NW.util,choise_grid$nw,"cost")

summary(choise_grid$cost_NW_wt)


choise_grid$cv_NW_wt<-mapply(calc.NW.util,choise_grid$nw,"cv")

summary(choise_grid$cv_NW_wt)

choise_grid$social_NW_wt<-mapply(calc.NW.util,choise_grid$nw,"")

summary(choise_grid$social_NW_wt)

choise_grid[1,]


calc.FT.util <- function(flighttech, type) {
  
  if(flighttech == "VTOL") {
    social_ft_wt <- 1
    cv_ft_wt<-0.7
    cost_ft_wt<-1
  }
  
  if(flighttech == "STOL") {
    social_ft_wt <- 0.6
    cv_ft_wt<-0.9
    cost_ft_wt<-0.6
  }
  
  
  if(type=="cost")
  {
    return(cost_ft_wt)
  }
  else if (type=="cv"){
    return (cv_ft_wt)
  }else{
    return (social_ft_wt)
  }
}


choise_grid$cost_FT_wt<-mapply(calc.FT.util,choise_grid$ft,"cost")

summary(choise_grid$cost_FT_wt)


choise_grid$cv_FT_wt<-mapply(calc.FT.util,choise_grid$ft,"cv")

summary(choise_grid$cv_FT_wt)

choise_grid$social_FT_wt<-mapply(calc.FT.util,choise_grid$ft,"")

summary(choise_grid$social_FT_wt)

choise_grid[1,]


calc.traveltime.util <- function(traveltime, type) {
  
  
  if(traveltime == 10) {
    social_traveltime_wt <- 1
    cv_traveltime_wt<-0.2
    cost_traveltime_wt<-1
  }
  if(traveltime == 15) {
    social_traveltime_wt <- 1
    cv_traveltime_wt<-0.4
    cost_traveltime_wt<-0.8
  }
  if(traveltime == 20) {
    social_traveltime_wt <- 0.9
    cv_traveltime_wt<-0.6
    cost_traveltime_wt<-0.7
  }
  if(traveltime == 25) {
    social_traveltime_wt <- 0.8
    cv_traveltime_wt<-0.8
    cost_traveltime_wt<-0.6
  }
  
  if(traveltime == 30) {
    social_traveltime_wt <- 0.6
    cv_traveltime_wt<-1
    cost_traveltime_wt<-0.5
  }
  if(traveltime == 35) {
    social_traveltime_wt <- 0.5
    cv_traveltime_wt<-1
    cost_traveltime_wt<-0.4
  }
  if(traveltime == 40) {
    social_traveltime_wt <- 0.3
    cv_traveltime_wt<-1
    cost_traveltime_wt<-0.3
  }
  
  if(traveltime == 45) {
    social_traveltime_wt <- 0.1
    cv_traveltime_wt<-1
    cost_traveltime_wt<-0.1
  }
  if(type=="cost")
  {
    return(cost_traveltime_wt)
  }
  else if (type=="cv"){
    return (cv_traveltime_wt)
  }else{
    return (social_traveltime_wt)
  }
 
}


choise_grid$cost_TT_wt<-mapply(calc.traveltime.util,choise_grid$tt,"cost")

summary(choise_grid$cost_TT_wt)


choise_grid$cv_TT_wt<-mapply(calc.traveltime.util,choise_grid$tt,"cv")

summary(choise_grid$cv_TT_wt)

choise_grid$social_TT_wt<-mapply(calc.traveltime.util,choise_grid$tt,"")

summary(choise_grid$social_TT_wt)

choise_grid[1,]

calc.SR.util <- function(safety_rel,type) {
  
  if(safety_rel == "Low") {
    social_sr_wt <- 0
    cv_sr_wt<-1
    cost_sr_wt<-0.1
  }
  
  if(safety_rel == "Med") {
    social_sr_wt <- 0.2
    cv_sr_wt<-0.5
    cost_sr_wt<-0.5
  }
  
  
  if(safety_rel == "High") {
    social_sr_wt <- 1
    cv_sr_wt<-0.1
    cost_sr_wt<-1
  }
 
  
   if(type=="cost")
  {
    return(cost_sr_wt)
  }
  else if (type=="cv"){
    return (cv_sr_wt)
  }else{
    return (social_sr_wt)
  }
  
  
}


choise_grid$cost_SR_wt<-mapply(calc.SR.util,choise_grid$sr,"cost")

summary(choise_grid$cost_SR_wt)


choise_grid$cv_SR_wt<-mapply(calc.SR.util,choise_grid$sr,"cv")

summary(choise_grid$cv_SR_wt)

choise_grid$social_SR_wt<-mapply(calc.SR.util,choise_grid$sr,"")

summary(choise_grid$social_SR_wt)

choise_grid[1,]


calc.ER.util <- function(erange,type) {
  
 
  
  if(erange == 1) {
    social_er_wt <- 0.6
    cv_er_wt<-1
    cost_er_wt<-0.5
  }
  if(erange == 5) {
    social_er_wt <- 0.8
    cv_er_wt<-0.7
    cost_er_wt<-0.7
  }
  if(erange == 10) {
    social_er_wt <- 0.8
    cv_er_wt<-0.6
    cost_er_wt<-0.8
  }
  if(erange == 15) {
    social_er_wt <- 0.9
    cv_er_wt<-0.5
    cost_er_wt<-0.9
  }
  
  if(erange == 20) {
    social_er_wt <- 1
    cv_er_wt<-0.3
    cost_er_wt<-1
  }
  if(type=="cost")
  {
    return(cost_er_wt)
  }
  else if (type=="cv"){
    return (cv_er_wt)
  }else{
    return (social_er_wt)
  }
  
  
}
choise_grid$cost_ER_wt<-mapply(calc.ER.util,choise_grid$er,"cost")

summary(choise_grid$cost_ER_wt)


choise_grid$cv_ER_wt<-mapply(calc.ER.util,choise_grid$er,"cv")

summary(choise_grid$cv_ER_wt)

choise_grid$social_ER_wt<-mapply(calc.ER.util,choise_grid$er,"")

summary(choise_grid$social_ER_wt)

choise_grid[1,]



calc.PSG.util <- function(passenger,type) {
  

  if(passenger == 1) {
    social_psg_wt <- 0.1
    cv_psg_wt<-1
    cost_psg_wt<-0.2
  }
  if(passenger == 2) {
    social_psg_wt <- 0.4
    cv_psg_wt<-0.9
    cost_psg_wt<-0.4
  }
  if(passenger == 3) {
    social_psg_wt <- 0.7
    cv_psg_wt<-0.8
    cost_psg_wt<-0.6
  }
  if(passenger == 4) {
    social_psg_wt <- 1
    cv_psg_wt<-0.5
    cost_psg_wt<-1
  }
  
  if(passenger == 5) {
    social_psg_wt <- 1
    cv_psg_wt<-0.2
    cost_psg_wt<-1
  }
  
  
  if(type=="cost")
  {
    return(cost_psg_wt)
  }
  else if (type=="cv"){
    return (cv_psg_wt)
  }else{
    return (social_psg_wt)
  }
}


choise_grid$cost_PSG_wt<-mapply(calc.PSG.util,choise_grid$psg,"cost")

summary(choise_grid$cost_PSG_wt)


choise_grid$cv_PSG_wt<-mapply(calc.PSG.util,choise_grid$psg,"cv")

summary(choise_grid$cv_PSG_wt)

choise_grid$social_PSG_wt<-mapply(calc.PSG.util,choise_grid$psg,"")

summary(choise_grid$social_PSG_wt)

choise_grid[1,]

calc.COST.util <- function(cost, type) {
  cost_cost_wt<-0
  
  if(cost == 200000) {
    social_cost_wt <- 0.1
    cv_cost_wt<-1
    cost_cost_wt<-0.2
  }
  if(cost == 400000) {
    social_cost_wt <- 0.4
    cv_cost_wt<-0.9
    cost_cost_wt<-0.4
  }
  if(cost == 600000) {
    social_cost_wt <- 0.7
    cv_cost_wt<-0.8
    cost_cost_wt<-0.6
  }
  if(cost == 800000) {
    social_cost_wt <- 1
    cv_cost_wt<-0.5
    cost_cost_wt<-1
  }
  
  if(cost == 1000000) {
    social_cost_wt <- 1
    cv_cost_wt<-0.2
    cost_cost_wt<-1
  }
  
  if(type=="cost")
  {
    return(cost_cost_wt)
  }
  else if (type=="cv"){
    return (cv_cost_wt)
  }else{
    return (social_cost_wt)
  }
  
}

choise_grid$cost_MSRP_wt<-mapply(calc.COST.util,choise_grid$cost,"cost")

summary(choise_grid$cost_MSRP_wt)


choise_grid$cv_MSRP_wt<-mapply(calc.COST.util,choise_grid$cost,"cv")

summary(choise_grid$cv_MSRP_wt)

choise_grid$social_MSRP_wt<-mapply(calc.COST.util,choise_grid$cost,"")

summary(choise_grid$social_MSRP_wt)

choise_grid[1,]



calc.NDB.util <- function(ndb ,type ) {
  
  
  if(ndb == 50) {
    social_ndb_wt <- 1
    cv_ndb_wt<-0.1
    cost_ndb_wt<-1
  }
  if(ndb == 60) {
    social_ndb_wt <- 0.9
    cv_ndb_wt<-0.3
    cost_ndb_wt<-0.8
  }
  if(ndb == 70) {
    social_ndb_wt <- 0.7
    cv_ndb_wt<-0.4
    cost_ndb_wt<-0.6
  }
  if(ndb == 80) {
    social_ndb_wt <- 0.3
    cv_ndb_wt<-0.7
    cost_ndb_wt<-0.4
  }
  
  if(ndb == 90) {
    social_ndb_wt <- 0.1
    cv_ndb_wt<-1
    cost_ndb_wt<-0.2
  }
  
  
  if(type=="cost")
  {
    return(cost_ndb_wt)
  }
  else if (type=="cv"){
    return (cv_ndb_wt)
  }else{
    return (social_ndb_wt)
  }
}


choise_grid$cost_Ndb_wt<-mapply(calc.NDB.util,choise_grid$ndb,"cost")

summary(choise_grid$cost_Ndb_wt)


choise_grid$cv_Ndb_wt<-mapply(calc.NDB.util,choise_grid$ndb,"cv")

summary(choise_grid$cv_Ndb_wt)

choise_grid$social_Ndb_wt<-mapply(calc.NDB.util,choise_grid$ndb,"")

summary(choise_grid$social_Ndb_wt)

choise_grid[1,]

calc.training_hours.util <- function(training_hours, type) {
  
 
  
  if(training_hours == 20) {
    social_th_wt <- 1
    cv_th_wt<-0.2
    cost_th_wt<-1
  }
  if(training_hours == 30) {
    social_th_wt <- 1
    cv_th_wt<-0.4
    cost_th_wt<-0.8
  }
  if(training_hours == 40) {
    social_th_wt <- 0.9
    cv_th_wt<-0.6
    cost_th_wt<-0.7
  }
  if(training_hours == 50) {
    social_th_wt <- 0.8
    cv_th_wt<-0.8
    cost_th_wt<-0.6
  }
  
  if(training_hours == 60) {
    social_th_wt <- 0.6
    cv_th_wt<-1
    cost_th_wt<-0.5
  }
  if(training_hours == 70) {
    social_th_wt <- 0.5
    cv_th_wt<-1
    cost_th_wt<-0.4
  }
  if(training_hours == 80) {
    social_th_wt <- 0.3
    cv_th_wt<-1
    cost_th_wt<-0.3
  }
  
  if(training_hours == 90) {
    social_th_wt <- 0.1
    cv_th_wt<-1
    cost_th_wt<-0.1
  }
  
  
  if(type=="cost")
  {
    return(cost_th_wt)
  }
  else if (type=="cv"){
    return (cv_th_wt)
  }else{
    return (social_th_wt)
  }
}


choise_grid$cost_TH_wt<-mapply(calc.training_hours.util,choise_grid$th,"cost")

summary(choise_grid$cost_TH_wt)


choise_grid$cv_TH_wt<-mapply(calc.training_hours.util,choise_grid$th,"cv")

summary(choise_grid$cv_TH_wt)

choise_grid$social_TH_wt<-mapply(calc.training_hours.util,choise_grid$th,"")

summary(choise_grid$social_TH_wt)

choise_grid[1,]



calc.DR.util <- function(drivingrange, type) {
  
  
  
  if(drivingrange == 50) {
    social_dr_wt <- 0.1
    cv_dr_wt<-1
    cost_dr_wt<-0.2
  }
  if(drivingrange == 75) {
    social_dr_wt <- 0.4
    cv_dr_wt<-0.9
    cost_dr_wt<-0.4
  }
  if(drivingrange == 100) {
    social_dr_wt <- 0.7
    cv_dr_wt<-0.8
    cost_dr_wt<-0.6
  }
  if(drivingrange == 125) {
    social_dr_wt <- 1
    cv_dr_wt<-0.5
    cost_dr_wt<-1
  }
  
  if(drivingrange == 150) {
    social_dr_wt <- 1
    cv_dr_wt<-0.2
    cost_dr_wt<-1
  }
  
  
  
  if(type=="cost")
  {
    return(cost_dr_wt)
  }
  else if (type=="cv"){
    return (cv_dr_wt)
  }else{
    return (social_dr_wt)
  }
}

# 
# choise_grid$cost_DR_wt<-mapply(calc.training_hours.util,choise_grid$dr,"cost")
# 
# summary(choise_grid$cost_TH_wt)
# 
# 
# choise_grid$cv_TH_wt<-mapply(calc.training_hours.util,choise_grid$dr,"cv")
# 
# summary(choise_grid$cv_TH_wt)
# 
# choise_grid$social_TH_wt<-mapply(calc.training_hours.util,choise_grid$dr,"")
# 
# summary(choise_grid$social_TH_wt)
# 
# choise_grid[1,]

choise_grid$total_social_util<- (choise_grid$social_ER_wt+choise_grid$social_ES_wt
+choise_grid$social_FT_wt+choise_grid$social_MSRP_wt
+choise_grid$social_Ndb_wt+choise_grid$social_NW_wt
+choise_grid$social_PSG_wt+choise_grid$social_TH_wt
+choise_grid$social_TT_wt+choise_grid$social_SR_wt)


summary(choise_grid$total_social_util)



choise_grid$total_cv_util <- (choise_grid$cv_ER_wt+choise_grid$cv_ES_wt+
  choise_grid$cv_FT_wt+choise_grid$cv_MSRP_wt+
  choise_grid$cv_Ndb_wt+choise_grid$cv_NW_wt+
  choise_grid$cv_PSG_wt+choise_grid$cv_SR_wt+
  choise_grid$cv_TH_wt+choise_grid$cv_TT_wt)


summary(choise_grid$total_cv_util)


choise_grid$total_cost_util <- (choise_grid$cost_ES_wt+choise_grid$cost_ER_wt+
                                choise_grid$cost_FT_wt+choise_grid$cost_MSRP_wt+
                                choise_grid$cost_Ndb_wt+choise_grid$cost_NW_wt+
                                choise_grid$cost_PSG_wt+choise_grid$cost_SR_wt+
                                choise_grid$cost_TH_wt+choise_grid$cost_TT_wt)




choise_grid$combined_util<-
  (choise_grid$social_ER_wt* choise_grid$cv_ER_wt+
     choise_grid$social_ES_wt*choise_grid$cv_ES_wt+
   choise_grid$social_FT_wt* choise_grid$cv_FT_wt+
     choise_grid$social_MSRP_wt*choise_grid$cv_MSRP_wt+
   choise_grid$social_Ndb_wt*choise_grid$cv_Ndb_wt+
     choise_grid$social_NW_wt*choise_grid$cv_NW_wt+
   choise_grid$social_PSG_wt*choise_grid$cv_PSG_wt+
     choise_grid$social_TH_wt*choise_grid$cv_SR_wt+
   choise_grid$social_TT_wt* choise_grid$cv_TH_wt+
     choise_grid$social_SR_wt*choise_grid$cv_TT_wt)
 


###########################################PLOTTTING ONLY###############

hist(choise_grid$total_social_util)
hist(choise_grid$total_cv_util)

hist(choise_grid$cv_ES_wt)

hist(choise_grid$combined_util)

summary(choise_grid)
dim(choise_grid)

ggplot(choise_grid, aes(x=combined_util, y=total_cost_util)) + geom_point()

ggplot(choise_grid, aes(x=total_cost_util, y=combined_util)) + geom_point()



gplot(choise_grid, aes(x=total_cv_util, y=total_cost_util)) + geom_point()

ggplot(choise_grid, aes(x=total_social_util, y=total_cost_util)) + geom_point()

ggplot(choise_grid, aes(total_cost_util, combined_util,color=sr)) +
  geom_point(aes(shape=es),alpha =1)

dim(choise_grid)
choise_3D<-choise_grid[1:50,]
dim(choise_3D)
# choise_3D

mycolors <- as.numeric(choise_grid$es)

sd(choise_3D$total_cost_util)

s3d<-scatterplot3d(choise_3D$total_cost_util,choise_3D$total_cv_util,
                   choise_3D$total_social_util,
      color = c("red","green","blue")[choise_3D$es],angle =20,grid=TRUE)

dev.off()


# scatterplot3d(choise_grid$total_cost_util,choise_grid$total_cv_util,
#               choise_grid$total_social_util, pch = 19, color="steelblue")





x <-  choise_grid$total_cost_util
y <- choise_grid$total_cv_util
z <- choise_grid$total_social_util

scatter3D(x, y, z, clab = c("Social acceptance ", " Utility"),phi = 0, bty ="g",ticktype = "detailed")



######################good one

#install.packages("rgl")
library(rgl)
library(magick)
rgl.open() 
rgl.bg(color = "lightgray") 

# with (choise_grid, plot3d(x=total_cv_util,y=total_cost_util,z=total_social_util,
#                           xlab="X", ylab="Y", zlab="Z", col=c(1,2,3)[as.numeric(choise_grid$sr)]))

plot3d(choise_grid$total_cv_util,choise_grid$total_cost_util,choise_grid$total_social_util,
       xlab="X", ylab="Y", zlab="Z", col=c(2,3,4)[as.numeric(choise_grid$sr)])

legend3d("topright", col=1:3,legend = levels(choise_grid$sr))

#rgl.bbox(color = "#333377") 

# 
# decorate3d( xlab = "x", ylab = "y", zlab = "z", 
#             box = TRUE, axes = TRUE, main = NULL, sub = NULL,
#             top = TRUE, aspect = FALSE, expand = 1)


#rgl.clear()


movie3d(spin3d(axis = c(0, 0, 1)), duration = 3,dir = getwd())
###########################


x <-choise_3D$total_cost_util
y<-choise_3D$total_cv_util
z<-choise_3D$total_social_util




########################ANOTHER ONE TRY works fine
x <-  choise_grid$total_cost_util
y <- choise_grid$total_cv_util
z <- choise_grid$total_social_util


rgl.open() # Open a new RGL device
rgl.points(x, y, z, color ="lightgray") # Scatter plot
decorate3d( xlab = "x", ylab = "y", zlab = "z", 
           box = TRUE, axes = TRUE, main = NULL, sub = NULL,
           top = TRUE, aspect = FALSE, expand = 1)
rgl.clear()
#movie3d(spin3d(axis = c(0, 0, 1)), duration = 5, dir = getwd())

######################PLOTLY 3D
p1 <- plot_ly(x = choise_3D$total_cost_util, y = choise_3D$total_cv_util,
             z = choise_3D$total_social_util,type ="scatter3d")
p1
chart_link = api_create(p, filename="surface-2")
chart_link
dev.off()
p1<-NULL
p1

#######################
p <- plot_ly(choise_grid, x = ~total_cost_util, y = ~total_cv_util,
             z = ~total_social_util, color = ~nw, colors = c('#BF382A', '#0C4B8E')) %>% 
  add_markers() %>%
  
layout(scene = list(xaxis = list(title = 'total cost util'),
                      yaxis = list(title = 'total cv util'),
                      zaxis = list(title = 'total social util')))


############################

p <- plot_ly(choise_grid, x = ~total_cost_util, y = ~total_cv_util, z = ~total_social_util,
             marker = list(color = ~psg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'total cost util'),
                      yaxis = list(title = 'total cv util'),
                      zaxis = list(title = 'total social util')),
         annotations = list(
           x = 1.0,
           y = 1.0,
           text = 'passengers',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p
p <- plot_ly(choise_3D, x = ~total_cost_util, y = ~total_cv_util, z = ~total_social_util,
             marker = list(color = ~psg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'total cost util'),
                      yaxis = list(title = 'total cv util'),
                      zaxis = list(title = 'total social util')),
         annotations = list(
           x = 1.0,
           y = 1.0,
           text = 'passengers',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p
