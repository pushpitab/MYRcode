a <- c("ABC", "DEF", "GHI")
b <- c("2012-05-01", "2012-05-02", "2012-05-03", "2012-05-04", "2012-05-05")
expand.grid(a,b)
setwd("C:/Users/Pushpita/Documents/R")



mission <- c("Taxi","Medical","Commuter",	"Cargo",	"Recreation")


power <- c("FossilFuel",	"Electric",	"Hybrid")

mat <- expand.grid(mission,power)

colnames(mat) <- c("mission","power")
mat



calc.power.wt <- function(power) {
  #print(power)
  
  if(power == "Electric") {
    power_wt <- 1
    
  }
  if(power == "Hybrid") {
    power_wt <- 0.5
  }
  if(power == "FossilFuel") {
    power_wt <- 0.2
  }
  power_wt # return value 
}

mat$wt <- mapply(calc.power.wt,mat$power)


mat

################Medical###########

power_wt <- c(0.2, 1, 0.5)
power_cost <-c(100, 200 , 300 )

psg <- c(1,	2	,3,	4,	6,	8)

psg_wt <- c(0.1,0.4,0.7,1)

#psg_cost <- c(10 , 30 , 50)
#psg=read.csv("psg.csv",header=TRUE,check.names=FALSE)
#psg
mat1 <- expand.grid(power_wt,psg_wt)
mat1

colnames(mat1) <- c("power_wt","psg_wt")
mat1




###################################


pilot <- c("Autonomous"	,"Piloted"	,"Remote")

pilot_wt <- c(1,0.5,0.2)

#remote costs more
pilot_cost <- c(200,100,150)


range <- c(10,	30,	60,	90,	120,	150,	180)

range_wt<-c(0, 0.1, 0.5, 1)


range_cost <-c(100,300,400,500)


m2 <- expand.grid(mission,power,psg, pilot,range)

dim(m2)

toffdist <- c(0,	5,	10,	40,	150,	300,	800,	1800)

toffdist_wt<-c( 1, 0.5, 0)

toffdist_cost <-c(500,250,100)

m3 <- expand.grid(mission,power,psg, pilot,range,toffdist)
dim(m3)

recharge_time <- c (10,30,45,60,360)

recharge_time_wt<-c(1,0.5,0)
recharge_time_cost <- c(500,400,200)

m4 <- expand.grid(mission,power,psg, pilot,range,toffdist,refuel)
dim(m4)
airspeed <- c(10,	20,	40,	80,	100,	150 ,	200)

airspeed_wt<-c(0, 0.1, 0.5, 1)
airspeed_cost<- c(100,200,400,500)

m5 <- expand.grid(mission,power,psg, pilot,range,toffdist,refuel,airspeed)
dim(m5)

grdspeed <- c(10,	20,	40,	80,	120)

grdspeed_wt<-c(0, 0.1, 0.5, 1)

grdspeed_cost<- c(10,20,40,50)


#################################Medical Mission####################

m6 <- expand.grid(power,psg, pilot,range,toffdist,recharge_time,airspeed,grdspeed)
dim(m6)


colnames(m6) <- c("power","psg","pilot","range","distance","recharge_time","airspeed","grdspeed")

#m6<- m6[!(m6$mission=="Recreation" & m6$psg>4),]

#m7<-subset(m6, m6$mission=="Recreation")

#dim(m7)
#head(m7)

calc.power.wt <- function(power) {
  
  if(power == "Electric") {
    power_wt <- 1
  }
  if(power == "Hybrid") {
    power_wt <- 0.5
  }
  if(power == "FossilFuel") {
    power_wt <- 0.2
  }
  power_wt   # return value 
}
m6$power_wt <- mapply(calc.power.wt,m6$power)
dim(m6)
summary(m6$power_wt)


calc.psg.wt <- function(psg) {
  
  if(psg == "Electric") {
    power_wt <- 1
  }
  if(power == "Hybrid") {
    power_wt <- 0.5
  }
  if(power == "FossilFuel") {
    power_wt <- 0.2
  }
  power_wt   # return value 
}



##################################################################

weight_matrix<-expand.grid(power_wt,psg_wt,pilot_wt,range_wt,toffdist_wt,recharge_time_wt,
                           airspeed_wt,grdspeed_wt)
colnames(weight_matrix) <- c("power_wt","psg_wt","pilot_wt","range_wt",
                             "toffdist_wt","recharge_time_wt","airspeed_wt","grdspeed_wt")

dim(weight_matrix)
head(weight_matrix)
#weight_matrix<- weight_matrix[!(weight_matrix$power_wt==0.2),]
#dim(weight_matrix)
#head(weight_matrix)

weight_matrix$utility <- (weight_matrix$power_wt *7+
                            weight_matrix$psg_wt*9+
                          weight_matrix$pilot_wt*6+
                          weight_matrix$range_wt*5+
                            weight_matrix$recharge_time_wt*2+
                            weight_matrix$toffdist_wt*4+
                              weight_matrix$airspeed_wt*3+
                            weight_matrix$grdspeed_wt*1)/45
head(weight_matrix)
dim(weight_matrix)
summary(weight_matrix$utility)




calculate.cost.power <- function(arg1) {
  
  if(arg1 == 0.2) {
    cost <- 100
  }
  if(arg1 == 1) {
    cost <- 200
  }
  if(arg1 == 0.5) {
    cost <- 300
  }
  
  cost   # return value 
}

calculate.cost.pilot <- function(arg1) {
  
  if(arg1 == 0.2) {
    cost1 <- 100
  }
  if(arg1 == 1) {
    cost1 <- 200
  }
  if(arg1 == 0.5) {
    cost1 <- 300
  }
  
  cost1   # return value 
}


calculate.cost.tdistance <- function(arg1) {
  
  if(arg1 == 0) {
    cost2 <- 100
  }
  if(arg1 == 1) {
    cost2 <- 500
  }
  if(arg1 == 0.5) {
    cost2 <- 300
  }
  
  cost2   # return value 
}

weight_matrix$cost <-(calculate.cost.power(weight_matrix$power_wt)*8+ 
                         weight_matrix$psg_wt*100*6+ 
                         calculate.cost.pilot(weight_matrix$pilot_wt)*5+
                         weight_matrix$range_wt*100*7+ 
                         weight_matrix$recharge_time_wt*100*3+ 
                         calculate.cost.tdistance(weight_matrix$toffdist_wt)*4+
                         weight_matrix$airspeed_wt*100*2+
                         weight_matrix$grdspeed_wt*100*1)


head(weight_matrix)
dim(weight_matrix)
summary(weight_matrix$cost)

weight_matrix11<- subset(weight_matrix, weight_matrix$range_wt > 0)

weight_matrix11<- subset(weight_matrix11, weight_matrix11$recharge_time_wt > 0)
weight_matrix11<- subset(weight_matrix11, weight_matrix11$airspeed_wt > 0)
weight_matrix11<- subset(weight_matrix11, weight_matrix11$grdspeed_wt > 0)
dim(weight_matrix11)

tail(weight_matrix1)
summary(weight_matrix1$utility)
summary(weight_matrix1$cost)

par(mar=c(1,1,1,1))
#plot(weight_matrix1$utility,weight_matrix1$cost)

library(ggplot2)

ggplot(weight_matrix1, aes(x=utility, y=cost)) + geom_point()
ggplot(weight_matrix1, aes(x=cost, y=utility)) + geom_point()+ geom_text(label=rownames(weight_matrix1))

weight_matrix2<-subset(weight_matrix1, weight_matrix1$utility > 0.67)
weight_matrix2<-subset(weight_matrix1, weight_matrix1$utility > 0.67)

dim(weight_matrix2)
weight_matrix2

write.csv(weight_matrix2, file = "weight_matrix2.csv")

############### for fossil fuel and TAXI


weight_matrix_ff<-expand.grid(psg_wt,pilot_wt,range_wt,toffdist_wt,
                           airspeed_wt,grdspeed_wt)
colnames(weight_matrix_ff) <- c("psg_wt","pilot_wt","range_wt",
                             "toffdist_wt","airspeed_wt","grdspeed_wt")

dim(weight_matrix_ff)
head(weight_matrix_ff)
weight_matrix_ff

weight_matrix_ff$utility <- 0.2*weight_matrix_ff$psg_wt* 
                            weight_matrix_ff$pilot_wt*weight_matrix_ff$range_wt *
                              weight_matrix_ff$toffdist_wt*weight_matrix_ff$airspeed_wt*
                              weight_matrix_ff$grdspeed_wt*100
head(weight_matrix_ff)
dim(weight_matrix_ff)
summary(weight_matrix_ff$utility)



weight_matrix_ff$cost <-( 20+ weight_matrix_ff$psg_wt* 10+
                            weight_matrix_ff$pilot_wt*100+
                            weight_matrix_ff$range_wt * 10+ weight_matrix_ff$toffdist_wt*100+
                            weight_matrix_ff$airspeed_wt* 20 +weight_matrix_ff$grdspeed_wt*10)

head(weight_matrix_ff)
dim(weight_matrix_ff)
summary(weight_matrix_ff$cost)

weight_matrix_ff<- weight_matrix_ff[!(weight_matrix_ff$utility==0),]

summary(weight_matrix_ff$utility)
summary(weight_matrix_ff$cost)

par(mar=c(1,1,1,1))


library(ggplot2)

ggplot(weight_matrix_ff, aes(x=utility, y=cost)) + geom_point()
ggplot(weight_matrix_ff, aes(x=cost, y=utility)) + geom_point()
