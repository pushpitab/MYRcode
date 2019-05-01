setwd("C:/Users/Pushpita/Documents/R")
install.packages("ggplot")
library(ggplot2)

#rm(m6)
mission <- c("Taxi","Medical","Commuter",	"Cargo",	"Recreation")
power <- c("FossilFuel",	"Electric",	"Hybrid")
psg <- c(1,	2	,3,	4, 6,	8)
pilot <- c("Autonomous"	,"Piloted"	,"Remote")
range <- c(10,	30,	60,	90,	120,	150,	180)
toffdist <- c(0,	5,	10,	40,	150,	300,	800,	1800)
recharge_time <- c (5,10,30,45,60,120,360)
airspeed <- c(10,	20,	40,	80,	100,	150 ,	200)
grdspeed <- c(10,	20,	40,	80,	120)


m6 <- expand.grid(power,psg, pilot,range,toffdist,recharge_time,airspeed,grdspeed)
dim(m6)


colnames(m6) <- c("power","psg","pilot","range","distance","recharge_time","airspeed","grdspeed")

calc.power.wt <- function(power) {
  
  if(power == "Electric") {
    power_wt <- 1
  }
  if(power == "Hybrid") {
    power_wt <- 0.3
  }
  if(power == "FossilFuel") {
    power_wt <- 0.5
  }
  power_wt   # return value 
}

m6$power_wt <- mapply(calc.power.wt,m6$power)
dim(m6)
summary(m6$power_wt)

calc.power.cost <- function(power) {
  
  if(power == "Electric") {
    power_cost <- 2
  }
  if(power == "Hybrid") {
    power_cost <- 2.5
  }
  if(power == "FossilFuel") {
    power_cost <- 1
  }
  power_cost   # return value 
}

m6$power_cost <- mapply(calc.power.cost,m6$power)
dim(m6)
summary(m6$power_cost)

calc.psg.wt <- function(psg) {
  
  if(psg == 1) {
    psg_wt <- 0.2
  }
  if( psg > 1 & psg < 5) {
    psg_wt <- 0.45
  }
  if( psg > 5 & psg < 8) {
    psg_wt <- 0.7
  }
  if(psg >= 8) {
    psg_wt <- 1
  }
  psg_wt   # return value 
}



summary(m6$psg)
psg_wt<-0;
m6$psg_wt <- mapply(calc.psg.wt, m6$psg)
dim(m6)
summary(m6$psg_wt)


calc.psg.cost <- function(psg) {
  
  if(psg == 1) {
    psg_cost <- 1
  }
  if( psg > 1 & psg < 4) {
    psg_cost <- 2
  }
  if( psg > 3 & psg < 6) {
    psg_cost <- 3
  }
  if( psg > 5 & psg < 8) {
    psg_cost <- 4
  }
  if(psg >= 8) {
    psg_cost <- 6
  }
  psg_cost   # return value 
}

m6$psg_cost <- mapply(calc.psg.cost, m6$psg)
dim(m6)
summary(m6$psg_cost)

calc.pilot.wt <- function(pilot) {
  
  if(pilot == "Autonomous") {
    pilot_wt <- 1
  }
  if(pilot == "Piloted") {
    pilot_wt <- 0.5
  }
  if(pilot == "Remote") {
    pilot_wt <- 0.3
  }
  pilot_wt   # return value 
}

m6$pilot_wt <- mapply(calc.pilot.wt, m6$pilot)
dim(m6)
summary(m6$pilot_wt)


calc.pilot.cost <- function(pilot) {
  
  if(pilot == "Autonomous") {
    pilot_cost <- 3
  }
  if(pilot == "Piloted") {
    pilot_cost <- 1
  }
  if(pilot == "Remote") {
    pilot_cost <- 2
  }
  pilot_cost   # return value 
}

m6$pilot_cost <- mapply(calc.pilot.cost, m6$pilot)
dim(m6)
summary(m6$pilot_cost)



calc.range.wt <- function(range) {
  
  if(range == 10) {
    range_wt <- 0.2
  }
  if( range > 10 & range < 50) {
    range_wt <- 0.5
  }
  if( range >= 50 & range < 90) {
    range_wt <- 0.8
  }
  if(range >= 90) {
    range_wt <- 1
  }
  range_wt   # return value 
}


m6$range_wt <- mapply(calc.range.wt, m6$range)
dim(m6)
summary(m6$range_wt)



calc.range.cost <- function(range) {
  
  if(range == 10) {
    range_cost <- 1
  }
  if( range > 10 & range < 50) {
    range_cost <- 2
  }
  if( range >= 40 & range < 90) {
    range_cost <- 3
  }
  if(range >= 90 & range <150) {
    range_cost <- 4
  }
  if(range >= 150) {
    range_cost <- 5
  }
  range_cost   # return value 
}

m6$range_cost <- mapply(calc.range.cost, m6$range)
dim(m6)
summary(m6$range_cost)




calc.toffdist.wt <- function(toffdist) {
  
  if(toffdist == 0) {
    toffdist_wt <- 1
  }
  if( toffdist > 0 & toffdist < 6) {
    toffdist_wt <- 0.7
  }
  if( toffdist >= 6 & toffdist < 10) {
    toffdist_wt <- 0.4
  }
  if( toffdist >= 10 & toffdist < 40) {
    toffdist_wt <- 0.2
  }
  if(toffdist >= 40) {
    toffdist_wt <- 0
  }
  toffdist_wt   # return value 
}

m6$toffdist_wt <- mapply(calc.toffdist.wt, m6$distance)
dim(m6)
summary(m6$toffdist_wt)

calc.toffdist.cost <- function(toffdist) {
  
  if(toffdist == 0) {
    toffdist_cost <- 8
  }
  if( toffdist > 0 & toffdist < 6) {
    toffdist_cost <- 7
  }
  if( toffdist >= 6 & toffdist < 10) {
    toffdist_cost <- 6
  }
  if( toffdist >= 10 & toffdist < 40) {
    toffdist_cost <- 5
  }
  if( toffdist >= 40 & toffdist < 300) {
    toffdist_cost <- 4
  }
  if(toffdist >= 300) {
    toffdist_cost <- 3
  }
  toffdist_cost   # return value 
}

m6$toffdist_cost <- mapply(calc.toffdist.cost, m6$distance)
dim(m6)
summary(m6$toffdist_cost)



calc.recharge_time.wt <- function(recharge_time, power) {
  
  
  if(power=="FossilFuel"){
   recharge_time_wt<-1
       
   }else{
    
    if(recharge_time == 5) {
      recharge_time_wt <- 1
    }
    if( recharge_time > 5 & recharge_time < 30) {
      recharge_time_wt <- 0.7
    }
    if( recharge_time >= 30 & recharge_time < 90) {
      recharge_time_wt <- 0.4
    }
    if( recharge_time >= 90 & recharge_time < 120) {
      recharge_time_wt <- 0.2
    }
    if(recharge_time >= 120) {
      recharge_time_wt <- 0
    }
    
  
   }
  recharge_time_wt   # return value 
}

summary(m6$recharge_time)
m6$recharge_time_wt <- mapply(calc.recharge_time.wt, m6$recharge_time, m6$power)
dim(m6)
summary(m6$recharge_time_wt)




calc.recharge_cost <- function(recharge_time, power) {
  
  
  if(power=="FossilFuel"){
    recharge_cost<-0
    
  }else{
    
    if(recharge_time == 5) {
      recharge_cost <- 7
    }
    if( recharge_time > 5 & recharge_time < 30) {
      recharge_cost <- 6
    }
    if( recharge_time >= 30 & recharge_time < 90) {
      recharge_cost <- 5
    }
    if( recharge_time >= 90 & recharge_time < 120) {
      recharge_cost <- 4
    }
    if(recharge_time >= 120) {
      recharge_cost <-3
    }
    
    
  }
  recharge_cost   # return value 
}

m6$recharge_cost <- mapply(calc.recharge_cost, m6$recharge_time, m6$power)
dim(m6)
summary(m6$recharge_cost)




calc.airspeed.wt <- function(airspeed) {
  
  if(airspeed == 10) {
    airspeed_wt <- 0
  }
  if( airspeed > 10 & airspeed <= 20) {
    airspeed_wt <- 0.2
  }
  if( airspeed > 20 & airspeed <= 40) {
    airspeed_wt <- 0.4
  }
  if( airspeed > 40 & airspeed <= 80) {
    airspeed_wt <- 0.6
  }
  if( airspeed > 80 & airspeed <= 100) {
    airspeed_wt <- 0.8
  }
  if(airspeed > 100) {
    airspeed_wt <- 1
  }
  airspeed_wt   # return value 
}


m6$airspeed_wt <- mapply(calc.airspeed.wt, m6$airspeed)
dim(m6)
summary(m6$airspeed_wt)




calc.airspeed.cost <- function(airspeed) {
  
  if(airspeed == 10) {
    airspeed_cost <- 1
  }
  if( airspeed > 10 & airspeed <= 20) {
    airspeed_cost <- 1
  }
  if( airspeed > 20 & airspeed <= 40) {
    airspeed_cost <- 3
  }
  if( airspeed > 40 & airspeed <= 80) {
    airspeed_cost <- 4
  }
  if( airspeed > 80 & airspeed <= 100) {
    airspeed_cost <- 5
  }
  if(airspeed > 100) {
    airspeed_cost <- 6
  }
  airspeed_cost   # return value 
}
m6$airspeed_cost <- mapply(calc.airspeed.cost, m6$airspeed)
dim(m6)
summary(m6$airspeed_cost)



calc.grdspeed.wt <- function(grdspeed) {
  
  if(grdspeed == 10) {
    grdspeed_wt <- 0
  }
  if( grdspeed > 10 & grdspeed <= 20) {
    grdspeed_wt <- 0.2
  }
 
  if( grdspeed > 20 & grdspeed <= 40) {
    grdspeed_wt <- 0.4
  }
  if( grdspeed > 40 & grdspeed <= 80) {
    grdspeed_wt <- 0.6
  }
  if( grdspeed > 80 & grdspeed <= 100) {
    grdspeed_wt <- 0.8
  }
  if(grdspeed > 100) {
    grdspeed_wt <- 1
  }
  grdspeed_wt   # return value 
}


m6$grdspeed_wt <- mapply(calc.grdspeed.wt, m6$grdspeed)
dim(m6)
summary(m6$grdspeed_wt)




calc.grdspeed.cost <- function(grdspeed) {
  
  if(grdspeed == 10) {
    grdspeed_cost <- 1
  }
  if( grdspeed > 10 & grdspeed <= 20) {
    grdspeed_cost <- 2
  }
  
  if( grdspeed > 20 & grdspeed <= 40) {
    grdspeed_cost <- 3
  }
  if( grdspeed > 40 & grdspeed <= 80) {
    grdspeed_cost <- 4
  }
  if( grdspeed > 80 & grdspeed <= 100) {
    grdspeed_cost <- 5
  }
  if(grdspeed > 100) {
    grdspeed_cost <- 6
  }
  grdspeed_cost   # return value 
}

m6$grdspeed_cost <- mapply(calc.grdspeed.cost, m6$grdspeed)
dim(m6)
summary(m6$grdspeed_cost)


weight_matrix<-m6
weight_matrix$utility <- (weight_matrix$power_wt *7+
                            weight_matrix$psg_wt*9+
                            weight_matrix$pilot_wt*6+
                            weight_matrix$range_wt*5+
                            weight_matrix$recharge_time_wt*2+
                            weight_matrix$toffdist_wt*4+
                            weight_matrix$airspeed_wt*3+
                            weight_matrix$grdspeed_wt*1)/45

dim(weight_matrix)
summary(weight_matrix$utility)


weight_matrix$cost <- (weight_matrix$power_cost *8+
                            weight_matrix$psg_cost*6+
                            weight_matrix$pilot_cost*5+
                            weight_matrix$range_cost*7+
                            weight_matrix$recharge_cost*3+
                            weight_matrix$toffdist_cost*4+
                            weight_matrix$airspeed_cost*2+
                            weight_matrix$grdspeed_cost*1)/181

dim(weight_matrix)
summary(weight_matrix$cost)
new_matrix<- rbind(weight_matrix[1,],weight_matrix[2,])

write.csv(new_matrix, file = "weight_matrix1.csv",append = TRUE)

ggplot(weight_matrix, aes(utility, cost,color=power)) +
  geom_point(alpha = 0.5, stat = "unique")

ggplot(weight_matrix, aes(utility, cost,color=pilot)) +
  geom_point(aes(shape=power),alpha = 0.5, stat = "unique")


rm(weight_matrix2)
weight_matrix2<-subset(weight_matrix, weight_matrix$utility > 0.6)
dim(weight_matrix2)
weight_matrix3<-subset(weight_matrix2, weight_matrix2$cost<0.8)
dim(weight_matrix3)
write.csv(weight_matrix3, file = "weight_matrix_cost_util.csv")


weight_matrix4<-subset(weight_matrix, weight_matrix$utility> 0.7)
dim(weight_matrix4)

ggplot(weight_matrix4, aes(utility, cost,color=pilot)) +
  geom_point(aes(shape=power),alpha =1)



write.csv(weight_matrix4, file = "weight_matrix4_util_0.7.csv")




