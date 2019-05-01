setwd("C:/Users/Pushpita/Documents/R")
# install.packages("ggplot")
# # install.packages("plotly")
# 
# library(plotly)
library(ggplot2)

parameters=read.csv("parameter.csv",header=TRUE,check.names=FALSE)
parameters
parameters <- na.omit(parameters)


consequenceTable=read.csv("consequenceTable.csv",header=TRUE,check.names=FALSE)
consequenceTable
dim(consequenceTable)

pilot <- c(1,	2)
psg <- c(1,2,3)
patients<-c(0,1,2)
medical_load<-c(0,50,100,150,300,600)
battery_load<-c(0,150,300,600,1200)
fuel_capacity<-c(0,50,100,150,300)
no_rotor <-c(1,2,4,6,8,12)
max_velocity<-c(20,40,80,100,120)

choise_grid<-expand.grid(pilot,psg,patients ,medical_load,battery_load,
                         fuel_capacity,no_rotor,max_velocity)

colnames(choise_grid) <- c("pilot","psg","patients","medical_load",
                           "battery_load","fuel_capacity","no_rotor","max_velocity")
dim(choise_grid)


choise_grid1<-data.frame(0,0,0,0,0,0,0,0,0)
colnames(choise_grid1) <- c("pilot","psg","patients","medical_load","battery_load","fuel_capacity","no_rotor","max_velocity")

calc.consequence <- function(choise_grid1,consequenceTable) {
  
  consequenceTable$Passenger_Mass<- parameters$Person_Mass*(choise_grid1$pilot+
                                                              choise_grid1$psg+
                                                              choise_grid1$patients)
  #print(consequenceTable$Passenger_Mass)
  
  if(choise_grid1$pilot==1)
  {
    consequenceTable$Width=parameters$Width_w_1_pilot
    
  }
  if(choise_grid1$pilot==2)
  {
    consequenceTable$Width=parameters$Width_w_2_pilots 
    
  }
  if(choise_grid1$patients==0)
  {
    consequenceTable$Length=parameters$Length_w_out_patient
    
  }
  if(choise_grid1$patients>0)
  {
    consequenceTable$Length=parameters$Length_w_patient
    
  }
 
  consequenceTable$Height<- parameters$Height

  
  consequenceTable$Frontal_Area<- consequenceTable$Width*consequenceTable$Height
  
  consequenceTable$Force_Drag <-  consequenceTable$Frontal_Area*parameters$Cd*
    choise_grid1$max_velocity*choise_grid1$max_velocity
  
  consequenceTable$Frame_Mass=parameters$Airframe_base_weight
  
  # if(consequenceTable$Width==2)
  # {
  #   consequenceTable$Frame_Mass=consequenceTable$Frame_Mass*consequenceTable$Width*
  #     consequenceTable$Length*parameters$Airframe_Density
  # }
  # if(consequenceTable$Length==6)
  # {
  
  #### new frame mass calculation
    consequenceTable$Frame_Mass=consequenceTable$Width*consequenceTable$Length*parameters$Airframe_Density
  
  
  consequenceTable$Electrical_Component_mass= (choise_grid1$no_rotor*(parameters$Motor_Mass+parameters$Inverter_Mass)
                                               +choise_grid1$battery_load*parameters$Battery_Shielding_Ratio)
                                   
  
  
 
  
 if(choise_grid1$fuel_capacity>0){
  
   consequenceTable$Gas_Component_Mass<-((choise_grid1$fuel_capacity*
                                            parameters$Gas_Mass_Density*
                                            (1+parameters$Gas_Tank_Mass_ratio))+
                                           parameters$Generator_Mass+parameters$Gas_Turbine_Mass)
 }
  
 
  
   consequenceTable$Energy_Mass=consequenceTable$Electrical_Component_mass+
                                  consequenceTable$Gas_Component_Mass
  
  consequenceTable$Electrical_Energy1<-choise_grid1$battery_load*parameters$Battery_Energy_Density
  
  
  consequenceTable$Electrical_Energy2<-consequenceTable$Electrical_Energy1*0.277778
  
  
  consequenceTable$Gas_Energy <- choise_grid1$fuel_capacity*parameters$Gas_Energy_Density
  
  consequenceTable$Total_Energy=consequenceTable$Electrical_Energy1+consequenceTable$Gas_Energy
  
  #if(consequenceTable$Electrical_Energy1>0){
    
    consequenceTable$Obtainable_Energy<- (consequenceTable$Gas_Energy*parameters$Gas_to_elect_eff+consequenceTable$Electrical_Energy1*parameters$Electric_to_kinetic_eff)
    
  #   
  # }else{
  #   consequenceTable$Obtainable_Energy<- consequenceTable$Gas_Energy*parameters$Electric_to_kinetic_eff
  # }
  ## added drive train mass
  consequenceTable$Drivetrain_Mass <- 0.2*(consequenceTable$Frame_Mass+consequenceTable$Passenger_Mass+consequenceTable$Energy_Mass)
  
  #######################
  
  consequenceTable$Battery_Volume <- consequenceTable$Electrical_Energy1/parameters$Battery_Volume_Density
  
  consequenceTable$Load_Area<-consequenceTable$Length*consequenceTable$Width*parameters$Usable_load_area_reduction
 
  consequenceTable$Battery_Pack_Height<-consequenceTable$Battery_Volume/consequenceTable$Load_Area
  ###############################
   consequenceTable$Total_Mass=(choise_grid1$medical_load+consequenceTable$Frame_Mass+
                              consequenceTable$Energy_Mass+consequenceTable$Passenger_Mass+
                                consequenceTable$Drivetrain_Mass)
  

  
  consequenceTable$Force_Hover<- consequenceTable$Total_Mass*9.8
  
  consequenceTable$Force_Movement<- sqrt(consequenceTable$Force_Drag^2+consequenceTable$Force_Hover^2)
  
  consequenceTable$Force_Generated<-((consequenceTable$Force_Movement)/(parameters$Rotor_Eff))
  
  #######################new  ones #############################
  consequenceTable$Climb_Power_needed<-(((consequenceTable$Force_Hover*parameters$Climb_height)/(60*parameters$Climb_rate))/1000)/parameters$Rotor_Eff
  
  consequenceTable$Power_rating_per_rotor<- consequenceTable$Climb_Power_needed/choise_grid1$no_rotor
  

  
  if(consequenceTable$Electrical_Component_mass>0){
    consequenceTable$Electric_Motor_Mass <-consequenceTable$Power_rating_per_rotor/parameters$Motor_Mass
  }
  

  
  consequenceTable$Spec_Range<- ((consequenceTable$Obtainable_Energy*1000000)/consequenceTable$Force_Generated)/1000
  
  
  
  if(choise_grid$no_rotor==1){
    consequenceTable$Risk_score<-1
    consequenceTable$Maint_Score<-1
    
  }
  if(choise_grid$no_rotor==2){
    consequenceTable$Risk_score<-1
    consequenceTable$Maint_Score<-0.9
    
  }

  if(choise_grid$no_rotor==4){
    consequenceTable$Risk_score<-0.9
    consequenceTable$Maint_Score<-0.6
    
  }
  if(choise_grid$no_rotor==6){
    consequenceTable$Risk_score<-0.8
    consequenceTable$Maint_Score<-0.6
    
  }
  if(choise_grid$no_rotor==8){
    consequenceTable$Risk_score<-0.6
    consequenceTable$Maint_Score<-0.6
    
  }
  if(choise_grid$no_rotor==12){
    consequenceTable$Risk_score<-0.25
    consequenceTable$Maint_Score<-0.8
    
  }
  
  if(choise_grid$battery_load >0 && choise_grid$fuel_capacity>0){
    consequenceTable$Nuisance<-0.6
  }else if(choise_grid$battery_load >0){
    consequenceTable$Nuisance<-0.4
  }else{
    consequenceTable$Nuisance<-0.1
  }
  
  
  consequenceTable
}
#choise_grid[2161,]
#new_consequence2 <-calc.consequence(choise_grid[2161,],consequenceTable)
#new_consequence2

new_consequence1<-consequenceTable
new_consequence1
output<-cbind(choise_grid[1,],new_consequence1[1,])


pb <- txtProgressBar(min = 0, max = 81000, style = 3)
 

for (i in 1:5) {
 
   choise_grid1<-choise_grid[i,]
 
   new_consequence1[i,]<- calc.consequence(choise_grid1,consequenceTable)

  output[i,]<-cbind(choise_grid[i,],new_consequence1[i,])
  
  setTxtProgressBar(pb, i)
}

dim(output)


########################plotting
# 
# ggplot(output, aes(Spec_Range, Total_Mass, color=battery_load)) +
#   geom_point(alpha = 0.5, stat = "unique")
# 
# ggplot(output, aes(Total_Mass, Spec_Range, color=battery_load)) +
#   geom_point(alpha = 0.5, stat = "unique")

############################### Calulate cost####

dim(output)


output$Frame_Cost<-output$Frame_Mass*parameters$Cost_per_kg_of_carbon

output$Battery_Cost<- output$Electrical_Energy2*parameters$Cost_per_kWh_of_batteries

output$gas_turbine_cost<-  output$Climb_Power_needed*parameters$Cost_per_kw_of_generator


output$Motors_Inverter_Cost<-0

if(output$Electrical_Energy1>0)
{
  output$Motors_Inverter_Cost <- output$Power_rating_per_rotor*parameters$Cost_per_kW_of_motors*output$no_rotor
}

output$DriveTrain_Cost <-output$Drivetrain_Mass*20

output$control_sensor_Cost <- 20000
output$Component_Cost<- (output$Frame_Cost+output$Battery_Cost+output$gas_turbine_cost+
                           output$Motors_Inverter_Cost+output$DriveTrain_Cost)

######################

output$Maint_hours<-0
output$Electric_Maint_hours<-0
output$Gas_Maint_hours<-0


if(output$battery_load>0)
{
  output$Electric_Maint_hours<-parameters$Elect_hours_per_year_maint
}
if(output$fuel_capacity>0)
{
  output$Gas_Maint_hours<-parameters$Gas_hours_per_year_maint
}


output$Maint_hours<-(output$Electric_Maint_hours+output$Gas_Maint_hours)


output_Maint_Cost<-output$Maint_hours*parameters$Maint_Cost_per_hour

output$fuel_use_perday<-200/output$Spec_Range

output$cost0fElec_peryear<- (output$Electrical_Energy2*parameters$Cost_of_electricity*output$fuel_use_perday*365)
output$costOfFuel_peryear <- ((output$fuel_capacity*parameters$Cost_of_fuel_lit)*(output$fuel_use_perday*365))

output$costOfPilot<-3*(output$pilot*parameters$Pilot_salary)



output$TotalcostOfMaintenance <- (output$cost0fElec_peryear+output$costOfFuel_peryear+output$costOfPilot)

#########################################3
output$Total_Cost<- output$TotalcostOfMaintenance+output$Component_Cost



output$OverallCost<-output$Total_Cost+5*output$TotalcostOfMaintenance

output$SocialCost<-((output$OverallCost*output$Risk_score)*(output$Nuisance*output$Maint_Score))

output

write.csv(output,file="output_new1.csv")

# output1=read.csv("output_new.csv",header=TRUE,check.names=FALSE)
# 
# dim(output1)
# 
# summary(output1$Frame_Mass)
# summary(output1$Total_Mass)
# ggplot(output1, aes(Spec_Range, Total_Mass, color=battery_load)) +
#   geom_point(alpha = 0.5, stat = "unique")
# 
# 
# hist(output1$Total_Cost)
# 
# hist(output1$Obtainable_Energy)
# 
# 
# ggplot(output1, aes(Spec_Range, Total_Cost, color=Passenger_Mass)) +
#   geom_point(alpha = 0.1)



