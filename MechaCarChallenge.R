library(dplyr)


# Deliverable 1 


mech_df <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)
mech_lm = lm(mpg~ vehicle_length + vehicle_weight +spoiler_angle  +  ground_clearance +
             AWD  , data = mech_df) #Create the linear regression
summary(mech_lm)

# Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset








#Is the slope of the linear model considered to be zero? Why or why not






#Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?






# Deliverable 2 
susp_df <- read.csv("Suspension_Coil.csv",stringsAsFactors = F,check.names = F)


total_summary<-susp_df%>% 
  summarise(Mean= mean(PSI),Median= median(PSI), Variance= var(PSI), SD= sd(PSI) )



lot_summary<-susp_df%>% group_by(Manufacturing_Lot)%>%
  summarise(Mean= mean(PSI),Median= median(PSI), Variance= var(PSI), SD= sd(PSI) )



# Deliverable 3
t.test(susp_df$PSI,mu = 1500)
t.test(subset(susp_df,Manufacturing_Lot == "Lot1")$PSI,mu = 1500)

t.test(subset(susp_df,Manufacturing_Lot == "Lot2")$PSI,mu = 1500)

t.test(subset(susp_df,Manufacturing_Lot == "Lot3")$PSI,mu = 1500)


