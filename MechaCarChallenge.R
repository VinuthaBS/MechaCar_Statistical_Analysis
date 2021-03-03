library(dplyr)

# LINEAR REGRESSION to predict mpg

# Read MechaCar_mpg.csv
MechaCar_table <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors=F)

#generate multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_table) 

#generate summary statistics for linear regression
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_table)) 

# SUMMARY STATISTICS for Suspension Coils

# Read Suspension_Coil.csv
SCoil_table <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors=F)

# Total Summary stats
total_summary = SCoil_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# Summary Stats grouped on Manufacturing_Lot
lot_summary = SCoil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# T-TESTS on Suspension Coils

# t-test across all lots
t.test(SCoil_table$PSI, mu=1500)

# t-test for Lot1
t.test(subset(SCoil_table$PSI, SCoil_table$Manufacturing_Lot == 'Lot1'), mu=1500)

# t-test for Lot2
t.test(subset(SCoil_table$PSI, SCoil_table$Manufacturing_Lot == 'Lot2'), mu=1500)

# t-test for Lot3
t.test(subset(SCoil_table$PSI, SCoil_table$Manufacturing_Lot == 'Lot3'), mu=1500)
