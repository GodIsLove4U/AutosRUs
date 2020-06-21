## Part I <---------- Multiple Linear Regression Written Analysis & Model

# Read CSV File with sample data of 50 vehicles
mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)

# Look at linear model information and perform calculations
mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data)

# Get a summary of the linear model data
summary(mecha_lm)

# Create a separate calculation
mecha_data$AWD <- factor(mecha_data$AWD)

# Create a box plot to visualize the data by comparing AWD to non-AWD
ggplot(mecha_data, aes(x = AWD, y = mpg)) +
  geom_boxplot(color = "black", fill = "steelblue") +
  geom_jitter(color = "red", width = .1, height = 0)

# Create a scatter plot by vehicle length and miles per gallon
ggplot(mecha_data, aes(x = vehicle_length, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red")

## Part II <---------- Summary Statistics Written Analysis & Model

# Create a variable that reads in and holds the 150 vehicles with suspension coil information
susp_data <- read.csv("Suspension_Coil.csv")

# Create a separate variable to look at the statistics
susp_data2 <- read.csv("Suspension_Coil.csv", stringsAsFactors = F, check.names =  F)

# Summary the suspension coil data
susp_sum <- susp_data2 %>% group_by(Manufacturing_Lot) %>%
    summarise(Mean=mean(PSI), Median=median(PSI), Var=var(PSI), SD=sd(PSI))

# Print a summary of the suspension coil data
summary(susp_sum)

# Create a box plot to visualize the data
ggplot(susp_data2, aes(x = VehicleID, y = PSI)) +
  geom_boxplot(color = "black", fill = "steelblue") +
  geom_jitter(color = "red", width = .1, height = 0)

# Summarize the suspension coil data, using mean, median, variance and standard deviation
tot_sum <- susp_data2 %>%
  summarise(Mean=mean(PSI), Median=median(PSI), Var=var(PSI), SD=sd(PSI))

# Print a summary of the data
summary(tot_sum)

## Part III <---------- Statistical Difference Written Analysis & Model 

# Do a t-test, looking at vehicles on Lot 1
t.test(subset(susp_data2, Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

# Do a t-test, looking at vehicles on Lot 2
t.test(subset(susp_data2, Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

# Do a t-test, looking at vehicles on Lot 3
t.test(subset(susp_data2, Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
