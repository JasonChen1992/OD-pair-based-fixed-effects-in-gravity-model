library(tidyverse)
library(dplyr)
library(car)

# Read in the data
my_data<- read.csv(".../County-to-County_migration_ReadytoUse_New.csv", header = TRUE)
train_data <- read.csv(".../migration_train_data_new.csv")
test_data <- read.csv(".../migration_test_data_new.csv")

###Table 1 results
####County to County migration data
# Model PPML GM
Poss_GM <- glm(Flow ~ log(O_pop)+log(D_pop)+log(Distance),family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
summary(Poss_GM)
logLik(Poss_GM)

# Model PPML GM_O/D
GM_O_D <- glm(Flow ~ log(O_pop) + log(D_pop) + log(Distance)+ Out_Alabama + In_Alabama + Inside_Alabama + 
               In_Arizona + Out_Arizona + Inside_Arizona + Out_Arkansas + In_Arkansas + Inside_Arkansas + 
               In_California + Out_California + Inside_California +
               Out_Colorado + In_Colorado + Inside_Colorado + Out_Connecticut + In_Connecticut + Inside_Connecticut + 
               Out_Delaware + In_Delaware + Inside_Delaware + Out_District.of.Columbia + In_District.of.Columbia + 
               Inside_District.of.Columbia + 
               Out_Florida + In_Florida + Inside_Florida + Out_Georgia + In_Georgia + Inside_Georgia + Out_Idaho + In_Idaho + 
               Inside_Idaho + Out_Illinois + 
               In_Illinois + Inside_Illinois + Out_Indiana + In_Indiana + Inside_Indiana + Out_Iowa + In_Iowa + Inside_Iowa + 
               Out_Kansas + In_Kansas + Inside_Kansas + Out_Kentucky + In_Kentucky + Inside_Kentucky + Out_Louisiana + 
               In_Louisiana + Inside_Louisiana + Out_Maine + In_Maine + Inside_Maine + Out_Maryland + In_Maryland + 
               Inside_Maryland + Out_Massachusetts + In_Massachusetts + Inside_Massachusetts + Out_Michigan + In_Michigan + 
               Inside_Michigan + Out_Minnesota + In_Minnesota + Inside_Minnesota + Out_Mississippi + In_Mississippi + 
               Inside_Mississippi + Out_Missouri + In_Missouri + Inside_Missouri + Out_Montana + In_Montana + Inside_Montana + 
               Out_Nebraska + In_Nebraska + Inside_Nebraska + Out_Nevada + In_Nevada + Inside_Nevada + Out_New.Hampshire + 
               In_New.Hampshire + Inside_New.Hampshire + Out_New.Jersey + In_New.Jersey + Inside_New.Jersey + Out_New.Mexico + 
               In_New.Mexico + Inside_New.Mexico + Out_New.York + In_New.York + Inside_New.York + Out_North.Carolina + 
               In_North.Carolina + Inside_North.Carolina + Out_North.Dakota + In_North.Dakota + Inside_North.Dakota + Out_Ohio + 
               In_Ohio + Inside_Ohio + Out_Oklahoma + In_Oklahoma + Inside_Oklahoma + Out_Oregon + In_Oregon + Inside_Oregon + 
               Out_Pennsylvania + In_Pennsylvania + Inside_Pennsylvania + Out_Rhode.Island + In_Rhode.Island + 
               Inside_Rhode.Island + Out_South.Carolina + In_South.Carolina + Inside_South.Carolina + Out_South.Dakota +
               In_South.Dakota + Inside_South.Dakota + Out_Tennessee + In_Tennessee + Inside_Tennessee +
               In_Texas +  
               Out_Utah + In_Utah + Inside_Utah + Out_Vermont + In_Vermont + Inside_Vermont + 
               Out_Virginia + In_Virginia + Inside_Virginia + Out_Washington + In_Washington + Inside_Washington + 
               Out_West.Virginia + In_West.Virginia + Inside_West.Virginia + Out_Wisconsin + In_Wisconsin + Inside_Wisconsin + 
               Out_Wyoming + In_Wyoming + Inside_Wyoming, family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
summary(GM_O_D)
logLik(GM_O_D)
AIC(GM_O_D)

# Model PPML GM_OD
GM_OD <- glm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance) + OD_pair, family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
summary(GM_OD)
logLik(GM_OD)

##Calculate pseudo-R2
null_model <- glm(Flow ~ 1, 
                  family = poisson(link = 'log'), 
                  data = my_data)
lnL0 <- logLik(null_model)
lnLM <- logLik(target_model)
pseudo_R2 <- 1 - as.numeric(lnLM / lnL0)
pseudo_R2

#define how to calculate Root Mean Squared Error (RMSE)
CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}

###Using training data to predict
mdata = test_data

#run three models one by one
Poss_GM <- glm(Flow ~ log(O_pop)+log(D_pop)+log(Distance),family= poisson(link = 'log'), na.action = na.exclude, data = train_data)
mdata$unconstrainedEst2 <- exp(predict(Poss_GM,mdata))

Poss_O_D <- glm(Flow ~ log(O_pop) + log(D_pop) + log(Distance)+ Out_Alabama + In_Alabama + Inside_Alabama + 
               In_Arizona + Out_Arizona + Inside_Arizona + Out_Arkansas + In_Arkansas + Inside_Arkansas + 
               In_California + Out_California + Inside_California +
               Out_Colorado + In_Colorado + Inside_Colorado + Out_Connecticut + In_Connecticut + Inside_Connecticut + 
               Out_Delaware + In_Delaware + Inside_Delaware + Out_District.of.Columbia + In_District.of.Columbia + 
               Inside_District.of.Columbia + 
               Out_Florida + In_Florida + Inside_Florida + Out_Georgia + In_Georgia + Inside_Georgia + Out_Idaho + In_Idaho + 
               Inside_Idaho + Out_Illinois + 
               In_Illinois + Inside_Illinois + Out_Indiana + In_Indiana + Inside_Indiana + Out_Iowa + In_Iowa + Inside_Iowa + 
               Out_Kansas + In_Kansas + Inside_Kansas + Out_Kentucky + In_Kentucky + Inside_Kentucky + Out_Louisiana + 
               In_Louisiana + Inside_Louisiana + Out_Maine + In_Maine + Inside_Maine + Out_Maryland + In_Maryland + 
               Inside_Maryland + Out_Massachusetts + In_Massachusetts + Inside_Massachusetts + Out_Michigan + In_Michigan + 
               Inside_Michigan + Out_Minnesota + In_Minnesota + Inside_Minnesota + Out_Mississippi + In_Mississippi + 
               Inside_Mississippi + Out_Missouri + In_Missouri + Inside_Missouri + Out_Montana + In_Montana + Inside_Montana + 
               Out_Nebraska + In_Nebraska + Inside_Nebraska + Out_Nevada + In_Nevada + Inside_Nevada + Out_New.Hampshire + 
               In_New.Hampshire + Inside_New.Hampshire + Out_New.Jersey + In_New.Jersey + Inside_New.Jersey + Out_New.Mexico + 
               In_New.Mexico + Inside_New.Mexico + Out_New.York + In_New.York + Inside_New.York + Out_North.Carolina + 
               In_North.Carolina + Inside_North.Carolina + Out_North.Dakota + In_North.Dakota + Inside_North.Dakota + Out_Ohio + 
               In_Ohio + Inside_Ohio + Out_Oklahoma + In_Oklahoma + Inside_Oklahoma + Out_Oregon + In_Oregon + Inside_Oregon + 
               Out_Pennsylvania + In_Pennsylvania + Inside_Pennsylvania + Out_Rhode.Island + In_Rhode.Island + 
               Inside_Rhode.Island + Out_South.Carolina + In_South.Carolina + Inside_South.Carolina + Out_South.Dakota +
               In_South.Dakota + Inside_South.Dakota + Out_Tennessee + In_Tennessee + Inside_Tennessee +
               In_Texas +  
               Out_Utah + In_Utah + Inside_Utah + Out_Vermont + In_Vermont + Inside_Vermont + 
               Out_Virginia + In_Virginia + Inside_Virginia + Out_Washington + In_Washington + Inside_Washington + 
               Out_West.Virginia + In_West.Virginia + Inside_West.Virginia + Out_Wisconsin + In_Wisconsin + Inside_Wisconsin + 
               Out_Wyoming + In_Wyoming + Inside_Wyoming,family= poisson(link = 'log'), na.action = na.exclude, data = train_data)
mdata$unconstrainedEst2 <- exp(predict(Poss_O_D,mdata))

Poss_OD <- glm(Flow ~ log(O_pop) + log(D_pop) + log(Distance) + ODpair_New, family= poisson(link = 'log'), na.action = na.exclude, data = train_data)
mdata$unconstrainedEst2 <- exp(predict(Poss_OD,mdata))



flow = mdata$Flow
###Calculate RMSE
CalcRMSE(flow,mdata$unconstrainedEst2)
# Calculate MAPE
absolute_percentage_error <- abs((flow - mdata$unconstrainedEst2) / flow)
mean(absolute_percentage_error, na.rm = TRUE)
# Calculate MAE
absolute_error <- abs(flow - mdata$unconstrainedEst2)
mean(absolute_error)
#Spearman correlation
cor.test(flow, mdata$unconstrainedEst2, method = "spearman")



###Code for Figure 1
C = cut_width(log(flow),0.5,boundary=0)

labs <- levels(C)[C]
lower <- as.numeric( sub("\\((.+),.*", "\\1", labs))
lower[is.na(lower)] = 0
upper <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs))


for (i in 1:length(mdata[,1])){
  mdata$benchmark[i] = (lower[i]+upper[i])/2
}

for (i in 1:length(mdata[,1])){
  A = mdata$unconstrainedEst2[mdata$benchmark==mdata$benchmark[i]]
  mdata$five_quantile[i] = log(quantile(A,probs = 0.05, na.rm = FALSE))
  mdata$nine5_quantile[i] = log(quantile(A,probs = 0.95, na.rm = FALSE))
}


for (i in 1:length(mdata[,1])){
  if (mdata$benchmark[i] >= mdata$five_quantile[i] & mdata$benchmark[i] <= mdata$nine5_quantile[i]){
    mdata$judge[i] = "green"
  } else{
    mdata$judge[i] = "red"
  }
}

pure_exp <- function(x) {
  exponents = floor(log10(x))
  parse(text=paste0('10^', exponents))
}


ggplot(data = mdata, aes(Flow, unconstrainedEst2)) +
  geom_point(color='grey') +
  geom_abline(aes(intercept=0, slope=1)) +
  geom_boxplot(aes(group=benchmark, color = judge), width = 0.4, outlier.alpha = 0) + 
  scale_color_manual(values = alpha(c("green","red"), 1)) +
  xlab("Flow intensity (observed)") +
  ylab("Flow intensity (predicted)") +
  scale_x_log10(breaks = 10^(0:5), 
                labels = pure_exp, 
                expand = c(0, 0), limits = c(1, 10^5)) +
  scale_y_log10(breaks = 10^(0:5), 
                labels = pure_exp, 
                expand = c(0, 0), limits = c(1, 10^5))


###Figure 2a (obtain the corresponding FE)
GM_O_D <- glm(Flow ~ log(O_pop) + log(D_pop) + log(Distance)+ Out_Alabama + In_Alabama + Inside_Alabama + 
               In_Arizona + Out_Arkansas + In_Arkansas + Inside_Arkansas + In_California + Out_California + Inside_California +
               Out_Colorado + In_Colorado + Inside_Colorado + Out_Connecticut + In_Connecticut + Inside_Connecticut + 
               Out_Delaware + In_Delaware + Inside_Delaware + Out_District.of.Columbia + In_District.of.Columbia + 
               Inside_District.of.Columbia + 
               Out_Florida + In_Florida + Inside_Florida + Out_Georgia + In_Georgia + Inside_Georgia + Out_Idaho + In_Idaho + 
               Inside_Idaho + Out_Illinois + 
               In_Illinois + Inside_Illinois + Out_Indiana + In_Indiana + Inside_Indiana + Out_Iowa + In_Iowa + Inside_Iowa + 
               Out_Kansas + In_Kansas + Inside_Kansas + Out_Kentucky + In_Kentucky + Inside_Kentucky + Out_Louisiana + 
               In_Louisiana + Inside_Louisiana + Out_Maine + In_Maine + Inside_Maine + Out_Maryland + In_Maryland + 
               Inside_Maryland + Out_Massachusetts + In_Massachusetts + Inside_Massachusetts + Out_Michigan + In_Michigan + 
               Inside_Michigan + Out_Minnesota + In_Minnesota + Inside_Minnesota + Out_Mississippi + In_Mississippi + 
               Inside_Mississippi + Out_Missouri + In_Missouri + Inside_Missouri + Out_Montana + In_Montana + Inside_Montana + 
               Out_Nebraska + In_Nebraska + Inside_Nebraska + Out_Nevada + In_Nevada + Inside_Nevada + Out_New.Hampshire + 
               In_New.Hampshire + Inside_New.Hampshire + Out_New.Jersey + In_New.Jersey + Inside_New.Jersey + Out_New.Mexico + 
               In_New.Mexico + Inside_New.Mexico + Out_New.York + In_New.York + Inside_New.York + Out_North.Carolina + 
               In_North.Carolina + Inside_North.Carolina + Out_North.Dakota + In_North.Dakota + Inside_North.Dakota + Out_Ohio + 
               In_Ohio + Inside_Ohio + Out_Oklahoma + In_Oklahoma + Inside_Oklahoma + Out_Oregon + In_Oregon + Inside_Oregon + 
               Out_Pennsylvania + In_Pennsylvania + Inside_Pennsylvania + Out_Rhode.Island + In_Rhode.Island + 
               Inside_Rhode.Island + Out_South.Carolina + In_South.Carolina + Inside_South.Carolina + Out_South.Dakota +
               In_South.Dakota + Inside_South.Dakota + Out_Tennessee + In_Tennessee + Inside_Tennessee +
               In_Texas + Out_Texas + Inside_Texas + 
               Out_Utah + In_Utah + Inside_Utah + Out_Vermont + In_Vermont + Inside_Vermont + 
               Out_Virginia + In_Virginia + Inside_Virginia + Out_Washington + In_Washington + Inside_Washington + 
               Out_West.Virginia + In_West.Virginia + Inside_West.Virginia + Out_Wisconsin + In_Wisconsin + Inside_Wisconsin + 
               Out_Wyoming + In_Wyoming + Inside_Wyoming, family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
summary(GM_O_D)

GM_OD_summary <- summary(GM_O_D)
GM_OD_summary_df <- data.frame(GM_OD_summary$coefficients)
write.csv(GM_OD_summary_df, file = ".../PPML_O_D_summary_results_Arizona.csv")
###Figure 2b
GM_O_D <- glm(Flow ~ log(O_pop) + log(D_pop) + log(Distance)+ Out_Alabama + In_Alabama + Inside_Alabama + 
               In_Arizona + Out_Arizona + Inside_Arizona + Out_Arkansas + In_Arkansas + Inside_Arkansas + 
               In_California + Out_California + Inside_California +
               Out_Colorado + In_Colorado + Inside_Colorado + Out_Connecticut + In_Connecticut + Inside_Connecticut + 
               Out_Delaware + In_Delaware + Inside_Delaware + Out_District.of.Columbia + In_District.of.Columbia + 
               Inside_District.of.Columbia + 
               Out_Florida + In_Florida + Inside_Florida + Out_Georgia + In_Georgia + Inside_Georgia + Out_Idaho + In_Idaho + 
               Inside_Idaho + Out_Illinois + 
               In_Illinois + Inside_Illinois + Out_Indiana + In_Indiana + Inside_Indiana + Out_Iowa + In_Iowa + Inside_Iowa + 
               Out_Kansas + In_Kansas + Inside_Kansas + Out_Kentucky + In_Kentucky + Inside_Kentucky + Out_Louisiana + 
               In_Louisiana + Inside_Louisiana + Out_Maine + In_Maine + Inside_Maine + Out_Maryland + In_Maryland + 
               Inside_Maryland + Out_Massachusetts + In_Massachusetts + Inside_Massachusetts + Out_Michigan + In_Michigan + 
               Inside_Michigan + Out_Minnesota + In_Minnesota + Inside_Minnesota + Out_Mississippi + In_Mississippi + 
               Inside_Mississippi + Out_Missouri + In_Missouri + Inside_Missouri + Out_Montana + In_Montana + Inside_Montana + 
               Out_Nebraska + In_Nebraska + Inside_Nebraska + Out_Nevada + In_Nevada + Inside_Nevada + Out_New.Hampshire + 
               In_New.Hampshire + Inside_New.Hampshire + Out_New.Jersey + In_New.Jersey + Inside_New.Jersey + Out_New.Mexico + 
               In_New.Mexico + Inside_New.Mexico + Out_New.York + In_New.York + Inside_New.York + Out_North.Carolina + 
               In_North.Carolina + Inside_North.Carolina + Out_North.Dakota + In_North.Dakota + Inside_North.Dakota + Out_Ohio + 
               In_Ohio + Inside_Ohio + Out_Oklahoma + In_Oklahoma + Inside_Oklahoma + Out_Oregon + In_Oregon + Inside_Oregon + 
               Out_Pennsylvania + In_Pennsylvania + Inside_Pennsylvania + Out_Rhode.Island + In_Rhode.Island + 
               Inside_Rhode.Island + Out_South.Carolina + In_South.Carolina + Inside_South.Carolina + Out_South.Dakota +
               In_South.Dakota + Inside_South.Dakota + Out_Tennessee + 
               In_Texas + Out_Texas + Inside_Texas + 
               Out_Utah + In_Utah + Inside_Utah + Out_Vermont + In_Vermont + Inside_Vermont + 
               Out_Virginia + In_Virginia + Inside_Virginia + Out_Washington + In_Washington + Inside_Washington + 
               Out_West.Virginia + In_West.Virginia + Inside_West.Virginia + Out_Wisconsin + In_Wisconsin + Inside_Wisconsin + 
               Out_Wyoming + In_Wyoming + Inside_Wyoming, family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
summary(GM_O_D)

GM_OD_summary <- summary(GM_O_D)
GM_OD_summary_df <- data.frame(GM_OD_summary$coefficients)
write.csv(GM_OD_summary_df, file = ".../PPML_O_D_summary_results_Tennessee.csv")




###############################################
#####Taxi data
taxi_data<- read.csv(".../Taxi_Data_ReadytoUse_Final_New.csv", header = TRUE)
train_data <- read.csv(".../Taxi_train_data_New.csv")
test_data <- read.csv(".../Taxi_test_data_New.csv")
mdata = test_data
# Model GM
GM <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance), na.action = na.exclude, data = taxi_data) ###switch lm() to glm() to obtain AIC value
summary(GM)
logLik(GM)

# Model GM_O/D
GM_O_D <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance)+ 
               In_Bronx + Out_Bronx + Inside_Bronx +
               In_Brooklyn +
               In_Manhattan + Out_Manhattan + Inside_Manhattan +
               In_Queens + Out_Queens + Inside_Queens+
               In_StatenIsland + Out_StatenIsland + Inside_StatenIsland,
             na.action = na.exclude, data = taxi_data)
summary(GM_O_D)
logLik(GM_O_D)

# Model GM_OD
GM_OD <- glm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance) + OD_pair, na.action = na.exclude, data = taxi_data)
summary(GM_OD)
logLik(GM_OD)


###Using training data to predict
mdata = test_data

GM <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance), na.action = na.exclude, data = train_data)
mdata$unconstrainedEst2 <- exp(predict(GM,mdata))

GM_O_D <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance)+ 
               In_Bronx + Out_Bronx + Inside_Bronx +
               In_Brooklyn +
               In_Manhattan + Out_Manhattan + Inside_Manhattan +
               In_Queens + Out_Queens + Inside_Queens+
               In_StatenIsland + Out_StatenIsland + Inside_StatenIsland,
               na.action = na.exclude, data = train_data)
mdata$unconstrainedEst2 <- exp(predict(GM_O_D,mdata))

GM_OD <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance) + OD_pair, na.action = na.exclude, data = train_data)
mdata$unconstrainedEst2 <- exp(predict(GM_OD,mdata))


flow = mdata$Flow

###Calculate RMSE
CalcRMSE(flow,mdata$unconstrainedEst2)
# Calculate MAPE
absolute_percentage_error <- abs((flow - mdata$unconstrainedEst2) / flow)
mean(absolute_percentage_error, na.rm = TRUE)
# Calculate MAE
absolute_error <- abs(flow - mdata$unconstrainedEst2)
mean(absolute_error)
#Spearman correlation
cor.test(flow, mdata$unconstrainedEst2, method = "spearman")


###For Figure 4 (Bring in the corresponding unconstrainedEst2 values according to different models)
C = cut_width(log(flow),0.5,boundary=0)

labs <- levels(C)[C]
lower <- as.numeric( sub("\\((.+),.*", "\\1", labs))
lower[is.na(lower)] = 0
upper <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs))


for (i in 1:length(mdata[,1])){
  mdata$benchmark[i] = (lower[i]+upper[i])/2
}

for (i in 1:length(mdata[,1])){
  A = mdata$unconstrainedEst2[mdata$benchmark==mdata$benchmark[i]]
  mdata$five_quantile[i] = log(quantile(A,probs = 0.05, na.rm = FALSE))
  mdata$nine5_quantile[i] = log(quantile(A,probs = 0.95, na.rm = FALSE))
}

for (i in 1:length(mdata[,1])){
  if (mdata$benchmark[i] >= mdata$five_quantile[i] & mdata$benchmark[i] <= mdata$nine5_quantile[i]){
    mdata$judge[i] = "green"
  } else{
    mdata$judge[i] = "red"
  }
}


pure_exp <- function(x) {
  exponents = floor(log10(x))
  parse(text=paste0('10^', exponents))
}


ggplot(data = mdata, aes(Flow, unconstrainedEst2)) +
  geom_point(color='grey') +
  geom_abline(aes(intercept=0, slope=1)) +
  geom_boxplot(aes(group=benchmark, color = judge), width = 0.4, outlier.alpha = 0) + 
  scale_color_manual(values = alpha(c("green","red"), 1)) +
  xlab("Flow intensity (data)") +
  ylab("Flow intensity (model)") +
  scale_x_log10(breaks = 10^(0:5), 
                labels = pure_exp, 
                expand = c(0, 0), limits = c(1, 10^5)) +
  scale_y_log10(breaks = 10^(0:5), 
                labels = pure_exp, 
                expand = c(0, 0), limits = c(1, 10^5))



###Figure 5a (obtain the corresponding FE)
GM_O_D <- glm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance)+ In_Bronx + Out_Bronx + Inside_Bronx +
               Out_Brooklyn +
               In_Manhattan + Out_Manhattan + Inside_Manhattan +
               In_Queens + Out_Queens + Inside_Queens+
               In_StatenIsland + Out_StatenIsland + Inside_StatenIsland,
               na.action = na.exclude, data = taxi_data)
summary(GM_O_D)
GM_OD_summary <- summary(GM_O_D)
GM_OD_summary_df <- data.frame(GM_OD_summary$coefficients)
write.csv(GM_OD_summary_df, file = "C:/...")
###Figure 5b
GM_O_D <- glm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance)+ In_Bronx + Out_Bronx + Inside_Bronx +
               In_Brooklyn +
               In_Manhattan + Out_Manhattan + Inside_Manhattan +
               In_Queens + Out_Queens + Inside_Queens+
               In_StatenIsland + Out_StatenIsland + Inside_StatenIsland,
               na.action = na.exclude, data = taxi_data)
summary(GM_O_D)
GM_OD_summary <- summary(GM_O_D)
GM_OD_summary_df <- data.frame(GM_OD_summary$coefficients)
write.csv(GM_OD_summary_df, file = "C:/...")


###Table 3
# Table 3 ANOVA and T-test results
# Initialize an empty dataframe to store the coefficients of the model
all_coefficients <- data.frame()
# Create a list of dates from 0109 to 0115
dates <- c("0109", "0110", "0111", "0112", "0113", "0114", "0115")
# Run the model for each date and collect the coefficients
for (date in dates) {
  file_name <- paste0(".../OD_RawData_", date, "_ReadytoUse_Final.csv")
  my_data <- read.csv(file_name, header = TRUE)

  GM_OD <- lm(log(Flow) ~ log(O_Pop) + log(D_Pop) + log(Distance) + OD_pair, na.action = na.exclude, data = my_data)
  # Get model coefficients and add to all_coefficients dataframe
  coef_data <- as.data.frame(t(coef(GM_OD)))
  coef_data$date <- date
  
  all_coefficients <- rbind(all_coefficients, coef_data)
}

# View summarized coefficient data
print(all_coefficients)
transposed_data <- as.data.frame(t(all_coefficients))
write.csv(transposed_data, 
          ".../GM_OD_DaybyDay_coefficients_summary_New.csv", row.names = TRUE) ##Export for next step analysis

# 1. Read data
data <- read.csv(".../GM_OD_DaybyDay_coefficients_summary_New.csv",header = TRUE) ##It is the FE for Table 3

# 2. Convert from wide to long format
data_sevendays <- gather(data, Day, Estimate, Day109:Day115)
data_weekdays <- gather(data, Day, Estimate, Day109:Day113)
# 3. ANOVA test
anova_result <- aov(Estimate ~ Day, data = data_sevendays)
print(summary(anova_result))

anova_result <- aov(Estimate ~ Day, data = data_weekdays)
print(summary(anova_result))

t_result <- t.test(data$Day114, data$Day115)
print(t_result)


###Table 4
data<- read.csv(".../TaxiData_0102_0108_ReadytoUse_Final.csv", header = TRUE)

GM_OD <- lm(log(Flow) ~ log(O_Pop) + log(D_Pop) + log(Distance) + OD_pair, na.action = na.exclude, data = data)
# Extract the summary
GM_OD_summary <- summary(GM_OD)
# Convert the summary into a data frame
GM_OD_summary_df <- data.frame(GM_OD_summary$coefficients)
# Export the summary data frame to a .csv file
write.csv(GM_OD_summary_df, 
          file = ".../Taxi_GM_OD_0102_0108_New.csv")
#Repeat the steps to get the other three data 
# "TaxiData_0109_0115_ReadytoUse_Final.csv";"TaxiData_0116_0122_ReadytoUse_Final.csv";"TaxiData_0123_0129_ReadytoUse_Final.csv"

list1 = read.csv(".../Taxi_GM_OD_0102_0108_New.csv",header = TRUE) #Corresponding FE results in Table 4 
list2 = read.csv(".../Taxi_GM_OD_0109_0115_New.csv",header = TRUE)
list3 = read.csv(".../Taxi_GM_OD_0116_0122_New.csv",header = TRUE)
list4 = read.csv(".../Taxi_GM_OD_0123_0129_New.csv",header = TRUE)
# Add a new column to identify the source of the data
list1$Group <- "0102_0108"
list2$Group <- "0109_0115"
list3$Group <- "0116_0122"
list4$Group <- "0123_0129"
# Combine data into a data frame
combined_data <- rbind(list1, list2, list3, list4)
# Compare the values of Estimate using ANOVA
fit <- lm(Estimate ~ Group, data = combined_data)
anova_result <- anova(fit)
print(anova_result)

###Table 5
AM_data<- read.csv(".../Taxi_Data_MorningRushHour_ReadytoUse_Final.csv", header = TRUE)
PM_data<- read.csv(".../Taxi_Data_EveningRushHour_ReadytoUse_Final.csv", header = TRUE)

GM_OD <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance) + OD_pair, na.action = na.exclude, data = AM_data)
summary(GM_OD)

GM_OD <- lm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance) + OD_pair, na.action = na.exclude, data = PM_data)
summary(GM_OD)


#######
##test multilevel
library(nlme)
library(MuMIn)
library(lme4)
library(lmerTest)


#Migration flow
my_data$log_Flow = (log$Flow)
USmultilevelmodel <- glmer(Flow ~ I(log(O_pop)) + I(log(D_pop)) + I(log(Distance)) + (1|O_state_name) + (1|D_state_name) + (1|O_state_name:D_state_name)
                           ,family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
AIC(USmultilevelmodel)
logLik(USmultilevelmodel)
summary(USmultilevelmodel)

#Fhv flow              #+ (1|O_borough:D_borough)
taxi_data$log_Flow = log(taxi_data$Flow)
multilevelmodel <- lmer(log_Flow ~ I(log(O_pop)) + I(log(D_pop)) + I(log(Distance)) + (1|O_borough) + (1|D_borough) 
                        , na.action = na.exclude, data = taxi_data)

random = ranef(multilevelmodel,condVar=TRUE)
random1 = random[[1]]
random1
random
multilevelmodel <- glm(log_Flow ~ I(log(O_pop)) + I(log(D_pop)) + I(log(Distance)) + as.factor(O_borough) + as.factor(D_borough), na.action = na.exclude, data = taxi_data)
AIC(multilevelmodel)
logLik(multilevelmodel)
summary(multilevelmodel)
VarCorr(model)




##For Appdenix
#PPML 10*2
Poss_O_D <- glm(Flow ~ log(O_pop) + log(D_pop) + log(Distance)+ Out_RegionA + In_RegionA + Inside_RegionA + 
                  Out_RegionB + In_RegionB + Inside_RegionB +
                  Out_RegionC + In_RegionC + Inside_RegionC +
                  Out_RegionD + In_RegionD + Inside_RegionD +
                  Out_RegionE + In_RegionE + Inside_RegionE +
                  In_RegionF +
                  Out_RegionG + In_RegionG + Inside_RegionG +
                  Out_RegionH + In_RegionH + Inside_RegionH +
                  Out_RegionI + In_RegionI + Inside_RegionI+
                  Out_RegionJ + In_RegionJ + Inside_RegionJ, family= poisson(link = 'log'), na.action = na.exclude, data = my_data)


#GM_O/D 2*51
GM_O_D <- glm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance)+ 
                In_Council1 + Out_Council1 + Inside_Council1 + In_Council2 + Out_Council2 + Inside_Council2 + 
                In_Council3 + Out_Council3 + Inside_Council3 + In_Council4 + Out_Council4 + Inside_Council4 +
                In_Council5 + Out_Council5 + Inside_Council5 + In_Council6 + Out_Council6 + Inside_Council6 +
                In_Council7 + Out_Council7 + Inside_Council7 + In_Council8 + Out_Council8 + Inside_Council8 +
                In_Council9 + Out_Council9 + Inside_Council9 + In_Council10 + Out_Council10 + Inside_Council10 +
                In_Council11 + Out_Council11 + Inside_Council11 + In_Council12 + Out_Council12 + Inside_Council12 +
                In_Council13 + Out_Council13 + Inside_Council13 + In_Council14 + Out_Council14 + Inside_Council14 +
                In_Council15 + Out_Council15 + Inside_Council15 + In_Council16 + Out_Council16 + Inside_Council16 +
                In_Council17 + Out_Council17 + Inside_Council17 + In_Council18 + Out_Council18 + Inside_Council18 +
                In_Council19 + Out_Council19 + Inside_Council19 + In_Council20 + Out_Council20 + Inside_Council20 +
                In_Council21 + Out_Council21 + Inside_Council21 + In_Council22 + Out_Council22 + Inside_Council22 +
                In_Council23 + Out_Council23 + Inside_Council23 + In_Council24 + Out_Council24 + Inside_Council24 +
                In_Council25 + Out_Council25 + Inside_Council25 + In_Council26 + Out_Council26 + Inside_Council26 +
                In_Council27 + Out_Council27 + Inside_Council27 + In_Council28 + Out_Council28 + Inside_Council28 +
                In_Council29 + Out_Council29 + Inside_Council29 + In_Council30 + Out_Council30 + Inside_Council30 +
                In_Council31 + Out_Council31 + Inside_Council31 + In_Council32 + Out_Council32 + Inside_Council32 +
                In_Council33 + Out_Council33 + Inside_Council33 + In_Council34 + Out_Council34 + Inside_Council34 +
                In_Council35 + Out_Council35 + Inside_Council35 + In_Council36 + Out_Council36 + Inside_Council36 +
                In_Council37 + Out_Council37 + Inside_Council37 + In_Council38 + Out_Council38 + Inside_Council38 +
                In_Council39 + Out_Council39 + Inside_Council39 + In_Council40 + Out_Council40 + Inside_Council40 +
                In_Council41 + Out_Council41 + Inside_Council41 + In_Council42 + Out_Council42 + Inside_Council42 +
                In_Council43 + Out_Council43 + Inside_Council43 + In_Council44 + Out_Council44 + Inside_Council44 +
                In_Council45 + Out_Council45 + Inside_Council45 + In_Council46 + Out_Council46 + Inside_Council46 +
                In_Council47 + Out_Council47 + Inside_Council47 + In_Council48 + Out_Council48 + Inside_Council48 +
                In_Council49 + Out_Council49 + Inside_Council49 + In_Council50 + Out_Council50 + Inside_Council50 +
                In_Council51 + Out_Council51 + Inside_Council51, na.action = na.exclude, data = taxi_data) 
##GM_O/D 2*262
GM_O_D262 <- glm(log(Flow) ~ log(O_pop) + log(D_pop) + log(Distance) + Origin_zone + Destination_zone
              , na.action = na.exclude, data = taxi_data)



#test 3000*2 PPML_O_D
Poss_GM <- glm(Flow ~ log(O_pop)+log(D_pop)+log(Distance)+O_full_county+D_full_county,family= poisson(link = 'log'), na.action = na.exclude, data = my_data)
summary(Poss_GM)
logLik(Poss_GM)
AIC(Poss_GM)
##Calculate pseudo-R2
null_model <- glm(Flow ~ 1, 
                  family = poisson(link = 'log'), 
                  data = my_data)
lnL0 <- logLik(null_model)
lnLM <- logLik(Poss_GM)
pseudo_R2 <- 1 - as.numeric(lnLM / lnL0)
pseudo_R2

mdata = test_data
Poss_GM_test <- glm(Flow ~ log(O_pop)+log(D_pop)+log(Distance)+O_full_county+D_full_county,family= poisson(link = 'log'), na.action = na.exclude, data = train_data)

###Calculate RMSE
CalcRMSE(flow,mdata$unconstrainedEst2)
# Calculate MAPE
absolute_percentage_error <- abs((flow - mdata$unconstrainedEst2) / flow)
mean(absolute_percentage_error, na.rm = TRUE)
# Calculate MAE
absolute_error <- abs(flow - mdata$unconstrainedEst2)
mean(absolute_error)
#Spearman correlation
cor.test(flow, mdata$unconstrainedEst2, method = "spearman")






