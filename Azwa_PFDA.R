#Libraries Installed
library("readr") #Reads CSV Files Into R
library("dplyr")  #Data Manipulation
library("tidyverse") #Not Sure
library("naniar") #MCAR
library("ggplot2") #Creating Plots

#Setting the CWD
setwd("D:/PFDA") 

#Data Import
local_file <- "4.hackingdata.csv"
csvdata <- read_csv(local_file)
hackingdata = data.frame(csvdata)



#Data Cleaning

#Removing Duplicates in Data
#Data Cleaning
#Removing Duplicates
nrow(hackingdata) #[212093]
hackingdata[duplicated(hackingdata) | duplicated(hackingdata, fromLast = TRUE), ]
hackingdata <- distinct(hackingdata)
nrow(hackingdata) #[211913]


#Missing Values

hackingdata %>%
  summarise(across(everything(), ~ sum(is.na(.))))

mcar_test(hackingdata) # p-value < 0.5 thus must impute

#DownTime [0 Missing Values]

#Ransom [159,820 Missing Values] 
#Since DownTime is a factor of Loss - And DownTime is never missing then there will always be loss
#But it does not mean the Ransom is always a factor as it could be 0
hackingdata <- hackingdata %>%
  mutate(Ransom = ifelse(is.na(Ransom), 0, Ransom))

#Loss [33,594 Missing Values] 
#Loss Mean = 2771.061
hackingdata$Loss[is.na(hackingdata$Loss)] <- mean(hackingdata$Loss, na.rm = TRUE)


#Outliers in Loss
Loss_Q1 <- quantile(hackingdata$Loss, 0.25)
Loss_Q3 <- quantile(hackingdata$Loss, 0.75)
Loss_IQR <- Loss_Q3 - Loss_Q1
Loss_LB <- Loss_Q1 - 1.5*Loss_IQR
Loss_UB <- Loss_Q3 + 1.5*Loss_IQR
Loss_Outliers <- hackingdata$Loss[hackingdata$Loss < Loss_LB | hackingdata$Loss > Loss_UB]

hackingdata$Loss <- ifelse(hackingdata$Loss > Loss_UB, Loss_UB, hackingdata$Loss)
hackingdata$Loss <- ifelse(hackingdata$Loss < Loss_LB, Loss_LB, hackingdata$Loss)