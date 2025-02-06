#Libraries Installed
library("readr") 
library("dplyr")  
library("tidyverse")
library("naniar") 
library("ggplot2") 
library("lubridate")
library("stringr")

# Read the CSV file "4.hackingdata.csv" from the current working directory
df <- read.csv("4.hackingdata.csv")

# Function to check for both NA and empty string values
missing_values <- colSums(is.na(df) | df == "")

# Convert to data frame for better readability
missing_values_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)




#Cleaning Ransom column 

#Checking for missing values in ransom. 
missing_ransom <- sum(is.na(df$Ransom) | df$Ransom == "")
print(missing_ransom)
#Converting to numeric by removing commas.
df$Ransom <- as.numeric(gsub(",", "", df$Ransom))  


#Cleaning Downtime column 

#Checking for missing values in downtime. 
missing_downtime <- sum(is.na(df$DownTime) | df$DownTime == "")
print(missing_downtime)
#Converting to numeric by removing commas.
df$DownTime <- as.numeric(gsub(",", "", df$DownTime))  


#Cleaning Loss column

#Checking for missing values in loss. 
missing_loss <- sum(is.na(df$Loss) | df$Loss == "")
print(missing_loss)
#Converting to numeric by removing commas.
df$Loss <- as.numeric(gsub(",", "", df$Loss))  