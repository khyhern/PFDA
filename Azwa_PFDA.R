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

