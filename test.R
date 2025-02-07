# Load necessary libraries
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(countrycode)  # For country name standardization
library(tidyverse)  # Data wrangling
library(janitor)  # Cleaning column names
library(readr)
library(zoo) # For rolling mean
library(tidyr)
library(caret)  # For machine learning-based imputation

# Read the CSV file "4.hackingdata.csv" from the current working directory
df <- read.csv("4.hackingdata.csv")

# Function to check for both NA and empty string values
missing_values <- colSums(is.na(df) | df == "")

# Convert to data frame for better readability
missing_values_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)

# Clean the 'Date' column 
# Step 1: Convert the 'Date' column to Date format (Automatically detects format)
df$Date <- parse_date_time(df$Date, orders = c("ymd", "dmy", "mdy", "d-b-Y"))

# Step 2: Determine the latest date in the dataset
latest_date <- max(df$Date, na.rm = TRUE)  # Find the most recent date in the dataset

# Step 3: Calculate the cutoff date (15 years before the latest recorded date)
cutoff_date <- latest_date - years(15)

# Step 4: Filter the dataset to keep only records from the past 15 years
df_filtered <- df %>% filter(Date >= cutoff_date)

# Step 5: Extract Year and Month for trend analysis
# Extract Year and Month as numeric values
df_filtered <- df_filtered %>%
  mutate(Year = year(Date),
         Month = month(Date))  # Ensure Month is numeric (1-12)

# Step 6: Verify the range of dates in the filtered dataset
summary(df_filtered$Date)
range(df_filtered$Date)  # Check if only data from the past 15 years is included
# Step 1: Inspect WebServer Column
summary(df_filtered$WebServer)  # Check summary stats
unique(df_filtered$WebServer)   # Identify inconsistencies

# Step 2: Convert to Lowercase & Trim Spaces
df_filtered$WebServer <- tolower(str_trim(df_filtered$WebServer))

# Step 3: Handle NA and Junk Values WITHOUT Removing Rows
df_filtered <- df_filtered %>%
  mutate(
    WebServer = ifelse(is.na(WebServer) | WebServer %in% c("", "noyb", "***************", "&quot;&quot;", "-", "+"), "unknown", WebServer),
    WebServer = ifelse(WebServer %in% c("webserver", "hosting", "generico web server v 5.46"), "other", WebServer)
  )

# Step 4: Standardize Common Web Server Names (While Keeping Versions)
df_filtered <- df_filtered %>%
  mutate(
    WebServer_Base = case_when(
      str_detect(WebServer, "apache") ~ "apache",
      str_detect(WebServer, "nginx") ~ "nginx",
      str_detect(WebServer, "iis") ~ "microsoft-iis",
      str_detect(WebServer, "lighttpd") ~ "lighttpd",
      str_detect(WebServer, "gws") ~ "google web server",
      str_detect(WebServer, "litespeed") ~ "litespeed",
      str_detect(WebServer, "tomcat") ~ "apache tomcat",
      str_detect(WebServer, "oracle") ~ "oracle server",
      str_detect(WebServer, "zeus") ~ "zeus",
      str_detect(WebServer, "ats") ~ "ats",
      str_detect(WebServer, "varnish") ~ "varnish",
      str_detect(WebServer, "cherrypy") ~ "cherrypy",
      str_detect(WebServer, "ideawebserver") ~ "ideawebserver",
      str_detect(WebServer, "bigip") ~ "bigip",
      str_detect(WebServer, "sun-java-system-web-server") ~ "sun-java-web-server",
      str_detect(WebServer, "modsecurity") ~ "modsecurity",
      str_detect(WebServer, "cloudflare-nginx") ~ "cloudflare",
      WebServer == "unknown" ~ "unknown",  # Preserve "unknown" entries
      TRUE ~ "other"  # Classify everything else as "other"
    ),
    
    # Extract Web Server Version
    WebServer_Version = str_extract(WebServer, "[0-9]+(\\.[0-9]+)*")
  )

# Combine WebServer_Base and WebServer_Version
df_filtered <- df_filtered %>%
  mutate(WebServer_Full = ifelse(is.na(WebServer_Version) | WebServer_Version == "Unknown",
                                 WebServer_Base,  
                                 paste(WebServer_Base, WebServer_Version)))

# Step 5: Infer Missing WebServer Based on OS
df_filtered <- df_filtered %>%
  mutate(
    WebServer_Base = case_when(
      WebServer_Base == "unknown" & OS == "windows" ~ "microsoft-iis",
      WebServer_Base == "unknown" & OS == "linux" ~ "apache",
      WebServer_Base == "unknown" & OS == "bsd" ~ "nginx",
      TRUE ~ WebServer_Base
    )
  )

# Step 6: Predict Missing WebServer Using Machine Learning
# Prepare data for prediction
df_train <- df_filtered %>% filter(WebServer_Base != "unknown")  # Known web servers
df_test <- df_filtered %>% filter(WebServer_Base == "unknown")   # Missing values

if (nrow(df_test) > 0) {
  set.seed(123)
  
  # Use OS, Country, and other features for prediction
  train_data <- df_train %>%
    select(OS, Country, Encoding, Lang, WebServer_Base)
  
  test_data <- df_test %>%
    select(OS, Country, Encoding, Lang)
  
  # Convert to factors for classification
  train_data <- train_data %>% mutate(across(everything(), as.factor))
  test_data <- test_data %>% mutate(across(everything(), as.factor))
  
  # Train a classification model (Random Forest)
  model <- train(WebServer_Base ~ ., data = train_data, method = "rf", trControl = trainControl(method = "cv", number = 5))
  
  # Predict missing WebServer values
  predictions <- predict(model, test_data)
  
  # Assign predictions to df_filtered
  df_filtered$WebServer_Base[df_filtered$WebServer_Base == "unknown"] <- as.character(predictions)
}

# Step 7: Verify Cleaned Data
unique(df_filtered$WebServer_Base)  # Check standardized names
summary(df_filtered$WebServer_Base)