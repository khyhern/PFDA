# Load necessary libraries
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(countrycode)  # For country name standardization
library(tidyverse)  # Data wrangling
library(janitor)  # Cleaning column names


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

# Step 5: Verify the range of dates in the filtered dataset
summary(df_filtered$Date)
range(df_filtered$Date)  # Check if only data from the past 15 years is included

# Cleaning & Validating the WebServer Column
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
    
    # Step 5: Extract the Web Server Version
    WebServer_Version = str_extract(WebServer, "[0-9]+(\\.[0-9]+)*")
  )

# Step 6: Verify Cleaned Data
unique(df_filtered$WebServer_Base)  # Check standardized names
unique(df_filtered$WebServer_Version)  # Check extracted versions
summary(df_filtered$WebServer_Base)

# Clean 'Country' column 
# https://rpubs.com/Teal_Emery/cleaning_intl_data_tips_and_tricks
# Helper function: Convert country name to ISO3C code
country_regex_to_iso3c <- function(country_string) {
  country_string %>%
    countrycode::countrycode(origin = "country.name", destination = "iso3c", origin_regex = TRUE)
}

# Helper function: Convert ISO3C back to standardized country name
iso3c_to_country_name <- function(country_string) {
  country_string %>%
    countrycode::countrycode(origin = "iso3c", destination = "country.name")
}

# Step 1: Standardize country names BEFORE applying countrycode
df_filtered <- df_filtered %>%
  mutate(Country = str_trim(Country)) %>%
  mutate(Country = case_when(
    # Handle completely unknown/missing values
    Country %in% c("", "Unknown", "UNKNOWN") ~ "Unknown",
    
    # Handle geographic regions and ambiguous names
    Country %in% c("Africa", "America", "Asia", "EUROPE", "SouthAmerica", "WestEuro", 
                   "EastEuro", "MiddleEast", "Oseania", "ASIA/PACIFIC REGION", 
                   "European Union", "European Uni") ~ "Unknown",
    
    # Handle invalid proxy/provider values
    Country %in% c("Anonymous Proxy", "ANONYMOUS PROXY", "Satellite Provider", "SATELLITE PROVIDER") ~ "Unknown",
    
    # Fix incorrect or non-standard country names
    Country == "T¸rkiye" ~ "Turkey",
    Country == "U.S.A" | Country == "Usa" ~ "United States",
    Country == "United State" ~ "United States",
    Country == "Uk" | Country == "United Kingd" ~ "United Kingdom",
    Country == "Uae" ~ "United Arab Emirates",
    Country == "MICRONESIA" ~ "Federated States of Micronesia",
    Country == "CALEDONIA" | Country == "New Caledoni" ~ "New Caledonia",
    Country %in% c("ASCENSIONISLAND", "Ascension Island") ~ "Saint Helena, Ascension and Tristan da Cunha",
    Country == "French Polyn" ~ "French Polynesia",
    Country == "Virgin Islan" | Country == "Virgin Islands" ~ "United States Virgin Islands",
    Country == "YUGOSLAVIA" ~ "Serbia",  # Yugoslavia no longer exists
    TRUE ~ Country
  ))

# Step 2: Apply ISO3C conversion and handle missing matches
df_filtered <- df_filtered %>%
  mutate(iso3c = country_regex_to_iso3c(Country)) %>%
  mutate(country_name = iso3c_to_country_name(iso3c)) %>%
  mutate(country_name = ifelse(is.na(country_name) | country_name == "", "Unknown", country_name))

# Display cleaned country column
unique(df_filtered$country_name)

#Cleaning 'Ransom' column 

#Checking for missing values in ransom. 
missing_ransom <- sum(is.na(df_filtered$Ransom) | df_filtered$Ransom == "")
print(missing_ransom)
#Converting to numeric by removing commas.
df_filtered$Ransom <- as.numeric(gsub(",", "", df_filtered$Ransom))  


#Cleaning 'Downtime' column 

#Checking for missing values in downtime. 
missing_downtime <- sum(is.na(df_filtered$DownTime) | df_filtered$DownTime == "")
print(missing_downtime)
#Converting to numeric by removing commas.
df_filtered$DownTime <- as.numeric(gsub(",", "", df_filtered$DownTime))  


#Cleaning 'Loss' column

#Checking for missing values in loss. 
missing_loss <- sum(is.na(df_filtered$Loss) | df_filtered$Loss == "")
print(missing_loss)
#Converting to numeric by removing commas.
df_filtered$Loss <- as.numeric(gsub(",", "", df_filtered$Loss))  

# Clean 'IP' column
# Step 1: Trim spaces
df_filtered <- df_filtered %>%
  mutate(IP = str_trim(IP))

# Step 2: Replace missing values (NA, empty, whitespace) with "Unknown"
df_filtered <- df_filtered %>%
  mutate(IP = ifelse(IP == "" | is.na(IP) | str_detect(IP, "^\\s*$"), "Unknown", IP))

# Step 3: Replace non-IP characters (anything not numbers, dots) with "Unknown"
df_filtered <- df_filtered %>%
  mutate(IP = ifelse(str_detect(IP, "[^0-9.]"), "Unknown", IP))



# Clean 'OS' column
# Step 1: Inspect the OS column to identify unique values and check for inconsistencies
# View unique OS entries
unique(df_filtered$OS)

# Check for missing values or empty entries in the OS column
sum(is.na(df_filtered$OS) | df_filtered$OS == "")

# Step 2: Clean the OS column
# Convert the OS column to lowercase and trim any extra spaces
df_filtered$OS <- tolower(str_trim(df_filtered$OS))

# Replace missing or empty values with "unknown"
df_filtered <- df_filtered %>% 
  mutate(OS = ifelse(is.na(OS) | OS == "", "unknown", OS))

# Step 3: Standardize OS Names
# Create a new column 'OS_Base' to classify the OS into categories
df_filtered <- df_filtered %>% 
  mutate(
    OS_Base = case_when(
      str_detect(OS, "windows") ~ "windows",    # Categorize Windows OS
      str_detect(OS, "linux") ~ "linux",        # Categorize Linux OS
      str_detect(OS, "mac") ~ "macos",          # Categorize MacOS
      str_detect(OS, "unix") ~ "unix",          # Categorize Unix
      str_detect(OS, "android") ~ "android",    # Categorize Android
      str_detect(OS, "ios") ~ "ios",            # Categorize iOS
      str_detect(OS, "bsd") ~ "bsd",            # Categorize BSD
      OS == "unknown" ~ "unknown",              # Preserve "unknown"
      TRUE ~ "other"                            # Classify anything else as "other"
    )
  )

# Step 4: Extract OS version numbers (if available)
# Use regular expressions to extract version numbers
df_filtered$OS_Version <- str_extract(df_filtered$OS, "[0-9]+(\\.[0-9]+)*")

# Step 5: Verify the cleaned data
# Check unique standardized OS categories
unique(df_filtered$OS_Base)

# Check unique extracted OS versions
unique(df_filtered$OS_Version)

# Summary of the OS_Base column
summary(df_filtered$OS_Base)

# Step 6: Final Check for missing or incorrect data
# Verify there are no unexpected missing values in OS or OS_Version
sum(is.na(df_filtered$OS) | df_filtered$OS == "")
sum(is.na(df_filtered$OS_Version))
