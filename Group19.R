#RYAN YEW KHY HERN, TP077536
# Objective 1: To analyze the impact of web server versions on website defacement frequency and severity.

# Analysis 1-1: What are the most commonly attacked web server versions?
# Analysis 1-2: Is there a correlation between outdated web servers and the frequency of website defacement?
# Analysis 1-3: Do outdated web servers experience higher ransom demands and downtime?
# Analysis 1-4: How does web server version distribution vary across different countries?
# Analysis 1-5: What is the trend of website defacement incidents based on web server versions over time?

# Load necessary libraries
library(dplyr)
library(stringr)
library(lubridate)

# Read the CSV file "4.hackingdata.csv" from the current working directory
df <- read.csv("4.hackingdata.csv")

# Function to check for both NA and empty string values
missing_values <- colSums(is.na(df) | df == "")

# Convert to data frame for better readability
missing_values_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)

# Filter Data from the Past 15 Years
# Step 1: Convert the 'Date' column to Date format (Automatically detects format)
df$Date <- parse_date_time(df$Date, orders = c("ymd", "dmy", "mdy", "d-b-Y"))

# Step 2: Define the cutoff date (January 1, 2010)
cutoff_date <- as.Date("2010-01-01")

# Step 3: Filter the dataset to keep only records from 2010 onwards
df_filtered <- df %>% filter(Date >= cutoff_date)

# Step 4: Verify the range of dates in the filtered dataset
summary(df_filtered$Date)
range(df_filtered$Date)  # Check if only data from 2010 onwards is included

# Cleaning & Validating the WebServer Column
# Step 1: Inspect WebServer Column
summary(df_filtered$WebServer)  # Check summary stats
unique(df_filtered$WebServer)   # Identify inconsistencies

# Step 1: Convert to Lowercase & Trim Spaces
df_filtered$WebServer <- tolower(str_trim(df_filtered$WebServer))

# Step 2: Extract the Base Web Server Name
df_filtered <- df_filtered %>%
  mutate(
    WebServer_Base = case_when(
      str_detect(WebServer, "apache") ~ "apache",
      str_detect(WebServer, "nginx") ~ "nginx",
      str_detect(WebServer, "microsoft-iis") ~ "microsoft-iis",
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
      TRUE ~ "other"  # Classify unknown values as 'other'
    ),
    
    # Step 3: Extract the Web Server Version
    WebServer_Version = str_extract(WebServer, "[0-9]+(\\.[0-9]+)*")
  )

# Step 4: Remove Junk or Invalid Entries
invalid_entries <- c("", "unknown", "noyb", "***************", "&quot;&quot;", "-", "webserver", "hosting", "generico web server v 5.46")
df_filtered <- df_filtered %>%
  filter(!(WebServer %in% invalid_entries) & !is.na(WebServer_Base))

# Step 5: Flag Outdated Web Servers (Example List)
outdated_versions <- c("apache/2.2", "nginx/1.14", "microsoft-iis/5.0", "microsoft-iis/6.0", "zeus/4.3")
df_filtered$IsOutdated <- ifelse(paste(df_filtered$WebServer_Base, df_filtered$WebServer_Version, sep = "/") %in% outdated_versions, "Yes", "No")

# Step 6: Verify Cleaned Data
unique(df_filtered$WebServer_Base)  # Check standardized names
unique(df_filtered$WebServer_Version)  # Check extracted versions
summary(df_filtered$WebServer_Base)
