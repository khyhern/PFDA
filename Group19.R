# RYAN YEW KHY HERN, TP077536
# LARA DOMINIQUE ISIDRO CASTRO, TP072345 
# NARINA KAUR SIDHU, TP079398
# AZWA AL ISLAM, TP078098

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
library(scales)
library(forcats)              # For handling factors in a cleaner way
library(corrplot)             # For creating correlation points
library(RColorBrewer)         # For color palettes
library(viridis)  # For color scales


# Read the CSV file "4.hackingdata.csv" from the current working directory
df <- read.csv("4.hackingdata.csv")

# Function to check for both NA and empty string values
missing_values <- colSums(is.na(df) | df == "")

# Convert to data frame for better readability
missing_values_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)

#DATA CLEANING
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

# Step 4: Count "Unknown" Web Server Cases Before Imputation
unknown_before <- df_filtered %>% filter(str_to_lower(WebServer) == "unknown") %>% nrow()
loss_unknown_before <- df_filtered %>% filter(str_to_lower(WebServer) == "unknown") %>% summarise(sum(Loss, na.rm = TRUE))

print(paste("Before Imputation - Unknown Web Server Count:", unknown_before))
print(paste("Before Imputation - Financial Loss from Unknowns: $", loss_unknown_before))

# Step 5: Impute "Unknown" WebServer Names Using OS Information
df_filtered <- df_filtered %>%
  mutate(WebServer = case_when(
    str_to_lower(WebServer) == "unknown" & str_detect(str_to_lower(OS), "windows") ~ "microsoft-iis",
    str_to_lower(WebServer) == "unknown" & str_detect(str_to_lower(OS), "linux") ~ "apache",
    str_to_lower(WebServer) == "unknown" & str_detect(str_to_lower(OS), "bsd") ~ "nginx",
    TRUE ~ WebServer  # Keep original values
  ))

# Step 6: Count "Unknown" Web Server Cases After Imputation
unknown_after <- df_filtered %>% filter(str_to_lower(WebServer) == "unknown") %>% nrow()
loss_unknown_after <- df_filtered %>% filter(str_to_lower(WebServer) == "unknown") %>% summarise(sum(Loss, na.rm = TRUE))

print(paste("After Imputation - Unknown Web Server Count:", unknown_after))
print(paste("After Imputation - Financial Loss from Remaining Unknowns: $", loss_unknown_after))

# Step 7: Reapply Standardization & Version Extraction AFTER Imputation
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
    )
  )

# Step 8: Extract the Web Server Version (Ensuring it Reflects Imputed Values)
df_filtered <- df_filtered %>%
  mutate(
    WebServer_Version = str_extract(WebServer, "(?<=\\b)[0-9]+(\\.[0-9]+)*"),
    WebServer_Version = na_if(WebServer_Version, "unknown")  # Convert 'unknown' to NA for proper handling
  )

# Step 9: Recreate the Full Web Server Label AFTER Imputation
df_filtered <- df_filtered %>%
  mutate(
    WebServer_Full = case_when(
      is.na(WebServer_Version) ~ WebServer_Base,  # Keep only base if version is missing
      TRUE ~ paste(WebServer_Base, WebServer_Version)  # Otherwise, use full version
    )
  )

# Step 10: Check for Multi-Version Entries (Remove or Handle Separately)
df_filtered <- df_filtered %>%
  filter(!str_detect(WebServer, "&|,|;"))  # Remove multi-server cases if needed

# Step 11: Verify Cleaned Data
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

# 1. Checking for missing values or empty strings in the 'Ransom' column
missing_ransom_before <- sum(is.na(df_filtered$Ransom) | df_filtered$Ransom == "")
print(paste("Missing Ransom values before cleaning:", missing_ransom_before))

# 2. Convert 'Ransom' column to numeric by removing commas and handling characters
df_filtered <- df_filtered %>%
  mutate(Ransom = as.character(Ransom),  # Ensure it's character before replacing commas
         Ransom = gsub(",", "", Ransom),  # Remove commas
         Ransom = as.numeric(Ransom))     # Convert to numeric

# 3. Impute Missing Values by Mean
df_filtered <- df_filtered %>%
  mutate(Ransom = ifelse(is.na(Ransom), mean(Ransom, na.rm = TRUE), Ransom))

# 4. Checking for remaining missing values after imputation
missing_ransom_after <- sum(is.na(df_filtered$Ransom))
print(paste("Missing Ransom values after cleaning:", missing_ransom_after))  


#Cleaning 'Downtime' column 

#Checking for missing values in downtime. 
missing_downtime <- sum(is.na(df_filtered$DownTime) | df_filtered$DownTime == "")
print(missing_downtime)
#Converting to numeric by removing commas.
df_filtered$DownTime <- as.numeric(gsub(",", "", df_filtered$DownTime))  


#Cleaning 'Loss' column

# 1. Checking for missing values or empty strings in the 'Loss' column
missing_loss_before <- sum(is.na(df_filtered$Loss) | df_filtered$Loss == "")
print(paste("Missing Loss values before cleaning:", missing_loss_before))

# 2. Convert 'Loss' column to numeric by removing commas and handling characters
df_filtered <- df_filtered %>%
  mutate(Loss = as.character(Loss),  # Ensure it's character before replacing commas
         Loss = gsub(",", "", Loss),  # Remove commas
         Loss = as.numeric(Loss))     # Convert to numeric

# 3. Impute Missing Values by Median Imputation per Web Server Group
df_filtered <- df_filtered %>%
  group_by(WebServer_Base) %>%
  mutate(Loss = ifelse(is.na(Loss), ifelse(all(is.na(Loss)), 0, median(Loss, na.rm = TRUE)), Loss)) %>%
  ungroup()  # Remove grouping after imputation

# 4. Checking for remaining missing values after imputation
missing_loss_after <- sum(is.na(df_filtered$Loss))
print(paste("Missing Loss values after cleaning:", missing_loss_after))

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

# Clean 'Encoding' column
# Replace "NULL" and empty values with NA
df_filtered$Encoding <- ifelse(df_filtered$Encoding %in% c("NULL", "", NA), NA, df_filtered$Encoding)

# Standardize encoding names
df_filtered$Encoding <- tolower(df_filtered$Encoding)  # Convert to lowercase
df_filtered$Encoding <- gsub("utf-8", "UTF-8", df_filtered$Encoding)
df_filtered$Encoding <- gsub("iso-8859-1", "ISO-8859-1", df_filtered$Encoding)
df_filtered$Encoding <- gsub("windows-1252", "Windows-1252", df_filtered$Encoding)
df_filtered$Encoding <- gsub("gb2312", "GB2312", df_filtered$Encoding)
df_filtered$Encoding <- gsub("big5", "BIG5", df_filtered$Encoding)

# Fill missing values with "Unknown"
df_filtered$Encoding[is.na(df_filtered$Encoding)] <- "Unknown"

# Check results
table(df_filtered$Encoding)

# Save cleaned dataset
write.csv(df_filtered, "cleaned_hackingdata.csv", row.names = FALSE)

#DATA ANALYSIS
# Ryan Yew Khy Hern, TP077536
# Objective 1: To investigate the relationship between web server and financial loss 
# Analysis 1-1: Web Server Versions Associated with the Highest Revenue Loss
# Identify which web server versions are linked to the most significant revenue losses due to defacement.

# Check unique standardized web server names and versions
unique(df_filtered$WebServer_Base)  
unique(df_filtered$WebServer_Version)

# Step 1: Recalculate Aggregation Without "Unknown"
total_loss_all <- sum(df_filtered$Loss, na.rm = TRUE)  # Compute total loss across all web servers

webserver_grouped <- df_filtered %>%
  filter(str_to_lower(WebServer) != "unknown") %>%
  group_by(WebServer) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    Percentage_Loss = (Total_Loss / total_loss_all) * 100,  # Calculate % contribution
    Median_Loss = median(Loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Loss))

# Step 2: Display the Top 10 Web Server Versions by Total Loss
top_webservers <- webserver_grouped %>% slice_max(Total_Loss, n = 10)
print(top_webservers)

# Step 3: Visualize the Data (Including Percentage Loss)
ggplot(top_webservers, aes(x = reorder(WebServer, Total_Loss), y = Total_Loss)) +
  geom_col(fill = "red", width = 0.7) +  # Improve bar spacing
  coord_flip() +  # Flip for better readability
  labs(title = "Top 10 Web Server Versions by Total Revenue Loss",
       x = "Web Server Version",
       y = "Total Loss (USD)") +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(top_webservers$Total_Loss) * 1.1)) +  # Adjust limit dynamically
  geom_text(aes(label = paste0(scales::comma(Total_Loss), " (", round(Percentage_Loss, 2), "%)")), 
            hjust = -0.1, size = 4, color = "black", fontface = "bold") +  # Display both total loss & percentage
  theme_minimal(base_size = 12) +  # Adjust font size
  theme(plot.title = element_text(hjust = 0.5))  # Center title

# Analysis 1-2: Assessing the Correlation Between Web Server Versions and Average Financial Loss Per Incident
# Calculate the average revenue loss per incident for each web server version and analyze correlation trends.

# 1. Aggregate data: Calculate average loss per incident for each web server version
webserver_avg_loss <- df_filtered %>%
  group_by(WebServer_Full) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    Incident_Count = n(),
    Avg_Loss_Per_Incident = mean(Loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Incident_Count >= 5)  # Remove web servers with very few incidents to avoid bias

# 2. Compute correlation matrix (ensuring no missing values)
correlation_data <- webserver_avg_loss %>%
  select(Avg_Loss_Per_Incident, Incident_Count, Total_Loss) %>%
  na.omit()  # Remove missing values before correlation analysis

correlation_matrix <- cor(correlation_data, use = "complete.obs", method = "pearson")
print("Correlation Matrix:")
print(correlation_matrix)

# 3. Scatter plot: Avg Loss per Incident vs Incident Count
ggplot(webserver_avg_loss, aes(x = Incident_Count, y = Avg_Loss_Per_Incident)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10() +  # Log scale for better visibility
  scale_y_log10() +  # Log scale to prevent extreme values dominating the plot
  labs(title = "Avg Loss per Incident vs. Incident Count",
       subtitle = paste("Correlation:", round(correlation_matrix["Incident_Count", "Avg_Loss_Per_Incident"], 3)),
       x = "Number of Incidents (Log Scale)",
       y = "Average Loss per Incident (Log Scale)") +
  theme_minimal()

# 4. Scatter plot: Total Loss vs Avg Loss per Incident
ggplot(webserver_avg_loss, aes(x = Total_Loss, y = Avg_Loss_Per_Incident)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10() +  # Log scale for better visualization
  scale_y_log10() +
  labs(title = "Total Loss vs Avg Loss per Incident",
       subtitle = paste("Correlation:", round(correlation_matrix["Total_Loss", "Avg_Loss_Per_Incident"], 3)),
       x = "Total Loss (USD, Log Scale)",
       y = "Average Loss per Incident (Log Scale)") +
  theme_minimal()

# Analysis 1-3: Web Server Version vs. Downtime Impact on Financial Loss
# Investigate whether specific web server versions contribute to longer downtimes and how that affects financial loss.

# 1. Aggregate downtime impact per Web Server Version
webserver_downtime <- df_filtered %>%
  group_by(WebServer_Full) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    Total_Downtime = sum(DownTime, na.rm = TRUE),
    Avg_Downtime = mean(DownTime, na.rm = TRUE),
    Incident_Count = n(),
    Avg_Loss_Per_Incident = mean(Loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Incident_Count >= 5)  # Remove web servers with very few incidents to avoid bias

# 2. Compute correlation matrix (ensuring proper NA handling)
correlation_data <- webserver_downtime %>%
  select(Avg_Downtime, Total_Downtime, Total_Loss, Avg_Loss_Per_Incident, Incident_Count) %>%
  na.omit()  # Remove missing values before correlation analysis

correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")
print("Correlation Matrix between DownTime and Financial Loss:")
print(correlation_matrix)

# 3. Scatter plot: Total DownTime vs. Financial Loss
ggplot(webserver_downtime, aes(x = Total_Downtime, y = Total_Loss)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Adjust transparency & size
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10(labels = scales::comma) +  # Log scale for better visualization
  scale_y_log10(labels = scales::comma) +
  labs(title = "Total DownTime vs. Total Financial Loss",
       subtitle = paste("Correlation:", round(correlation_matrix["Total_Downtime", "Total_Loss"], 3)),
       x = "Total DownTime (Log Scale, Days)",
       y = "Total Loss (Log Scale, USD)") +
  theme_minimal()

# 4. Scatter plot: Average Downtime vs. Average Loss per Incident
ggplot(webserver_downtime, aes(x = Avg_Downtime, y = Avg_Loss_Per_Incident)) +
  geom_point(color = "green", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10(labels = scales::comma) +  # Log scale to prevent extreme values dominating the plot
  scale_y_log10(labels = scales::comma) +
  labs(title = "Avg Downtime vs. Avg Loss per Incident",
       subtitle = paste("Correlation:", round(correlation_matrix["Avg_Downtime", "Avg_Loss_Per_Incident"], 3)),
       x = "Average Downtime (Log Scale, Days)",
       y = "Average Loss per Incident (Log Scale, USD)") +
  theme_minimal()

# 5. Identify Top 10 Web Server Versions with the Longest Downtimes
top_downtime_webservers <- webserver_downtime %>%
  arrange(desc(Total_Downtime)) %>%
  slice_max(Total_Downtime, n = 10)

print("Top 10 Web Server Versions with the Longest Downtimes:")
print(top_downtime_webservers)

# 6. Bar Plot for Top 10 Web Servers with the Longest Downtime (Improved)
ggplot(top_downtime_webservers, aes(x = reorder(WebServer_Full, Total_Downtime), y = Total_Downtime)) +
  geom_col(fill = "purple", width = 0.7) +
  coord_flip() +  # Flip for better label visibility
  geom_text(aes(label = scales::comma(Total_Downtime)), 
            hjust = -0.1,  # Move text to the right of bars
            color = "black", fontface = "bold", size = 4) +  # Make text clear
  scale_y_continuous(labels = scales::comma, limits = c(0, 4500000)) +  # Set y-axis max to 4,500,000
  labs(title = "Top 10 Web Server Versions with the Longest Downtime",
       x = "Web Server Version",
       y = "Total Downtime (Days)") +
  theme_minimal()

# Extra Feature 1-3: Categorizing Web Servers by Downtime Severity
# -----------------------------------------------------------
# This extra feature enhances the analysis by grouping web servers into three downtime severity categories:
# - Low Downtime: < 100,000 total downtime
# - Moderate Downtime: 100,000 - 499,999 total downtime
# - High Downtime: ≥ 500,000 total downtime
# By categorizing the data, we can compare the financial impact of different downtime levels more effectively.

webserver_downtime <- webserver_downtime %>%
  mutate(Downtime_Category = factor(case_when(
    Total_Downtime < 100000 ~ "Low Downtime",
    Total_Downtime >= 100000 & Total_Downtime < 500000 ~ "Moderate Downtime",
    Total_Downtime >= 500000 ~ "High Downtime"
  ), levels = c("Low Downtime", "Moderate Downtime", "High Downtime")))  # Set order for better visualization

# Define Custom Colors for Clarity
# -------------------------------
# Custom color mapping improves readability by making each downtime category visually distinct.
downtime_colors <- c("Low Downtime" = "#2ca25f", "Moderate Downtime" = "#2b8cbe", "High Downtime" = "#de2d26")

# Visualization: Downtime Severity vs. Total Loss
# -----------------------------------------------
# This box plot compares total financial loss across different downtime severity categories.
# It helps identify whether higher downtime results in significantly greater financial losses.
ggplot(webserver_downtime, aes(x = Downtime_Category, y = Total_Loss, fill = Downtime_Category)) +
  geom_boxplot(alpha = 0.7, outlier.color = "black", outlier.shape = 16, outlier.size = 1.5) +
  scale_fill_manual(values = downtime_colors) +  # Apply custom colors
  scale_y_log10(labels = scales::comma) +  # Log scale to handle large variations in financial loss
  labs(title = "Financial Loss Across Downtime Severity Categories",
       x = "Downtime Severity",
       y = "Total Financial Loss (Log Scale, USD)") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend for cleaner visualization
        axis.text.x = element_text(size = 12, face = "bold"),  # Improve x-axis readability
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))  # Center title for aesthetics



# Analysis 1-4: Web Server Vulnerability Trend Over Time
# Analyze the historical trend of web server vulnerabilities by tracking which versions have been defaced most frequently over time.

# 1. Aggregate incidents per Web Server Version over time (grouped by year-month)
webserver_trend <- df_filtered %>%
  group_by(Year, Month, WebServer_Full) %>%
  summarise(
    Incident_Count = n(),
    .groups = "drop"
  ) %>%
  mutate(Date = ymd(paste(Year, Month, "01")))  # Convert Year-Month to Date format

# 2. Identify the Top 5 Most Frequently Attacked Web Server Versions
top_vulnerable_webservers <- webserver_trend %>%
  group_by(WebServer_Full) %>%
  summarise(Total_Incidents = sum(Incident_Count)) %>%
  arrange(desc(Total_Incidents)) %>%
  slice_head(n = 5) %>%  
  pull(WebServer_Full)

# 3. Filter dataset to include only the top 5 vulnerable web servers
webserver_trend_top <- webserver_trend %>%
  filter(WebServer_Full %in% top_vulnerable_webservers) %>%
  arrange(Date)

# 4. Apply a Rolling Mean (Moving Average) to Smooth Trends (12-Month Window)
webserver_trend_top <- webserver_trend_top %>%
  group_by(WebServer_Full) %>%
  mutate(Smoothed_Incidents = zoo::rollmean(Incident_Count, k = 12, fill = NA, align = "right"))

# 5. Remove NA values from the smoothed dataset before plotting
webserver_trend_top <- webserver_trend_top %>%
  filter(!is.na(Smoothed_Incidents))

# 6. Define a Color Palette for Stronger Contrast
custom_colors <- c("apache" = "red", "microsoft-iis" = "gold", 
                   "microsoft-iis 6.0" = "green", "nginx" = "blue", 
                   "unknown" = "purple")

# 7. Adjust Y-Axis Scaling to Avoid Large Empty Spaces
y_max <- max(webserver_trend_top$Smoothed_Incidents, na.rm = TRUE) * 1.1

# 8. Plot the Smoothed Time Series with Enhanced Formatting
ggplot(webserver_trend_top, aes(x = Date, y = Smoothed_Incidents, color = WebServer_Full)) +
  geom_line(size = 1.2) +  # Slightly thicker lines
  geom_point(alpha = 0.5, size = 2) +  # Points for context
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 years") +  # Format x-axis with 2-year breaks
  scale_y_continuous(limits = c(0, y_max), labels = scales::comma) +  # Adjust y-axis scaling
  scale_color_manual(values = custom_colors) +  # Use predefined colors
  labs(title = "Web Server Vulnerability Trend Over Time (Smoothed)",
       x = "Time (Year-Month)",
       y = "Number of Defacements (Smoothed)",
       color = "Web Server Version") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "bottom",  # Move legend below chart
        legend.title = element_blank(),  # Remove legend title for clarity
        panel.grid.major = element_blank())  # Remove major grid lines

# Analysis 1-5: Financial Loss Distribution Across Different Web Server Types
# Categorize web servers (e.g., Apache, Nginx, IIS, etc.) and analyze the distribution of financial losses to determine which type is the most financially impacted.

# Aggregate financial loss per Web Server Type (WebServer_Base)
webserver_loss <- df_filtered %>%
  group_by(WebServer_Base) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),  # Sum should now work correctly
    Incident_Count = n(),
    Avg_Loss = mean(Loss, na.rm = TRUE),
    Median_Loss = median(Loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Loss))  # Sort by highest financial impact

# Select top 10 web servers by financial impact
top_webserver_loss <- head(webserver_loss, 10)

# 1. Define Y-Axis Limit (Extend by 10% for better spacing)
y_max <- max(top_webserver_loss$Total_Loss, na.rm = TRUE) * 5

# 2. Log Scale Bar Chart - Total Loss per Web Server Type
ggplot(top_webserver_loss, aes(x = reorder(WebServer_Base, Total_Loss), y = Total_Loss)) +
  geom_col(fill = "red", width = 0.7) +  # Restore original bar width
  geom_text(aes(label = scales::comma(Total_Loss)), 
            hjust = -0.1, size = 4, color = "black", fontface = "bold") +  # Restore standard label positioning
  coord_flip() +  # Flip for better readability
  scale_y_log10(labels = scales::comma, breaks = scales::log_breaks(n = 5), limits = c(1, y_max)) +  # Log scale on Y-axis
  labs(title = "Top 10 Web Server Types by Total Financial Loss",
       x = "Web Server Type",
       y = "Total Loss (Log Scale, USD)") +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "gray", linetype = "dashed"))  # Keep grid lines for readability

# 3. Box Plot - Financial Loss Distribution Across Web Server Types (Log Scale)
ggplot(df_filtered, aes(x = WebServer_Base, y = Loss)) +
  geom_boxplot(fill = "blue", alpha = 0.7, outlier.color = "red", outlier.shape = 16, outlier.size = 1.5) +
  coord_flip() +  # Flip for better readability
  scale_y_log10(labels = scales::comma, limits = c(10, max(df_filtered$Loss, na.rm = TRUE) * 1.5)) +  # Adjust log scale range
  labs(title = "Financial Loss Distribution Across Web Server Types",
       x = "Web Server Type",
       y = "Financial Loss (Log Scale, USD)") +
  theme_minimal()




# Lara Dominique Isidro Castro, TP072345
# Objective 2: To investigate the influence of server operating systems and web server types on hacking vulnerability and financial loss.


# Analysis 2-1: Which server operating systems are most commonly associated with hacking incidents?

# Counts the number of hacking incidents for each server OS
os_counts <- df_filtered %>%
  count(OS_Base, sort = TRUE)

# Prints the OS counts in descending order
print(os_counts)

# Plots the most commonly hacked server OS using a bar chart
ggplot(os_counts, aes(x = reorder(OS_Base, -n), y = n, fill = OS_Base)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Most Commonly Hacked Server Operating Systems (OS)", fill = "Server OS",  
       x = "Server Operating System", y = "Number of Hacking Incidents") +
  scale_y_log10(labels = scales::comma) +                     # Uses a logarithmic scale for the y-axis for better visualization of results
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +                                           # Changes the plot theme for a cleaner and more modern look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))    # Rotates x-axis labels for better readability


# Analysis 2-2: Is there a significant correlation between specific server operating systems and financial loss?

# Computes the average financial loss for each server OS
os_loss <- df_filtered %>%
  group_by(OS_Base) %>%
  summarise(Average_Loss = mean(Loss, na.rm = TRUE)) %>%
  arrange(desc(Average_Loss))

# Prints the average financial loss for each server OS
print(os_loss)

# Checks for missing or non-finite values in the "Loss" column
sum(is.na(df_filtered$Loss))
sum(!is.finite(df_filtered$Loss)) # Keeps only rows with finite Loss values

# Filters data to exclude rows with non-finite financial losses
df_filtered_clean <- df_filtered %>%
  filter(is.finite(Loss))  # Keeps only rows with finite Loss values

# Shows the distribution of financial losses for each OS using boxplots
ggplot(df_filtered_clean, aes(x = OS_Base, y = Loss, fill = OS_Base)) +
  geom_boxplot() +
  geom_text(stat = "summary", fun = median, aes(label = round(..y.., 1)), color = "black", size = 3, vjust = -1) +
  labs(title = "Financial Loss by Server Operating System (OS)",  fill = "Server OS",
       x = "Server Operating System", y = "Financial Loss") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 2-3: How does the distribution of downtime vary across different server operating systems?

# Computes the average downtime for each server operating system
os_downtime <- df_filtered %>%
  group_by(OS_Base) %>%
  summarise(Average_Downtime = mean(DownTime, na.rm = TRUE),
            Median_Downtime = median(DownTime, na.rm = TRUE),
            Max_Downtime = max(DownTime, na.rm = TRUE)) %>%
  arrange(desc(Average_Downtime))

# Prints the average, median, and max downtime for each server OS
print(os_downtime)

# Plots the distribution of downtime for each server OS using boxplots
ggplot(df_filtered, aes(x = OS_Base, y = DownTime, fill = OS_Base)) +
  geom_boxplot() +
  geom_text(stat = "summary", fun = median, aes(label = round(..y.., 1)), color = "black", size = 3, vjust = -1) +
  labs(title = "Distribution of Downtime by Server Operating System (OS)", fill = "Server OS",
       x = "Server Operating System", y = "Downtime (Days)") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 2-4: What is the variability in financial loss associated with different web server types?

# Computes summary statistics (average, median, max, and standard deviation) for financial losses by web server type
web_loss_variability <- df_filtered_clean %>%
  group_by(WebServer_Base) %>%
  summarise(Average_Loss = mean(Loss, na.rm = TRUE),
            Median_Loss = median(Loss, na.rm = TRUE),
            Max_Loss = max(Loss, na.rm = TRUE),
            SD_Loss = sd(Loss, na.rm = TRUE)) %>%
  arrange(desc(Average_Loss))

# Prints the summary statistics for financial loss by web server type
print(web_loss_variability)

# Plots the distribution of financial losses for each web server type using a scatter plot
ggplot(df_filtered_clean, aes(x = WebServer_Base, y = Loss, color = WebServer_Base)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  # Adds jitter to avoid overlapping points
  geom_text(stat = "summary", fun = median, aes(label = round(..y.., 1)), color = "black", size = 3, vjust = -1) +
  labs(title = "Variability of Financial Loss by Web Server Type", color = "Web Server Type",
       x = "Web Server Type", y = "Financial Loss") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 2-5: Do certain server OS and web server combinations show a higher risk of hacking vulnerability?

# Computes the frequency of hacking incidents for each combination of server OS and web server type
os_web_counts <- df_filtered %>%
  count(OS_Base, WebServer_Base, sort = TRUE)

# Prints the frequency of hacking incidents for each OS-Web Server combination
print(os_web_counts)

# Visualizes the frequency of hacking incidents for each OS-Web Server combination using a heatmap
ggplot(os_web_counts, aes(x = OS_Base, y = WebServer_Base, fill = n)) +
  geom_tile() +   # Each cell represents the frequency of incidents for a combination
  geom_text(aes(label = n), color = "white", size = 3) +
  labs(title = "Hacking Incidents by Server OS and Web Server", fill = "Number of Incidents",
       x = "Server OS", y = "Web Server Type") +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Extra Feature 1: Using geom_jitter() instead of geom_point() for the Scatter Plot
# The use of geom_jitter() in Analysis 2-4 enhances visualization by introducing random noise (jitter) to overlapping points. It
# ensures that points with similar financial losses do not overlap and are easier to interpret.

# Extra Feature 2: Using a Heatmap (geom_tile) for OS-Web Server Combinations
# The heatmap visualization using `geom_tile()` in Analysis 2-5 allows for a comparative analysis of hacking incidents across 
# combinations of server operating systems and web server types. The heatmap provides a quick visual reference to identify high-risk 
# combinations, enhancing interpretability and aiding decision-makers in prioritizing security efforts.




# Narina Kaur Sidhu, TP079398 
# Objective 3: To investigate the relationship between ransom demands and encoding methods on financial loss. 

##### Analysis 3-1: What is the relationship between ransom demands and financial loss? #####
# Categorizing Ransom Amounts
df_filtered <- df_filtered %>%
  mutate(ransom_category = cut(
    Ransom,
    breaks = c(-Inf, 1546, 2300, Inf),
    labels = c("Small", "Medium", "High")
  ))

# Display summary statistics for Ransom
summary(df_filtered$Ransom)

# Boxplot of Loss by Ransom Category
ggplot(df_filtered, aes(x = ransom_category, y = Loss, fill = ransom_category)) +
  geom_boxplot(color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 0)), 
               color = "red", vjust = -0.5, size = 3) +
  labs(title = "Loss by Ransom Category",
       x = "Ransom Category",
       y = "Loss") +
  scale_fill_manual(values = c("Small" = "lightblue", "Medium" = "lightgreen", "High" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none")

# Bar Chart of Average Loss by Ransom Category
print(
  ggplot(df_filtered, aes(x = ransom_category, y = Loss, fill = ransom_category)) +
    stat_summary(fun = mean, geom = "bar", color = "black") +
    stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 0)), 
                 color = "red", vjust = -0.5, size = 3) +
    labs(title = "Average Loss by Ransom Category",
         x = "Ransom Category",
         y = "Average Loss") +
    scale_fill_manual(values = c("Small" = "lightblue", "Medium" = "lightgreen", "High" = "lightcoral")) +
    theme_minimal() +
    theme(legend.position = "none")
)


##### Analysis 3-2: Do specific encoding methods result in higher financial losses? #####

# Bar plot for all encoding methods
ggplot(df_filtered, aes(x = Encoding, y = Loss)) +
  stat_summary(fun = "mean", geom = "bar", fill = "cadetblue3", color = "black") +
  labs(
    title = "Average Financial Loss by Encoding Method",
    x = "Encoding Method",
    y = "Average Financial Loss ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

# Group encoding methods into broader categories
df_filtered <- df_filtered %>%
  mutate(Encoding_Grouped = case_when(
    str_detect(Encoding, "windows") ~ "Windows",
    str_detect(Encoding, "utf") ~ "UTF",
    str_detect(Encoding, "iso|ISO") ~ "ISO",
    str_detect(Encoding, "ascii") ~ "ASCII",
    str_detect(Encoding, "big5|BIG5") ~ "Big5",
    str_detect(Encoding, "euc") ~ "EUC",
    str_detect(Encoding, "gb2312|GB2312") ~ "GB",
    str_detect(Encoding, "koi8-r") ~ "KOI",
    str_detect(Encoding, "tis|TIS") ~ "TIS",
    str_detect(Encoding, "shift_jis") ~ "Shift-JIS",
    str_detect(Encoding, "litespeed") ~ "LiteSpeed",
    str_detect(Encoding, "Unknown") ~ "Unknown",
    TRUE ~ "Other"
  ))

# Bar plot for grouped encoding methods
ggplot(df_filtered, aes(x = Encoding_Grouped, y = Loss)) +
  stat_summary(fun = "mean", geom = "bar", fill = "cadetblue3", color = "black") +
  labs(
    title = "Average Financial Loss by Encoding Method (Grouped)",
    x = "Encoding Method (Grouped)",
    y = "Average Financial Loss ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels


##### Analysis 3-3: How does the frequency of different encoding methods vary across ransom attacks? #####
# Group encoding methods into broader categories
df_filtered <- df_filtered %>%
  mutate(Encoding_Grouped = case_when(
    str_detect(tolower(Encoding), "windows") ~ "Windows",
    str_detect(tolower(Encoding), "utf") ~ "UTF",
    str_detect(tolower(Encoding), "iso") ~ "ISO",
    str_detect(tolower(Encoding), "ascii") ~ "ASCII",
    str_detect(tolower(Encoding), "big5") ~ "Big5",
    str_detect(tolower(Encoding), "euc") ~ "EUC",
    str_detect(tolower(Encoding), "gb2312") ~ "GB",
    str_detect(tolower(Encoding), "koi") ~ "KOI",
    str_detect(tolower(Encoding), "tis") ~ "TIS",
    str_detect(tolower(Encoding), "shift_jis") ~ "Shift-JIS",
    str_detect(tolower(Encoding), "litespeed") ~ "LiteSpeed",
    str_detect(tolower(Encoding), "unknown") ~ "Unknown",
    TRUE ~ "Other"
  ))

# Count the frequency of each encoding category
encoding_grouped_frequency <- df_filtered %>%
  group_by(Encoding_Grouped) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Percentage = Count / sum(Count) * 100,  # Calculate percentage
         Label = paste0(Encoding_Grouped, "\n", Count, " (", round(Percentage, 1), "%)"))  # Create label text

# Display the frequency table
print(encoding_grouped_frequency, n = 13)

# Visualize the frequency distribution for grouped encoding categories using a pie chart
ggplot(encoding_grouped_frequency, aes(x = "", y = Count, fill = Encoding_Grouped)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to pie chart
  labs(
    title = "Proportion of Encoding Categories in Ransomware Attacks",
    fill = "Encoding Category"
  ) +
  scale_fill_manual(
    values = c(
      "Windows" = "#F8766D", "UTF" = "brown3", "ISO" = "chocolate1", 
      "ASCII" = "coral3", "Big5" = "darkolivegreen3", "EUC" = "darkgoldenrod", 
      "GB" = "#00BA38", "KOI" = "aquamarine4", "TIS" = "cyan3", 
      "Shift-JIS" = "lightpink", "LiteSpeed" = "darkorchid2", 
      "Unknown" = "darkred", "Other" = "burlywood2"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Remove x-axis labels for pie chart


##### Analysis 3-4: Does ransom amount and encoding method collectively influence the prediction of financial loss? #####
# Group encoding methods into broader categories
df_filtered <- df_filtered %>%
  mutate(Encoding_Grouped = case_when(
    str_detect(Encoding, "windows") ~ "Windows",
    str_detect(Encoding, "utf") ~ "UTF",
    str_detect(Encoding, "iso|ISO") ~ "ISO",
    str_detect(Encoding, "ascii") ~ "ASCII",
    str_detect(Encoding, "big5|BIG5") ~ "Big5",
    str_detect(Encoding, "euc") ~ "EUC",
    str_detect(Encoding, "gb2312|GB2312") ~ "GB",
    str_detect(Encoding, "koi") ~ "KOI",
    str_detect(Encoding, "tis|TIS") ~ "TIS",
    str_detect(Encoding, "shift_jis") ~ "Shift-JIS",
    str_detect(Encoding, "litespeed") ~ "LiteSpeed",
    str_detect(Encoding, "Unknown") ~ "Unknown",
    TRUE ~ "Other"
  ))

# Heat Map 1: Financial Loss Across Ransom Amount & Encoding Type
ggplot(df_filtered, aes(x = Ransom, y = Encoding_Grouped)) +
  stat_summary_2d(aes(z = Loss), bins = 50) +  # Create 2D bins for heat map
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Use plasma color scale
  labs(
    title = "Financial Loss Across Ransom Amount & Encoding Type",
    x = "Ransom Amount ($)",
    y = "Character Encoding Type",
    fill = "Avg Loss ($)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),  # Bold y-axis labels
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(face = "bold", size = 18),  # Bold title
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10),  # Legend text size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_y_discrete(limits = rev(levels(df_filtered$Encoding_Grouped)), drop = TRUE) +  # Reverse y-axis order
  coord_cartesian(expand = FALSE)  # Remove padding around the plot

# Heat Map 2: Ransom vs Top 5 Encoding Groups (excluding Unknown/Other), colored by Loss
# Identify top 5 encoding groups by average loss (excluding Unknown/Other)
top_encodings <- df_filtered %>%
  filter(!(Encoding_Grouped %in% c("Unknown", "Other"))) %>%  # Exclude "Unknown" and "Other"
  group_by(Encoding_Grouped) %>%
  summarise(avg_loss = mean(Loss, na.rm = TRUE)) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 5) %>%
  pull(Encoding_Grouped)

# Filter dataset for only the top 5 encoding methods
df_filtered_clean <- df_filtered %>%
  filter(Encoding_Grouped %in% top_encodings)

# Heat Map 2: Ransom vs Top 5 Encoding Groups
ggplot(df_filtered_clean, aes(x = Ransom, y = Encoding_Grouped)) +
  stat_summary_2d(aes(z = Loss), bins = 50) +  # Create 2D bins for heat map
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Use plasma color scale
  labs(
    title = "Financial Loss Across Ransom Amount & Top 5 Encoding Types",
    x = "Ransom Amount ($)",
    y = "Top 5 Encoding Methods",
    fill = "Avg Loss ($)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),  # Bold y-axis labels
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(face = "bold", size = 18),  # Bold title
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10),  # Legend text size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_y_discrete(limits = rev(top_encodings), drop = TRUE) +  # Reverse y-axis order
  coord_cartesian(expand = FALSE)  # Remove padding around the plot


# Azwa Al Islam, TP078098
# Objective 4 :  To investigate if financial loss is influenced by the amount of ransom paid and downtime experienced.
# Analysis 4-1: Does an increased downtime cause a larger financial loss?

# Categorizing the Data
df_filtered <- df_filtered %>%
  mutate(downtime_category = cut(
    DownTime,
    breaks = c(-Inf, 10, 20, 30, 40, 50, Inf),
    labels = c("A", "B", "C", "D", "E", "F")
  )) %>%
  group_by(downtime_category) %>%
  mutate(total_loss = sum(Loss, na.rm = TRUE)) %>%
  ungroup()
# Visualizing the Data With a Bar Chart
downtime_labels <- c(
  "A" = "0–10 days",
  "B" = "11–20 days",
  "C" = "21–30 days",
  "D" = "31–40 days",
  "E" = "41–50 days",
  "F" = "51–60 days"
)

ggplot(df_filtered, aes(x = downtime_category, y = total_loss, fill = downtime_category)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = downtime_labels) +  # Update x-axis labels
  scale_fill_manual(values = c("#FFC0CB", "#FFB6C1", "#FF69B4", "#FF1493", "#C71585", "#8B008B"), 
                    labels = downtime_labels, name = "DownTime Category") +  # Update legend
  labs(
    title = "Total Loss by DownTime Category",
    x = "DownTime Category",
    y = "Total Loss"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#Analysis 4-2: How does the distribution of financial loss vary across different ransom amounts categorized as low, medium, and high? 

#Grouping into Low, Medium, and High
#Low = (-infinite - 1000), medium = (1000-2000), high = (2000 - infinite)
# Categorize Ransom Values Into Low, Medium, and High
df_filtered$ransom_category <- cut(df_filtered$Ransom, 
                                   breaks = c(-Inf, 1456, 2300, Inf),
                                   labels = c("Low", "Medium", "High"),
                                   right = FALSE)
#Visualizing With a Density Plot for Financial Loss Distribution By Ransom Category
ggplot(df_filtered, aes(x = Loss, fill = ransom_category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Financial Loss for Different Ransom Categories",
       x = "Financial Loss",
       y = "Density") +
  scale_fill_manual(values = c("Low" = "#FFB6C1", "Medium" = "#ADD8E6", "High" = "#FBEC94")) +  
  theme_minimal()

#Analysis 4-3 : Which factor—ransom paid or downtime—has a greater impact on financial loss?
# Calculating correlation between Loss and Ransom
cor_ransom_loss <- cor(df_filtered$Ransom, df_filtered$Loss, use = "complete.obs", method = "pearson")

# Calculating correlation between Loss and DownTime
cor_downtime_loss <- cor(df_filtered$DownTime, df_filtered$Loss, use = "complete.obs", method = "pearson")

# Printing the results
print(paste("Correlation between Ransom and Loss:", cor_ransom_loss))
print(paste("Correlation between DownTime and Loss:", cor_downtime_loss))

#Visualization With Scatter Plots
# Scatter plot for Ransom vs Loss
ggplot(df_filtered, aes(x = Ransom, y = Loss)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Relationship between Ransom and Loss",
       x = "Ransom Paid",
       y = "Financial Loss") +
  theme_minimal()
# Scatter plot for DownTime vs Loss
ggplot(df_filtered, aes(x = DownTime, y = Loss)) +
  geom_point(color = "lightpink") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Relationship between DownTime and Loss",
       x = "Downtime (Days)",
       y = "Financial Loss") +
  theme_minimal()
#Assessing variance in Loss is explained by Ransom and DownTime.
# Linear regression for Ransom effect on Loss
lm_ransom <- lm(Loss ~ Ransom, data = df_filtered)
summary(lm_ransom)

# Linear regression for DownTime effect on Loss
lm_downtime <- lm(Loss ~ DownTime, data = df_filtered)
summary(lm_downtime)

#Analysis 4-4: Can financial loss be accurately predicted by combining the ransom amount and downtime?
#Heatmap to comprehend the impact of the combination of ‘Ransom’ and ‘DownTime’ on ‘Loss’.
ggplot(df_filtered, aes(x = Ransom, y = DownTime)) +
  stat_summary_2d(aes(z = Loss), bins = 40) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  labs(title = "Heatmap of Ransom Amount, Downtime, and Financial Loss",
       x = "Ransom Amount ('000)",
       y = "Downtime (Days)",
       fill = "Avg Loss") +
  theme_minimal()

