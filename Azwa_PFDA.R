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

# Read the CSV file "4.hackingdata.csv" from the current working directory
df <- read.csv("4.hackingdata.csv")

# Function to check for both NA and empty string values
missing_values <- colSums(is.na(df) | df == "")

# Convert to data frame for better readability
missing_values_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)
#
#
#
#
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
#
#
#
#
#Cleaning Ransom column 
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
#
#
#
#
#Cleaning 'Downtime' column 

#Checking for missing values in downtime. 
missing_downtime <- sum(is.na(df_filtered$DownTime) | df_filtered$DownTime == "")
print(missing_downtime)
#Converting to numeric by removing commas.
df_filtered$DownTime <- as.numeric(gsub(",", "", df_filtered$DownTime))   
#
#
#
#
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
#
#
#
#
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
#
#
#
#
#DATA ANALYSIS
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
