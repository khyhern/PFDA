# Load necessary libraries
library(dplyr)    # For data manipulation
library(stringr)  # For string operations

# Step 1: Read the dataset
df <- read.csv("4.hackingdata.csv")

# Step 2: Inspect the OS column to identify unique values and check for inconsistencies
# View unique OS entries
unique(df$OS)

# Check for missing values or empty entries in the OS column
sum(is.na(df$OS) | df$OS == "")

# Step 3: Clean the OS column
# Convert the OS column to lowercase and trim any extra spaces
df$OS <- tolower(str_trim(df$OS))

# Replace missing or empty values with "unknown"
df <- df %>% 
  mutate(OS = ifelse(is.na(OS) | OS == "", "unknown", OS))

# Step 4: Standardize OS Names
# Create a new column 'OS_Base' to classify the OS into categories
df <- df %>% 
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

# Step 5: Extract OS version numbers (if available)
# Use regular expressions to extract version numbers
df$OS_Version <- str_extract(df$OS, "[0-9]+(\\.[0-9]+)*")

# Step 6: Verify the cleaned data
# Check unique standardized OS categories
unique(df$OS_Base)

# Check unique extracted OS versions
unique(df$OS_Version)

# Summary of the OS_Base column
summary(df$OS_Base)

# Step 7: Final Check for missing or incorrect data
# Verify there are no unexpected missing values in OS or OS_Version
sum(is.na(df$OS) | df$OS == "")
sum(is.na(df$OS_Version))
