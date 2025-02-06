# Load necessary package
library(tidyverse)

# Read the CSV file
df <- read.csv("C:\\Users\\Narina Kaur\\Desktop\\FDA ASSIGNMENT\\4.hackingdata.csv", stringsAsFactors = FALSE)


# Replace "NULL" and empty values with NA
df$Encoding <- ifelse(df$Encoding %in% c("NULL", "", NA), NA, df$Encoding)

# Standardize encoding names
df$Encoding <- tolower(df$Encoding)  # Convert to lowercase
df$Encoding <- gsub("utf-8", "UTF-8", df$Encoding)
df$Encoding <- gsub("iso-8859-1", "ISO-8859-1", df$Encoding)
df$Encoding <- gsub("windows-1252", "Windows-1252", df$Encoding)
df$Encoding <- gsub("gb2312", "GB2312", df$Encoding)
df$Encoding <- gsub("big5", "BIG5", df$Encoding)

# Fill missing values with "Unknown"
df$Encoding[is.na(df$Encoding)] <- "Unknown"

# Save the cleaned data
write.csv(df, "cleaned_data.csv", row.names = FALSE)

# Check results
table(df$Encoding)
