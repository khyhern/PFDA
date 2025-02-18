# PFDA
# Narina Kaur Sidhu, TP079398 
# Objective 3: To investigate the relationship between ransom demands and encoding methods on financial loss. 

library(ggplot2)
library(dplyr)
library(viridis)  # For color scales
library(stringr) 

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
# Ensure Loss and Encoding columns are clean and numeric
df_filtered <- df_filtered %>%
  mutate(
    Loss = as.numeric(Loss)  # Ensure Loss is numeric
  )

# Check for missing values in Encoding and Loss columns
missing_encoding <- sum(is.na(df_filtered$Encoding) | df_filtered$Encoding == "")
missing_loss <- sum(is.na(df_filtered$Loss))

# Print the results
print(paste("Missing Encoding values:", missing_encoding))
print(paste("Missing Loss values:", missing_loss))

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