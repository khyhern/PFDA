# Lara Dominique Isidro Castro, TP072345
# Objective 2: To investigate the influence of server operating systems and web server types on hacking vulnerability and financial loss.


# Loads necessary libraries
library(forcats)              # For handling factors in a cleaner way
library(corrplot)             # For creating correlation points
library(RColorBrewer)         # For color palettes


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
