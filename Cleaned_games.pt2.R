library(dplyr)
library(tidyr)
library(stringr)
library(caTools)
library(caret)
library(lightgbm)
library(ggplot2)
library(data.table)

new_games <- read.csv("Final Project Data/Cleaned_new_games.csv")  # Replace with your file path

colnames(new_games)


#missing values
missing_per_variable <- colSums(is.na(new_games))
missing_per_variable


# List of variables to drop
variables_to_drop <- c("Peak.CCU", "Drop", "DiscountDLC.count", "Full.audio.languages", 
                       "Reviews", "Header.image", "Website", "Support.url", 
                       "Support.email", "Metacritic.score", "Metacritic.url", 
                       "Score.rank", "Notes", "Average.playtime.forever", 
                       "Average.playtime.two.weeks", "Median.playtime.forever", 
                       "Median.playtime.two.weeks", "Screenshots", "Movies")

new_games <- new_games[, !colnames(new_games) %in% variables_to_drop]

head(new_games)
colnames(new_games)


# Create a new column for age-based ratings
new_games <- new_games %>%
  mutate(
    age_rating = case_when(
      Required.age >= 0 & Required.age <= 12 ~ "E for Everyone",
      Required.age >= 13 & Required.age <= 16 ~ "Teen",
      Required.age == 17 ~ "Mature",
      Required.age >= 18 ~ "Adult",
      TRUE ~ "Unknown"  # For missing or unexpected values
    )
  )

# Check the distribution of the new variable
table(new_games$age_rating)


# Drop the User.score column
new_games <- new_games %>% select(-User.score)
colnames(new_games)



# Consolidate Estimated.owners into broader categories
new_games <- new_games %>%
  mutate(
    owner_category = case_when(
      Estimated.owners %in% c("0 - 0", "0 - 20000", "20000 - 50000") ~ "Small Owners",
      Estimated.owners %in% c("50000 - 100000", "100000 - 200000", "200000 - 500000") ~ "Medium Owners",
      Estimated.owners %in% c("500000 - 1000000", "1000000 - 2000000", "2000000 - 5000000") ~ "Large Owners",
      Estimated.owners %in% c("5000000 - 10000000", "10000000 - 20000000", "20000000 - 50000000") ~ "Very Large Owners",
      Estimated.owners %in% c("50000000 - 100000000", "100000000 - 200000000") ~ "Massive Owners",
      TRUE ~ "Unknown"  # Handle any unexpected values
    )
  )

# View the distribution of the new consolidated variable
table(new_games$owner_category)



# Convert "True" to 1 and "False" to 0 in the Windows column
new_games <- new_games %>%
  mutate(Windows = ifelse(Windows == "True", 1, ifelse(Windows == "False", 0, NA)))

new_games <- new_games %>%
  mutate(
    Mac = ifelse(Mac == "True", 1, ifelse(Mac == "False", 0, NA)),
    Linux = ifelse(Linux == "True", 1, ifelse(Linux == "False", 0, NA))
  )


# Verify the conversion
head(new_games[, c("Windows", "Mac", "Linux")])

colnames(new_games)


# Drop the Tags and Publishers columns
new_games <- new_games %>% select(-Tags, -Publishers)
colnames(new_games)

#write.csv(new_games, "Cleaned_games_pt3.csv", row.names = FALSE)
new_games <- read.csv("/Users/matthewbecker/Desktop/MGSC 410/Final project/Cleaned_games_pt3.csv")  # Replace with your file path


# Create new binary variables for Single_player and Multi_Player
new_games <- new_games %>%
  mutate(
    Single_player = ifelse(grepl("Single-player", Categories, ignore.case = TRUE), 1, 0),
    Multi_Player = ifelse(grepl("Multi-player|Shared/Split Screen|Co-op", Categories, ignore.case = TRUE), 1, 0)
  )

# Verify the new variables
head(new_games[, c("Categories", "Single_player", "Multi_Player")])

colnames(new_games)


# Split and identify unique genres
unique_genres <- new_games %>%
  mutate(Genres = strsplit(as.character(Genres), ",")) %>%  # Split the genres by commas
  unnest(Genres) %>%                                        # Expand the list into individual rows
  distinct(Genres)                                          # Get unique genres

# Print the unique genres
print(unique_genres)

# Print all the unique genres
print(unique_genres$Genres)


print(unique_genres$Genres)

# Count the occurrences of each genre
genre_counts <- new_games %>%
  mutate(Genres = strsplit(as.character(Genres), ",")) %>%  # Split the genres by commas
  unnest(Genres) %>%                                        # Expand the list into individual rows
  count(Genres, sort = TRUE)                                # Count and sort by frequency

# Print the count of each unique genre
print(genre_counts)

#----------------------------------------------------------------------

# List of genres to extract
genres_to_extract <- c(
  "Indie", "Casual", "Action", "Adventure", "Simulation", "Strategy", 
  "RPG", "Sports", "Racing", "Massively Multiplayer", "Violent", "Gore", 
  "Free to Play"
)

# Update `new_games` with binary variables for specified genres
new_games <- new_games %>%
  mutate(Genres = strsplit(as.character(Genres), ",")) %>%  # Split genres into lists
  unnest(Genres) %>%                                        # Expand into separate rows
  mutate(
    Genres = ifelse(Genres %in% c("Free to Play", "Free To Play"), "Free to Play", Genres)  # Normalize Free to Play
  ) %>%
  filter(Genres %in% genres_to_extract) %>%                 # Keep only specified genres
  pivot_wider(
    names_from = Genres,
    values_from = Genres,
    values_fill = 0,
    values_fn = function(x) 1
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))     # Apply `replace_na` only to numeric columns

# Preview the updated `new_games` dataset
head(new_games)
colnames(new_games)

head(new_games[, c("Casual", "Free to Play")])
dim(new_games)

write.csv(new_games, "Cleaned_games_pt4.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------
#MODEL - KMeans Clustering - START HERE
#------------------------------------------------------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(cluster)
library(factoextra)
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(caTools)
library(caret)
library(lightgbm)
library(ggplot2)
library(data.table)
library(mclust)


colnames(new_games_model)
# Read data
new_games_model <- read.csv("Final Project Data/Cleaned_games_pt4.csv")



# 1. Initial Data Preparation
# Convert date to numeric (days since earliest release)
new_games_model$Release.date <- as.Date(new_games_model$Release.date, format="%b %d, %Y")
new_games_model$days_since_release <- as.numeric(new_games_model$Release.date - min(new_games_model$Release.date, na.rm = TRUE))

# Add derived variables to main dataset
new_games_model <- new_games_model %>%
  mutate(
    years_since_release = days_since_release / 365,
    Achievements = replace_na(Achievements, 0),
    owner_level = case_when(
      owner_category == "Small Owners" ~ 1,
      owner_category == "Medium Owners" ~ 2,
      owner_category == "Large Owners" ~ 3,
      owner_category == "Very Large Owners" ~ 4,
      owner_category == "Massive Owners" ~ 5,
      TRUE ~ NA_real_
    )
  )

# 2. Feature Selection and Preparation
focused_features_with_index <- new_games_model %>%
  mutate(
    original_index = row_number(),
    # Log transform skewed variables
    log_achievements = log1p(Achievements),
    log_years = log1p(years_since_release),
    # Price tiers
    price_free = as.numeric(Price == 0),
    price_budget = as.numeric(Price > 0 & Price < 10),
    price_midrange = as.numeric(Price >= 10 & Price < 30),
    price_premium = as.numeric(Price >= 30)
  ) %>%
  select(
    original_index,
    price_free, price_budget, price_midrange, price_premium,
    log_years,
    Required.age,
    owner_level,
    log_achievements,
    Windows, Linux,
    Single_player,
    Multi_Player,
    Action,
    Adventure,
    Strategy,
    RPG,
    Sports,
    Simulation,
    Indie,
    Massively.Multiplayer,
    Violent
  ) %>%
  na.omit()

# Modified feature weights to balance distribution and variance explained
feature_weights <- c(
  # Price features - slightly reduced but still important
  price_free = 1.4,      # Slightly reduced
  price_budget = 1.4,    
  price_midrange = 1.4,  
  price_premium = 1.4,   
  
  # Game characteristics - increased importance
  log_years = 1.4,           # Increased
  Required.age = 1.2,        # Increased
  owner_level = 1.4,         # Increased
  log_achievements = 1.3,    # Increased
  
  # Platform features - increased importance
  Windows = 1.0,            # Increased
  Linux = 1.0,             # Increased
  
  # Gameplay modes - increased
  Single_player = 1.4,     # Increased
  Multi_Player = 1.4,      # Increased
  
  # Genres - increased importance
  Action = 1.4,            # Increased
  Adventure = 1.4,         # Increased
  Strategy = 1.4,          # Increased
  RPG = 1.4,              # Increased
  Sports = 1.4,           # Increased
  Simulation = 1.4,        # Increased
  
  # Special categories - balanced with others
  Indie = 1.4,            # Increased
  Massively.Multiplayer = 1.4,  # Increased
  Violent = 1.0           # Increased
)

# Scale and weight features
focused_features <- scale(focused_features_with_index[,-1]) %*% diag(feature_weights)

# Perform k-means clustering with k=5
set.seed(123)
km_result <- kmeans(focused_features, centers = 5, nstart = 25)

# Create final_app_data using the same rows as focused_features
final_app_data <- new_games_model[focused_features_with_index$original_index,] %>%
  mutate(
    price_free = as.numeric(Price == 0),
    price_budget = as.numeric(Price > 0 & Price < 10),
    price_midrange = as.numeric(Price >= 10 & Price < 30),
    price_premium = as.numeric(Price >= 30),
    log_years = log1p(years_since_release),
    log_achievements = log1p(Achievements)
  ) %>%
  select(
    Name,
    price_free, price_budget, price_midrange, price_premium,
    log_years,
    Price,
    Release.date,
    Supported.languages,
    Positive,
    Negative,
    Developers,
    Required.age,
    owner_level,
    log_achievements,
    Windows, Linux,
    Single_player,
    Multi_Player,
    Action,
    Adventure,
    Strategy,
    RPG,
    Sports,
    Simulation,
    Indie,
    Massively.Multiplayer,
    Violent
  )

# Add the cluster assignments
final_app_data$Cluster <- km_result$cluster

# Save this for your app
saveRDS(final_app_data, "app_data.rds")

# Save model components
model_export <- list(
  km_centers = km_result$centers,
  feature_weights = feature_weights,
  column_means = attr(focused_features, "scaled:center"),
  column_sds = attr(focused_features, "scaled:scale")
)
saveRDS(model_export, "cluster_model.rds")




# Calculate clustering metrics
cluster_metrics <- list(
  total_ss = sum(scale(focused_features)^2),
  within_ss = km_result$tot.withinss,
  between_ss = km_result$betweenss
)
cluster_metrics$explained_var <- cluster_metrics$between_ss / cluster_metrics$total_ss

# Print metrics
print("K-means Clustering Metrics:")
print(paste("Within-cluster Sum of Squares:", round(cluster_metrics$within_ss, 2)))
print(paste("Between-cluster Sum of Squares:", round(cluster_metrics$between_ss, 2)))
print(paste("Explained Variance Ratio:", round(cluster_metrics$explained_var, 4)))

# Add cluster assignments directly to the original data
new_games_model$cluster <- NA
new_games_model$cluster[focused_features_with_index$original_index] <- km_result$cluster

# Create distribution plot
dist_plot <- ggplot(subset(new_games_model, !is.na(cluster)), 
                    aes(x = factor(cluster))) +
  geom_bar() +
  labs(title = "Distribution of Games Across Clusters",
       x = "Cluster",
       y = "Number of Games") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

# Create heatmap data
heatmap_data <- new_games_model %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    `Years Since Release` = mean(years_since_release, na.rm = TRUE),
    `Required Age` = mean(Required.age, na.rm = TRUE),
    `Owner Level` = mean(owner_level, na.rm = TRUE),
    `Achievements` = mean(Achievements, na.rm = TRUE),
    `% Free` = mean(Price == 0, na.rm = TRUE) * 100,
    `% Budget` = mean(Price > 0 & Price < 10, na.rm = TRUE) * 100,
    `% Mid-range` = mean(Price >= 10 & Price < 30, na.rm = TRUE) * 100,
    `% Premium` = mean(Price >= 30, na.rm = TRUE) * 100,
    `% Action` = mean(Action, na.rm = TRUE) * 100,
    `% Indie` = mean(Indie, na.rm = TRUE) * 100,
    `% MMO` = mean(Massively.Multiplayer, na.rm = TRUE) * 100,
    `% Multiplayer` = mean(Multi_Player, na.rm = TRUE) * 100,
    `% RPG` = mean(RPG, na.rm = TRUE) * 100,
    `% Strategy` = mean(Strategy, na.rm = TRUE) * 100,
    `% Sports` = mean(Sports, na.rm = TRUE) * 100,
    `% Simulation` = mean(Simulation, na.rm = TRUE) * 100
  ) %>%
  gather(key = "Features", value = "Value", -cluster)

# Create heatmap
heatmap_plot <- ggplot(heatmap_data, 
                       aes(x = Features, y = factor(cluster), fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "darkblue", 
                       mid = "lightblue",
                       midpoint = median(heatmap_data$Value, na.rm = TRUE)) +
  labs(title = "Cluster Characteristics Heatmap",
       x = "Features",
       y = "Cluster",
       fill = "Value (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display plots
print(dist_plot)
print(heatmap_plot)


#--------------------------------------------------------------------------------------------------------------------------------------
#EDA/VISUALS CODE HERE
#--------------------------------------------------------------------------------------------------------------------------------------

#GENRE DISTRIBUTION
# List of binary genre columns
binary_genres <- c(
  "Indie", "Casual", "Action", "Adventure", "Simulation", "Strategy", 
  "RPG", "Sports", "Racing", "Massively Multiplayer", "Violent", "Gore", 
  "Free to Play"
)

# Summarize the distribution of genres
genre_distribution <- new_games %>%
  select(all_of(binary_genres)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Genre", values_to = "Count")

# Create the bar chart
ggplot(genre_distribution, aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the axes for readability
  labs(
    title = "Distribution of Binary Genres",
    x = "Genre",
    y = "Count"
  ) +
  theme_minimal()


#MUTLI/SINLGE player distribution
# Summarize the counts of Multi_player and Single_player games
player_distribution <- new_games %>%
  select(Multi_Player, Single_player) %>%
  summarise(
    Multi_Player = sum(Multi_Player, na.rm = TRUE),
    Single_player = sum(Single_player, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Player_Type", values_to = "Count")

# Create a bar chart for the distribution
ggplot(player_distribution, aes(x = Player_Type, y = Count, fill = Player_Type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Distribution of Multi-player and Single-player Games",
    x = "Game Type",
    y = "Count"
  ) +
  theme_minimal()

#AGE GROUPS
# Summarize the distribution of age_rating
age_rating_distribution <- new_games %>%
  count(age_rating)

# Create the bar chart for age_rating
ggplot(age_rating_distribution, aes(x = reorder(age_rating, n), y = n, fill = age_rating)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Distribution of Age Ratings",
    x = "Age Rating",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#OWNERS
# Summarize the distribution of owner_category
owner_category_distribution <- new_games %>%
  count(owner_category)

# Create the bar chart for owner_category
ggplot(owner_category_distribution, aes(x = reorder(owner_category, n), y = n, fill = owner_category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Distribution of Owner Categories",
    x = "Owner Category",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Unique values in age_rating
cat("Unique values in age_rating:\n")
print(unique(new_games$age_rating))

# Unique values in owner_category
cat("\nUnique values in owner_category:\n")
print(unique(new_games$owner_category))



# Create dataframe for SS metrics
ss_metrics_df <- data.frame(
  Metric = c("Within-cluster SS", "Between-cluster SS"),
  Value = c(248739.65, 924992.62)
)

# Create the Sum of Squares plot with corrected text formatting
ss_plot <- ggplot(ss_metrics_df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "#4C51BF", alpha = 0.8) +
  geom_text(aes(label = format(Value, big.mark = ",", scientific = FALSE)), 
            vjust = -0.5, 
            size = 4) +
  theme_minimal() +
  labs(title = "Within and Between Cluster Sum of Squares",
       x = "",
       y = "Sum of Squares") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )


# Create separate visualization for explained variance
# Create improved visualization for explained variance
variance_plot <- ggplot(data.frame(x = 1, Explained = 0.4839, Unexplained = 1 - 0.4839), 
                        aes(x = "", y = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "white") +
  geom_rect(aes(ymin = 0, ymax = Explained, xmin = -0.5, xmax = 0.5), 
            fill = "#4C51BF", alpha = 0.8) +
  geom_rect(aes(ymin = Explained, ymax = 1, xmin = -0.5, xmax = 0.5), 
            fill = "#FF0000", alpha = 0.8) +
  coord_polar(theta = "y") +
  geom_text(aes(y = Explained/2, 
                label = sprintf("%.1f%% Explained", Explained*100)), 
            size = 5) +
  geom_text(aes(y = Explained + (1-Explained)/2, 
                label = sprintf("%.1f%% Unexplained", (1-Explained)*100)), 
            size = 5) +
  theme_void() +
  labs(title = "Explained Variance Ratio") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Display plots
print(ss_plot)
print(variance_plot)

# Optional: Arrange side by side
library(gridExtra)
grid.arrange(ss_plot, variance_plot, ncol = 2, widths = c(1.2, 1))










