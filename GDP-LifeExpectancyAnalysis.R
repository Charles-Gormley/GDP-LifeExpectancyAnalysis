# Set Current Working Directory
setwd("C:/Users/Charl/OneDrive/Documents/Education/Drexel/du_term_spring_23/INFO-332/Assignments/A2")

######################## Intro: Loading Data & Packages ########################
library(dplyr)
library(ggplot2)
library(ggrepel)
library(purrr)
library(ggplot2)

spending <- read.csv("healthcare_spending.csv")
expectancy <- read.csv("life_expectancy.csv")
country_codes <- read.csv("country_codes.csv")

######################## Creation Of Dataset ################################################
# Select only year 2018 in all databases
spending <- filter(spending, TIME == 2018)
expectancy <- filter(expectancy, TIME == 2018)

# Further Filtering of Databases for totals
spending <- filter(spending, MEASURE == "PC_GDP")
spending <- filter(spending, SUBJECT == "TOT")
expectancy <- filter(expectancy, SUBJECT == "TOT")

# Renaming country codes to location for easier merging
country_codes <- rename(country_codes, "LOCATION" = "CODE")

# Merging Columns 1:1 based on country code, LOCATION, and LOCATION.
df_final <- inner_join(spending, expectancy, by = "LOCATION")
df_final <- inner_join(df_final, country_codes, by = "LOCATION")

# Selecting only relevant columns
df_final <- select(df_final, "Value.x", "Value.y", "Country")

# Renaming columns
df_final <- rename(df_final, country = "Country", 
                   spending_pct_gdp = "Value.x",
                   life_expectancy = "Value.y")


# Dropping observations that have null values
df_final <- na.omit(df_final)

# Sorting Values
df_final <- df_final %>% arrange(desc(life_expectancy))
df_final

######################## Preparation of Data ################################################
# Selecting Columns to be clustered
df_cluster <- df_final %>% select(spending_pct_gdp, life_expectancy)

# Normalizing Data for clusterization
df_cluster <- scale(df_cluster[, c("spending_pct_gdp", "life_expectancy")])

######################## Cluster Analysis: Elbow Method for Optimal K. ################################################
# K-Means Elbow Analysis
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = df_cluster, centers = k)
  model$tot.withinss})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
# Analysis: Seems as if the elbow point is 5. 5 will be our optimal k.

######################## Creating Cluster Data Frame with optimal k=5 ################################################
cluster_result <- kmeans(df_cluster, centers=5)
cluster_labels <- cluster_result$cluster
df_final$cluster_labels <- cluster_labels

######################## New Plot with Cluster Colors ################################################
plt <- ggplot(data=df_final, aes(x= spending_pct_gdp, y=life_expectancy, color = factor(cluster_labels))) + geom_point() 
plt <- plt + xlab("Healthcare Spending (% GDP)") + ylab("Life Expectancy(years)")
plt <- plt + geom_text_repel(aes(label = country), size=3, max.overlaps=20)
plt <- plt + scale_color_discrete()
plt

