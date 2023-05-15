# Set Current Working Directory
setwd("C:/Users/Charl/OneDrive/Documents/Education/Drexel/du_term_spring_23/INFO-332/Assignments/A2")

######################## Intro: Loading Data & Packages ########################
library(dplyr)
library(ggplot2)
library(ggrepel)

spending <- read.csv("healthcare_spending.csv")
expectancy <- read.csv("life_expectancy.csv")
country_codes <- read.csv("country_codes.csv")

######################## Task 1 ################################################
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



######################## Task 2 ################################################
library(ggplot2)
plt <- ggplot(data=df_final, aes(x= spending_pct_gdp, y=life_expectancy)) + geom_point() 
plt <- plt + xlab("Healthcare Spending (% GDP)") + ylab("Life Expectancy(years)")
plt <- plt
plt <- plt + geom_text_repel(aes(label = country), size=1, max.overlaps=20)
plt

