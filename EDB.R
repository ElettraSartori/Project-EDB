#####################
# EDB DATA CLEANING #
#####################
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("stringr")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# Set the working directory
setwd("/Users/elettrasartori/Desktop/project_ETP/input")

# Load the data
# dowload the data from: https://fbellelli.com/EDB-data.html
edb_data <- read.csv("Extended EDB (by measure-country-HS chapter).csv", stringsAsFactors = FALSE)

#rename variables 
edb_data <- edb_data %>%
  rename(hs2 = Tentative.HS.chapters.match) %>%
  select(-Year) %>%
  rename(year = START, 
         sector = Harmonized.types.of.sectors.subject.to.the.measure)

#keep only the variables that for the subsequent analysis
edb_data <- edb_data %>%
  select(Nr, Agreement, COUNTRIES, `Region.group`, `Development.status`,
         `Harmonized.types.of.environment.related.objectives`,
         `Harmonized.types.of.measures`, year, END, ISIC,
         BREADTH, DEPTH, MEASURE_SCORE, hs2, sector, 
         `Absolute.link.strength..L_tilde.`, `Relative.link.strength..L_bar.`)

#drop if year hs2 or COUNTRIES is missing
#(because those are the key variables used for combine the two databases)
edb_data <- edb_data %>%
  drop_na(year, hs2, COUNTRIES)


# sector 
edb_data <- edb_data %>%
  separate_rows(sector, sep = ";") 

#as double check, compare it with Table 9 Bellelli et al (2022):
edb_data <- edb_data %>%
  mutate(sector_new = case_when(
    hs2 >= 6 & hs2 <= 14 ~ "Agriculture",
    hs2 >= 28 & hs2 <= 40 ~ "Chemicals",
    hs2 %in% c(84, 85) ~ "Energy",
    hs2 >= 44 & hs2 <= 48 ~ "Forestry",
    hs2 == 3 ~ "Fisheries",
    (hs2 >= 15 & hs2 <= 24) | (hs2 >= 50 & hs2 <= 70) | (hs2 >= 84 & hs2 <= 96) ~ "Manufacturing",
    (hs2 >= 25 & hs2 <= 27) | (hs2 >= 71 & hs2 <= 83) ~ "Mining",
    TRUE ~ "" 
  ))

#no correspondence, let's keep the original variable 
edb_data <- edb_data %>%
  select(-sector_new)

# environmental objective 
# Split 'Harmonized.types.of.environment.related.objectives' into multiple columns
edb_data <- edb_data %>%
  separate_rows(Harmonized.types.of.environment.related.objectives, sep = ";") %>%
  rename(env_objective = Harmonized.types.of.environment.related.objectives)

table(edb_data$env_objective)

# Type of measure 
edb_data <- edb_data %>%
  separate_rows(Harmonized.types.of.measures, sep = ";") %>%
  rename(measure_type = Harmonized.types.of.measures)

edb_data <- edb_data %>%
  mutate(REG = ifelse(measure_type %in% c("Grants and direct payments", "Income or price support", "Non-monetary support", "Tax concessions", "Loans and financing", "", "Other support measures", "Public procurement"), 0, 1),
         REG = factor(REG, levels = c(0, 1), labels = c("SUB", "REG")))

table(edb_data$REG)

# Save the cleaned data to file
write.csv(edb_data, "EDB.csv", row.names = FALSE)

##########################
# EDB DATA VISUALISATION #
##########################

################
# 1) LINE PLOT #
################

# Displaying frequency of each country
table(edb_data$COUNTRIES)

EU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
        "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
        "Slovenia", "Spain", "Sweden")

edb_data$COUNTRIES <- ifelse(edb_data$COUNTRIES %in% EU, "EU", edb_data$COUNTRIES)

# Filtering and organizing the data
df <- subset(edb_data, COUNTRIES %in% c("China", "India", "EU"))
df <- df[, c("COUNTRIES", "year", "Nr")]
df <- df[order(df$COUNTRIES, df$year, df$Nr), ]
df <- df[!duplicated(df), ]
df$raw_count <- ave(df$Nr, df$COUNTRIES, df$year, FUN = length)
df$nr <- NULL
df <- df[!duplicated(df), ]
df <- df[order(df$COUNTRIES, df$year, df$raw_count), ]

# Plotting the data
df <- subset(df, year <= 2019 & year >= 1990)
ggplot(df, aes(x = year, y = raw_count, color = COUNTRIES)) +
  geom_line() +
  labs(x = "Year", y = "Raw Count", color = "Countries") +
  scale_color_manual(values = c("China" = "blue", "India" = "red", "EU" = "green")) +
  theme_minimal()

#################
# 2) CUMULATIVE #
#################
# Calculate raw_count within each group of countries and year
df$raw_count <- ave(df$Nr, df$COUNTRIES, df$year, FUN = length)

# Calculate cumulative count within each country
df$cumulative_count <- ave(df$raw_count, df$COUNTRIES, FUN = cumsum)

# Exclude "EU" from the data
df <- df[df$COUNTRIES != "EU", ]

# Now plot with the updated data
ggplot(df, aes(x = year, y = cumulative_count, color = COUNTRIES)) +
  geom_line() +
  labs(x = "Year", y = "Cumulative Count", color = "Countries") +
  scale_color_manual(values = c("China" = "blue", "India" = "red")) +
  theme_minimal()


##########################
# 3) HORIZONTAL BAR PLOT #
##########################
# Specify columns of interest
columns <- c("measure_type", "REG", "Nr")

# Generate new dataframe without duplications and unnecessary columns
df <- edb_data %>%
  distinct(across(all_of(columns))) %>%
  select(-Nr)

# Group by measure_type and REG, then calculate counts
df <- df %>%
  group_by(measure_type, REG) %>%
  summarise(count = n())

# Create the bar plot
plot <- ggplot(df, aes(x = reorder(measure_type, count), y = count, fill = as.factor(REG))) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("gray", "black"), labels = c("SUB", "REG")) +
  labs(x = "Measure Type", y = "Count", fill = "REG", title = "Horizontal Bar Plot") +
  theme_minimal()

# Print the plot
print(plot)


################
# 4) PIE CHART #
################

# Specify columns of interest
columns <- c("Nr", "sector")

# Generate new dataframe without duplications and unnecessary columns
data <- distinct(edb_data, across(all_of(columns)))

# Calculate count by sector
data <- data %>%
  count(sector, name = "count")

# Create pie chart using ggplot2
ggplot(data, aes(x = "", y = count, fill = sector)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Sector Distribution") +
  guides(fill = guide_legend(title = "Sectors"))


##############
# 5) BARPLOT # (not in the presentation)
##############

# Specify columns of interest
columns <- c("Nr", "env_objective")

# Generate a new dataframe without duplications and unnecessary columns
data <- distinct(edb_data, across(all_of(columns)))

# Calculate the count for each environmental objective
data <- data %>%
  count(env_objective, name = "count") %>%
  arrange(count) %>%
  mutate(env_objective = factor(env_objective, levels = env_objective))

# Create a bar chart with points
ggplot(data, aes(x = env_objective, y = count)) +
  geom_segment(aes(xend = env_objective, yend = 0)) +
  geom_point(size = 1, color = "black") +
  coord_flip() +
  theme_bw() +
  xlab("")

################################################################################

################
# policy score # 
###############
rm(list = ls())
edb_data <- read.csv("EDB.csv", stringsAsFactors = FALSE)

# Filter the data based on Chinese data, which are from 2007 to 2011
edb_data <- subset(edb_data, year >= 2007 & year <= 2011)

# (1) raw count of active measures 
edb_data <- edb_data %>%
  group_by(COUNTRIES, year, hs2) %>%
  mutate(raw_count = n()) %>%
  ungroup()

count <- edb_data %>%
  select(COUNTRIES, year, hs2, raw_count, REG) %>%
  distinct()

# (2) Depth score (equation 5 page 18 Bellelli et al (2022))

depth_score <- edb_data %>%
  select(COUNTRIES, hs2, year, sector, Nr, DEPTH, Relative.link.strength..L_bar., MEASURE_SCORE, REG) %>%
  arrange(COUNTRIES, hs2, year, sector) %>%
  group_by(Nr, COUNTRIES, year) %>%
  mutate(active = 1) %>%
  ungroup() %>%
  group_by(Nr) %>%
  mutate(active_depth = active * DEPTH) %>%
  ungroup() %>%
  group_by(Nr, hs2) %>%
  mutate(active_depth_l = active_depth * Relative.link.strength..L_bar.) %>%
  ungroup() %>%
  group_by(COUNTRIES, hs2, year, REG) %>%
  summarise(depth_score = sum(active_depth_l)) %>%
  ungroup()


