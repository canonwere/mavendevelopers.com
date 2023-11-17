install.packages("readr")
install.packages("ggplot2")
library(readr)
library(plotly)
library(ggplot2)
data <- read.csv("MAVENDEVCMT429.csv")
library(readr)
MAVENDEVCMT429 <- read_csv("MAVENDEVCMT429.csv")
View(MAVENDEVCMT429)
#CODE 1
pie_chart_data <- table(MAVENDEVCMT429$`What are the primary causes of influenza?`)
pie_chart <- ggplot(data = as.data.frame(pie_chart_data), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal()
print(pie_chart)
#CODE 2
# Create a data frame from the table
pie_chart_data <- as.data.frame(table(MAVENDEVCMT429$`Which influenza virus types are most commonly responsible for seasonal outbreaks?`))

# Calculate percentages as whole numbers (no decimal points)
total <- sum(pie_chart_data$Freq)
pie_chart_data$Percent <- round((pie_chart_data$Freq / total) * 100)
label <- paste(pie_chart_data$Var1, pie_chart_data$Percent, "%")
# Create the pie chart
pie_chart <- ggplot(data = pie_chart_data, aes(x = "", y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Influenza Virus Types Responsible for Seasonal Outbreaks")

# Add percentage labels to the pie chart
pie_chart <- pie_chart +
  geom_text(position = position_stack(vjust = 0.5), size = 5)

# Show the pie chart
print(pie_chart)
# CODE 3
# Calculate the total number of people
total_people <- sum(MAVENDEVCMT429$TotalNumberOfPeople)
library(ggplot2)
library(dplyr)
# Calculate the total number of people by counting unique "Full names"
total_people <- MAVENDEVCMT429 %>%
  group_by(`Which region in Kenya is mostly affected?`) %>%
  summarize(TotalPeople = n_distinct(`Full names`))
# Create the bar graph
bar_graph <- ggplot(total_people, aes(x = `Which region in Kenya is mostly affected?`, y = TotalPeople)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of People Affected by Region in Kenya", x = "Region", y = "Total Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
# Show the bar graph
print(bar_graph)
# Load the required libraries
 #CODE 4
library(ggplot2)
library(dplyr)

# Calculate the percentages
season_data <- MAVENDEVCMT429 %>%
  group_by(`During what season is the rate of infection for influenza high?`) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


library(ggplot2)

# Create a data frame from the table
pie_chart_data <- as.data.frame(table(MAVENDEVCMT429$`During what season is the rate of infection for influenza high?`))

# Calculate percentages as whole numbers (no decimal points)
total <- sum(pie_chart_data$Freq)
pie_chart_data$Percent <- round((pie_chart_data$Freq / total) * 100)
label <- paste(pie_chart_data$Var1, pie_chart_data$Percent, "%")

# Create the pie chart
pie_chart <- ggplot(data = pie_chart_data, aes(x = "", y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Influenza Infection Rates by Season")

# Add percentage labels to the pie chart
pie_chart <- pie_chart +
  geom_text(position = position_stack(vjust = 0.5), size = 5)

# Show the pie chart
print(pie_chart)
#11/07/2023
#PHASE 3
#Couing number of people
library(dplyr)

# Calculate the total number of people by counting unique "Full names" within each region
total_people <- MAVENDEVCMT429 %>%
  group_by(`Which region in Kenya is mostly affected?`) %>%
  summarize(TotalPeople = n_distinct(`Full names`))

# Merge the total_people data with the MAVENDEVCMT429 dataset based on the region
MAVENDEVCMT429_with_count <- left_join(MAVENDEVCMT429, total_people, by = 'Which region in Kenya is mostly affected?')

# Check if 'TotalNumberOfPeople' column exists in the merged dataset
if (!("TotalNumberOfPeople" %in% names(MAVENDEVCMT429_with_count))) {
  # Calculate TotalNumberOfPeople based on the count of unique Full names within regions
  MAVENDEVCMT429_with_count <- MAVENDEVCMT429_with_count %>%
    mutate(TotalNumberOfPeople = n_distinct(`Full names`))
}
# Display the resulting dataset with the new 'TotalNumberOfPeople' column
print(MAVENDEVCMT429_with_count)
#Model 1
# Calculate the total number of people by counting unique "Full names" within each region
total_people <- MAVENDEVCMT429 %>%
  group_by(`Which region in Kenya is mostly affected?`) %>%
  summarize(TotalNumberOfPeople = n_distinct(`Full names`))

# Merge the total_people data with the MAVENDEVCMT429 dataset based on the region
MAVENDEVCMT429_with_count <- left_join(MAVENDEVCMT429, total_people, by = 'Which region in Kenya is mostly affected?')

# Check the levels of categorical variables
table(MAVENDEVCMT429_with_count$`Which influenza virus types are most commonly responsible for seasonal outbreaks?`)
table(MAVENDEVCMT429_with_count$`During what season is the rate of infection for influenza high?`)

# Train an ARIMA model to forecast influenza infection rates over time
library(forecast)
ts_data <- ts(MAVENDEVCMT429_with_count$TotalNumberOfPeople, frequency = 12)  # Assuming monthly data
model_arima <- auto.arima(ts_data)
print(model_arima)
#Model 2
# Fit a random forest classifier to predict the influenza virus types responsible for outbreaks
# Fit a linear regression model to predict the number of people affected by influenza
linear_model <- lm(TotalNumberOfPeople ~ `What are the common symptoms of influenza in adults?`, data = MAVENDEVCMT429_with_count)
summary(linear_model)
#Model 3
# Perform K-means clustering to identify clusters of regions based on the number of affected people
library(stats)
kmeans_clusters <- kmeans(scale(MAVENDEVCMT429_with_count$TotalNumberOfPeople), centers = 3)
print(kmeans_clusters)
#Model 4
# Train a naive Bayes classifier to predict the influenza virus types
library(e1071)
model_nb <- naiveBayes(`Which influenza virus types are most commonly responsible for seasonal outbreaks?` ~ ., data = MAVENDEVCMT429_with_count)
print(model_nb)
#Model 5
# Fit a Gaussian mixture model to identify clusters in the dataset
library(mclust)
gmm_model <- Mclust(MAVENDEVCMT429_with_count[, c('Score', 'TotalNumberOfPeople')])
print(gmm_model)
#example 6
# Apply text mining techniques to analyze free-text responses about influenza symptoms
library(tm)
library(SnowballC)
text_corpus <- Corpus(VectorSource(MAVENDEVCMT429_with_count$`What are the common symptoms of influenza in adults?`))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, stemDocument)
print(text_corpus)
