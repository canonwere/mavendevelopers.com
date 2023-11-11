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