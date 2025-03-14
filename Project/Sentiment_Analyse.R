library(magrittr)
library(ggplot2) 
library(NLP)
library(tidytext) # Sentiment analysis and tokenization
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)  # For map function

#### SENTIMEN ANALYSE #####
# Sentiment-Analyse: Sind Reden eher positiv, neutral oder negativ? 
# Vergleich der Rhetorik: Welche Begriffe nutzen die Parteien am häufigsten?
# Themen-Häufigkeit pro Partei: Welche Partei sprach am meisten über COVID-19?


# Load the scraped data
df <- read_csv("/Users/psylviana/Downloads/Project/bundestag_speeches.csv")
# Load filtered files
df_covid_climate <- readRDS("df_covid_climate.rds")

# Load Bing sentiment dictionary
bing_sentiments <- get_sentiments("bing")


# Tokenize speeches and join with Bing sentiment lexicon
df_sentiment <- df_covid_climate %>%
  unnest_tokens(word, Speech_clean) %>%  # Tokenize speeches into words
  inner_join(bing_sentiments, by = "word") %>%  # Tokenize speeches into words
  group_by(Faction, sentiment) %>%  # Group by faction and sentiment
  summarise(count = n(), .groups = "drop") %>%  # Count the words
  filter(Faction != "Unbekannt") # Remove "Unbekannt"


# Create a bar plot of sentiment counts per faction with labels
ggplot(df_sentiment, aes(x = Faction, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add labels
  theme_minimal() +
  scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "firebrick")) +
  labs(title = "Sentiment-Analyse nach Fraktion",
       x = "Fraktion",
       y = "Anzahl der Wörter",
       fill = "Sentiment") +
  theme(axis.text.x = element_text()) +
  theme(panel.grid = element_blank())





# Tokenize the speeches, join with Bing sentiment lexicon, and summarize by year
df_sentiment_yearly <- df_covid_climate %>%
  unnest_tokens(word, Speech_clean) %>%
  inner_join(bing_sentiments, by = "word") %>%
  group_by(year, sentiment) %>%
  summarise(count = n(), .groups = "drop")



# Plotting sentiment analysis over time using geom_line
ggplot(df_sentiment_yearly, aes(x = year, y = count, color = sentiment, group = sentiment)) +
  geom_line() +  # Create lines for each sentiment
  geom_text(aes(label = count), 
            position = position_nudge(y = 10),  # Nudge the labels above the points
            size = 3, color = "black", vjust = -0.5) +  # Add the count labels
  labs(title = "Sentiment Analysis by Year",
       x = "Year",
       y = "Word Count",
       color = "Sentiment") +
  theme_minimal() +
  theme(panel.grid = element_blank())




