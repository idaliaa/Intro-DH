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
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "firebrick")) +
  labs(title = "Sentiment-Analyse nach Fraktion",
       x = "Fraktion",
       y = "Anzahl der Wörter",
       fill = "Sentiment") +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(panel.grid = element_blank())



#### Extract Year from URL ####

# Function to extract year based on the session number in the URL
extract_year <- function(url) {
  if (is.na(url) || !grepl("https://opendiscourse.de/plenarsitzungen/\\d{2}-\\d+", url)) {
    return(NA)  # Return NA if the URL is malformed
  }
  
  # Extract session number from URL (e.g., "19-73")
  session_number <- sub("https://opendiscourse.de/plenarsitzungen/(\\d{2}-\\d{2,3}).*", "\\1", url)
  
  # Split the session number into parts (e.g., "19" and "73")
  parts <- strsplit(session_number, "-")[[1]]
  
  if (length(parts) != 2) {
    return(NA)  # Return NA if the session number is malformed
  }
  
  # Determine year based on the session number
  session_num <- as.numeric(parts[2])  # Convert session part to numeric
  
  if (parts[1] == "20") {
    if (session_num >= 10) {
      return(2022)  # For URLs 20-10 to 20-77
    } else {
      return(2021)  # For URLs 20-01 to 20-09
    }
  } else if (parts[1] == "19") {
    if (session_num >= 139) {
      return(2020)  # For URLs 19-139 to 19-202
    } else if (session_num >= 73) {
      return(2019)  # For URLs 19-73 to 19-138
    }
  }
  
  return(NA)  # Return NA if no match is found
}

# Apply this function to the 'url' column to extract the year
df_covid_climates <- df_covid_climate %>%
  mutate(year = sapply(URL, extract_year))



# Tokenize the speeches, join with Bing sentiment lexicon, and summarize by year
df_sentiment_yearly <- df_covid_climates %>%
  unnest_tokens(word, Speech_clean) %>%
  inner_join(bing_sentiments, by = "word") %>%
  group_by(year, sentiment) %>%
  summarise(count = n(), .groups = "drop")




# Plotting sentiment analysis over time using geom_line
ggplot(df_sentiment_yearly, aes(x = year, y = count, color = sentiment, group = sentiment)) +
  geom_line() +  # Create lines for each sentiment
  labs(title = "Sentiment Analysis by Year",
       x = "Year",
       y = "Word Count",
       color = "Sentiment") +
  theme_minimal()


extract_year <- function(url) {
  # Validate URL structure
  if (!grepl("^https://opendiscourse.de/plenarsitzungen/\\d{2}-\\d{3}$", url)) {
    return(NA_integer_)  # Return NA if URL format is incorrect
  }
  
  # Extract session number (e.g., "19-073")
  session_number <- sub(".*plenarsitzungen/(\\d{2})-(\\d{3}).*", "\\1-\\2", url)
  
  # Split into parts
  parts <- strsplit(session_number, "-")[[1]]
  session_group <- as.numeric(parts[1])
  session_num <- as.numeric(parts[2])  # Convert padded "073" → 73
  
  # Assign year based on session number
  if (session_group == 20) {
    return(ifelse(session_num >= 10, 2022, 2021))
  } else if (session_group == 19) {
    if (session_num >= 139) {
      return(2020)
    } else if (session_num >= 73) {
      return(2019)
    }
  }
  
  return(NA_integer_)  # Default case
}

df_covid_climates <- df_covid_climate %>%
  mutate(year = map_int(URL, extract_year))  # Use "URL" instead of "url"






