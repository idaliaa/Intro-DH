library(magrittr)
library(ggplot2) 
library(NLP)
library(tidytext) # Sentiment-Analyse
library(topicmodels) # Topic Modelling
library(tm) # Topic Modelling 
library(reshape2)
library(tidytext)
library(stopwords)
library(dplyr)
library(tidyverse)
library(textmineR)

# Load filtered data (COVID and climate-related speeches)
df_covid_climate <- readRDS("df_covid_climate.rds")

# Create custom germans stopwordss
german_stopwords <- stopwords::stopwords("de")  
custom_stopwords <- c(german_stopwords, 
                      "damen", "herren", "schon", "liebe", "dass", "menschen", "müssen", "mehr", "kollegen",
                      "meine", "sehr", "vielen", "werte", "genau", "ja", "kolleginen", "weiter", "kolleginnen",
                      "überhaupt", "eigentlich", "natürlich", "wirklich", "euro", "1", "2", "deutschland",
                      "einfach", "wissen", "lassen", "ganz", "bisschen", "unsere", "gerade", "bereich",
                      "deshalb", "letzten", "jahren", "präsident", "präsidentin", "unserer", "deswegen",
                      "prozent", "beispiel", "richtig", "geehrte", "geehrter", "darüber", "möchte", "besser",
                      "gesagt", "endlich", "brauchen", "antrag", "bundesregierung", "worden", "möglich",
                      "stellen", "wichtig", "kommen", "deutlich", "darauf", "nämlich", "europa", "glaube", 
                      "millionen", "unternehmen", "unserem", "milliarden", "gemacht", "haushalt", "stelle",
                      "sollten", "gehört", "stehen", "deutschen", "vielleicht", "deutsche", "nächsten", 
                      "besonders", "schaffen", "zukunft", "maßnahmen", "politik", "gesetz", "leider", 
                      "bereits", "mittel", "unseren", "minister", "ministerin", "gemeinsam", "nehmen", 
                      "sollen", "wurden", "großen", "frauen", "debatte", "europäischen", "europäische", 
                      "tatsächlich"
                      
                      )


# Tokenization and stopword removal
speech_tokens_clean <- df_covid_climate %>%
  unnest_tokens(word, Speech_clean) %>%  # Tokenize text
  filter(!word %in% custom_stopwords) %>%  # Remove custom stopwords
  filter(nchar(word) > 5) # Remove short words (less than 6 characters)


# Create Document-Term Matrix (DTM)
dtm <- speech_tokens_clean %>%
  count(URL, word) %>%
  cast_dtm(document = URL, term = word, value = n)


# Train LDA model with 16 topics
lda_model <- LDA(dtm, k = 20, control = list(seed = 1234))


# Extract the most important words for each topic
topics <- tidy(lda_model, matrix = "beta")

# Display the top 10 terms for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()



# Visualization: Top 10 words per topic
ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free_y") +  # Create separate plots for each topic
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +
  labs(title = "Top 10 Wörter pro Thema", x = "Wörter", y = "Wichtigkeit (Beta)") +
  theme(axis.text.y = element_text(size = 8)) +
  theme(panel.grid = element_blank())



# Top 10 Wörter / Thema
  ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 Wörter pro Thema", x = "Wörter", y = "Wahrscheinlichkeit") +
  theme_minimal()


  ggplot(top_terms, aes(x = factor(topic), y = term, fill = beta)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Themen-Wörter-Heatmap", x = "Thema", y = "Wörter") +
  theme_minimal()





# Save cleaned tokenized data, DTM, and topics model
saveRDS(speech_tokens_clean, "speech_tokens_clean.rds")
saveRDS(dtm, "dtm.rds")
saveRDS(topics, "topics.rds")

