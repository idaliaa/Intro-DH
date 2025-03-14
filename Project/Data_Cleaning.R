library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2) 
library(gt)



# Load the scraped data
df <- read_csv("/Users/psylviana/Downloads/Project/bundestag_speeches.csv")


# Create a 'Faction' column for political faction classification
df <- df %>%
  mutate(
    Faction = case_when(
      str_detect(Politician_ID, "CDU/CSU") ~ "CDU/CSU",
      str_detect(Politician_ID, "SPD") ~ "SPD",
      str_detect(Politician_ID, "Grüne") ~ "Grüne",
      str_detect(Politician_ID, "AfD") ~ "AfD",
      str_detect(Politician_ID, "FDP") ~ "FDP",
      str_detect(Politician_ID, "DIE LINKE") ~ "Die Linke",  
      str_detect(Politician_ID, "Fraktionslos") ~ "Fraktionslos",
      TRUE ~ "Unbekannt"  # Fallback for unrecognized factions
    )
  )

# Create a 'Fraktion' column based on the last part of the 'Politician_ID'
df <- df %>%
  mutate(
    Fraktion = str_trim(word(Politician_ID, -1, sep = ","))  # Extract last part after last comma
  )

# Check unique values in 'Fraktion' column
unique(df$Fraktion)


### DATA CLEANING ###

# Check for any missing (NA) values in the dataset
colSums(is.na(df))

df$Politician_ID[is.na(df$Politician_ID)] <- "Unbekannt"
df$Fraktion[is.na(df$Fraktion)] <- "Unbekannt"
df$Speech[is.na(df$Speech)] <- "Keine Rede verfügbar"



# Remove duplicate rows and empty speeches
df_clean <- df %>%
  distinct() %>%  
  filter(!is.na(Speech), Speech != "")

# Remove speeches from "Mitglied des Präsidiums" and "Minister:in"
df_clean <- df_clean %>%
  filter(!str_detect(Name, "Mitglied des Präsidiums|Minister:in|Staatssekretär:in|Gast|Unbekannt|Kanzler:in"))

# Normalize text: convert to lowercase, remove punctuation, and trim extra spaces
df_clean <- df_clean %>%
  mutate(Speech_clean = Speech %>%
           tolower() %>%
           str_replace_all("[[:punct:]]", "") %>%  
           str_squish())  # Extra Leerzeichen entfernen



# Define keywords for filtering COVID and Climate-related speeches
keywords <- c(
  "covid", "corona", "klimawandel", "klima", "pandemie", "coronavirus", "antigenschnelltest",
  "covid-19", "sars-cov-2", "lockdown", "quarantäne", "infektion", "virus", 
  "impfung", "impfstoff", "corona-maßnahmen", "social distancing", "grüne energie",
  "maskenpflicht", "covid-19-tests", "gesundheitskrise", "ökologische landwirtschaft",
  "pandemiebekämpfung", "wirtschaftskrise", "co2-emissionen", "treibhausgase", 
  "nachhaltigkeit", "klimaschutz", "klimaneutralität", "kohlenstoffdioxid", "abfallvermeidung",
  "erneuerbare energien", "grüne energie", "klimapolitik", "umweltzerstörung", 
  "klimapolitik", "klimaschutz", "klimakrise", "klimaflucht", "globale erwärmung", 
  "klimawandel bekämpfen", "emissionen", "globaler klimawandel", "umweltkrise", 
  "epidemie", "mutation", "variante", "herdenimmunität", "kontaktverfolgung", "inzidenz",
  "r-wert", "intensivstation", "beatmung", "pcr-test", "schnelltest", "umweltschutz",
  "nachhaltige entwicklung", "grüne wirtschaft", "umweltkrise", "ökologie", "naturschutz",
  "biodiversität", "kreislaufwirtschaft", "energiewende", "erneuerbare energien",
  "klimagerechtigkeit", "klimaresilienz", "klimaanpassung", "dekarbonisierung", 
  "recycling", "abfallmanagement", "umweltbewusstsein", "nachhaltige landwirtschaft",
  "biologischer anbau", "klimafolgen", "umweltzerstörung", "ökologische krise", 
  "selbsttest", "ct-wert", "kontaktperson", "infektionskette", "asymptomatisch", "post-covid-syndrom",
  "mutante", "antikörpertest", "booster-impfung", "hybridimmunität", "t-zell-immunität"
  
)

# Filter speeches that contain any of the keywords related to COVID and Climate
df_covid_climate <- df_clean %>%
  filter(str_detect(Speech_clean, paste(keywords, collapse = "|")))  # Look for any of the keywords


###### PARTY MENTIONED vs. NOT MENTIONED COVID & CLIMATE ######

# Create a column to indicate whether the speech mentions COVID or Climate Change
df_clean <- df_clean %>%
  mutate(mentions_covid_climate = ifelse(str_detect(Speech_clean, paste(keywords, collapse = "|")), 1, 0))

# Count the number of speeches per party, with and without COVID/Climate mentions
party_mentions <- df_clean %>%
  group_by(Faction) %>%
  summarise(
    total_speeches = n(),
    mentioned_covid_climate = sum(mentions_covid_climate),
    not_mentioned_covid_climate = total_speeches - mentioned_covid_climate
  )

# Summary of how many parties mentioned or did not mention COVID/Climate -> all parties mentioned
party_mentions_summary <- party_mentions %>%
  summarise(
    total_parties = n(),
    parties_mentioned = sum(mentioned_covid_climate > 0),
    parties_not_mentioned = sum(mentioned_covid_climate == 0)
  )

# Reshape the data for better visualization
party_mention_table <- party_mentions %>%
  pivot_longer(cols = c("mentioned_covid_climate", "not_mentioned_covid_climate"),
               names_to = "Mention_Type", values_to = "Count")

# Create the table plot
ggplot(party_mention_table, aes(x = Faction, y = Mention_Type, fill = Count)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = Count), size = 5, color = "black") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Speech Mentions per Party", x = "Party", y = "Mention Type")



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
df_covid_climate <- df_covid_climate %>%
  mutate(year = sapply(URL, extract_year))





