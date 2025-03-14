library(rvest)
library(dplyr)

# Generate URLs for the 19th legislative period (19-1 to 19-239)
urls_19 <- paste0("https://opendiscourse.de/plenarsitzungen/19-", sprintf("%03d", 73:239))

# Generate URLs for the 20th legislative period (20-1 to 20-77)
urls_20 <- paste0("https://opendiscourse.de/plenarsitzungen/20-", sprintf("%03d", 1:77))



# Combine both urls 
urls <- c(urls_19, urls_20)

# Initialize an empty list to store the speech
all_speeches <- list()

# Loop through each URL to scrape data
for (url in urls) {
  webpage <- read_html(url)
  
  # Extract politician names
  names <- webpage %>%
    html_elements(".chakra-heading.css-1codx82") %>%   
    html_text2()
  
  # Extract politician IDs
  politician_ids <- webpage %>%
    html_elements(".text-gray-600") %>%  
    html_text2()
  
  # Extract the speech content
  speeches <- webpage %>%
    html_elements(".chakra-text.css-d5luuw") %>%  
    html_text2()
  
  # Create a temporary dataframe
  temp_df <- data.frame(
    Name = names,
    Politician_ID = politician_ids,
    Speech = speeches,
    URL = url  # Record the URL to track the source of each speech
  )
  
  # Append to list
  all_speeches[[url]] <- temp_df
}

# Combine all data frames into one
final_df <- bind_rows(all_speeches)

# Save to CSV
# write.csv(final_df, "bundestag_speeches.csv", row.names = FALSE)
write.csv(final_df, "/Users/psylviana/Downloads/Project/bundestag_speeches.csv", row.names = FALSE, quote = TRUE)

print("Scraping complete! Data saved to 'bundestag_speeches.csv'")



 






