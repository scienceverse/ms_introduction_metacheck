RGX_ASPREDICTED <- "aspredicted"
text_found <- search_text(xml, RGX_ASPREDICTED, return = "paragraph")
# blind links often end up having a space in the extracted hyperlink. 
# e.g., https://aspredicted.org/blind.php? x=6uq6sx which should have been extracted as https://aspredicted.org/blind.php?x=6uq6sx
# So we fix it by removing the space. 
text_found$text <- gsub("blind\\.php\\?\\s*", "blind.php?", text_found$text)
RGX_ASPREDICTED_LINK <- "aspredicted\\.org/\\s*[a-zA-Z0-9]{5}"
text_found_as_predicted <- search_text(text_found, RGX_ASPREDICTED_LINK, return = "match")
RGX_ASPREDICTED_BLIND <- "http://aspredicted\\.org/blind\\.php\\?x=s*[a-zA-Z0-9]{6}"
text_found_as_predicted_blind <- search_text(text_found, RGX_ASPREDICTED_BLIND, return = "match")

# Clean up
# Remove any spaces between `.io/` and the 5 characters
text_found_as_predicted$text <- gsub("aspredicted.org/\\s*", "aspredicted.org/", text_found_as_predicted$text)

text_found_as_predicted$http_aspredicted <- ifelse(grepl("^https?://", text_found_as_predicted$text), text_found$text, paste0("https://", text_found$text))
text_found_as_predicted$http_aspredicted <- paste0(text_found_as_predicted$text,".pdf")
text_found_as_predicted$aspredicted_id <- substr(text_found_as_predicted$text, nchar(text_found_as_predicted$text) - 4, nchar(text_found$text))

# Remove blind and old public links
text_found_as_predicted <- text_found_as_predicted %>%
  filter(!(aspredicted_id %in% c("publi", "blind")))

# Then remove duplicates
text_found_as_predicted <- text_found_as_predicted %>% distinct(text, .keep_all = TRUE)

# Then we search through all projects:

# Function to process a single URL and extract data
process_url <- function(url) {
  # Insert your cookie value here
  cookie_value <- "mypAijHvvVMGAcTbsjmaMTGecMTbn3iDzWyvmiFe1neol97yH3"
  
  # Make the GET request with the cookie
  page <- GET(
    url,
    add_headers(Cookie = paste0("PHPSESSID=", cookie_value))
  ) %>% read_html()
  
  # Check the page content
  page  
  # Extract the text content
  text <- page %>% html_text2()
  
  # Regular expression pattern to split by one or more newlines or multiple spaces
  split_text <- unlist(strsplit(text, "\n"))
  
  # Remove empty lines and lines with only whitespace (including \r, \n, spaces)
  split_text <- split_text[nchar(gsub("\\s", "", split_text)) > 0]
  
  # Initialize a list to hold the answers
  answers <- list(
    title = split_text[2]
  )
  
  # Function to extract text for a question
  extract_question <- function(start_text, end_text, split_text) {
    # Wrap the function in tryCatch to handle errors
    result <- tryCatch({
      start_index <- grep(start_text, split_text)
      end_index <- grep(end_text, split_text)
      
      # Ensure both start and end indices are found
      if(length(start_index) == 0 || length(end_index) == 0) {
        stop("Start or end text not found")
      }
      
      # Extract the text between the start and end indices
      found_text <- split_text[(start_index + 1):(end_index - 1)]
      
      # Combine the text into a single string and return
      paste(found_text, collapse = " ")
      
    }, error = function(e) {
      # Return "ERROR" if any error occurs
      return("ERROR")
    })
    
    return(result)
  }
  
  
  answers$created <- extract_question("Pre-registered on", "Have any data been collected", split_text)
  answers$authors <- extract_question("Author", "Pre-registered on", split_text)
  
  # Extract answers for Q1-Q8
  answers$Q1 <- extract_question("Have any data been collected", "What's the main question being asked or hypothesis being tested", split_text)
  answers$Q2 <- extract_question("What's the main question being asked or hypothesis being tested in this study?", "Describe the key dependent variable", split_text)
  answers$Q3 <- extract_question("Describe the key dependent variable", "How many and which conditions will", split_text)
  answers$Q4 <- extract_question("How many and which conditions will", "Specify exactly which analyses you will conduct", split_text)
  answers$Q5 <- extract_question("Specify exactly which analyses you will conduct", "Describe exactly how outliers will be defined|Any secondary analyses", split_text)
  answers$Q6 <- extract_question("Describe exactly how outliers will be defined|Any secondary analyses", "How many observations will be collected", split_text)
  answers$Q7 <- extract_question("number will be determined", "Anything else you would like to pre-register", split_text)
  answers$Q8 <- extract_question("secondary analyses, variables collected", "Version of AsPredicted Questions", split_text)
  
  # Extract version
  version_index <- grep("Version of AsPredicted Questions", split_text)
  answers$version <- paste(split_text[version_index], collapse = " ")
  
  return(answers)
}

# Initialize the dataframe for storing results
aspredicted <- data.frame(
  id = text_found_as_predicted$id,
  link = text_found_as_predicted$http_aspredicted,
  title = rep(NA_character_, nrow(text_found_as_predicted)),
  created = rep(NA_character_, nrow(text_found_as_predicted)),
  authors = rep(NA_character_, nrow(text_found_as_predicted)),
  Q1 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q2 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q3 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q4 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q5 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q6 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q7 = rep(NA_character_, nrow(text_found_as_predicted)),
  Q8 = rep(NA_character_, nrow(text_found_as_predicted)),
  version = rep(NA_character_, nrow(text_found_as_predicted)),
  stringsAsFactors = FALSE
)

n <- nrow(text_found_as_predicted)

for (i in 1:n) {
  print(i)
  url <- aspredicted$link[i]
  print(url)
  # Process the URL and get answers
  answers <- process_url(url)
  
  # Store the extracted data into the dataframe
  aspredicted$title[i] <- answers$title
  aspredicted$created[i] <- answers$created
  aspredicted$authors[i] <- answers$authors
  aspredicted$Q1[i] <- answers$Q1
  aspredicted$Q2[i] <- answers$Q2
  aspredicted$Q3[i] <- answers$Q3
  aspredicted$Q4[i] <- answers$Q4
  aspredicted$Q5[i] <- answers$Q5
  aspredicted$Q6[i] <- answers$Q6
  aspredicted$Q7[i] <- answers$Q7
  aspredicted$Q8[i] <- answers$Q8
  aspredicted$version[i] <- answers$version
  print(answers$Q1)
  Sys.sleep(20)
  
}

# Remove rows which led to an error (not an actual preregistration)
aspredicted <- aspredicted %>%
  filter(!(Q1 %in% "ERROR"))

# Store the data
save(aspredicted, file = "psychscience_aspredicted_info.RData")
