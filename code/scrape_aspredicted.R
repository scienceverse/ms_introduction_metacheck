# Function to process a single URL and extract data

url <- "https://aspredicted.org/publi.pdf"
process_aspredicted_url <- function(url) {
  # Get the HTML content
  page <- GET(url) %>% read_html()
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




# process_aspredicted_url <- function(urlæ) {
#   # Fetch the URL
#   resp <- tryCatch(GET(url), error = function(e) stop("Failed to fetch URL: ", e$message))
#   
#   # Validate HTTP status
#   if (http_error(resp)) {
#     stop("HTTP request failed with status: ", status_code(resp))
#   }
#   
#   # Check content type
#   content_type <- headers(resp)$`content-type`
#   if (!grepl("html", content_type, ignore.case = TRUE)) {
#     stop("Content type is not HTML: ", content_type)
#   }
#   
#   # Check size of raw content
#   raw_size <- length(content(resp, "raw"))
#   if (raw_size > max_size) {
#     warning("Content size (", raw_size, " bytes) exceeds limit (", max_size, "). Truncating.")
#     txt <- substr(content(resp, "text"), 1, max_size)
#   } else {
#     txt <- content(resp, "text")
#   }
#   
#   # Parse HTML safely
#   page <- tryCatch(read_html(txt), error = function(e) stop("Failed to parse HTML: ", e$message))
#   
#   # Extract text content
#   text <- html_text2(page)
#   
#   # Split and clean text
#   split_text <- unlist(strsplit(text, "\n"))
#   split_text <- split_text[nchar(gsub("\\s", "", split_text)) > 0]
#   
#   # Initialize answers list
#   answers <- list(title = ifelse(length(split_text) >= 2, split_text[2], "ERROR"))
#   
#   # Helper function for extracting question text
#   extract_question <- function(start_text, end_text, split_text) {
#     tryCatch({
#       start_index <- grep(start_text, split_text)
#       end_index <- grep(end_text, split_text)
#       if (length(start_index) == 0 || length(end_index) == 0) stop("Start or end text not found")
#       paste(split_text[(start_index + 1):(end_index - 1)], collapse = " ")
#     }, error = function(e) "ERROR")
#   }
#   
#   # Populate answers
#   answers$created  <- extract_question("Pre-registered on", "Have any data been collected", split_text)
#   answers$authors  <- extract_question("Author", "Pre-registered on", split_text)
#   answers$Q1       <- extract_question("Have any data been collected", "What's the main question being asked or hypothesis being tested", split_text)
#   answers$Q2       <- extract_question("What's the main question being asked or hypothesis being tested in this study?", "Describe the key dependent variable", split_text)
#   answers$Q3       <- extract_question("Describe the key dependent variable", "How many and which conditions will", split_text)
#   answers$Q4       <- extract_question("How many and which conditions will", "Specify exactly which analyses you will conduct", split_text)
#   answers$Q5       <- extract_question("Specify exactly which analyses you will conduct", "Describe exactly how outliers will be defined|Any secondary analyses", split_text)
#   answers$Q6       <- extract_question("Describe exactly how outliers will be defined|Any secondary analyses", "How many observations will be collected", split_text)
#   answers$Q7       <- extract_question("number will be determined", "Anything else you would like to pre-register", split_text)
#   answers$Q8       <- extract_question("secondary analyses, variables collected", "Version of AsPredicted Questions", split_text)
#   
#   # Extract version
#   version_index <- grep("Version of AsPredicted Questions", split_text)
#   answers$version <- ifelse(length(version_index) > 0, paste(split_text[version_index], collapse = " "), "ERROR")
#   
#   return(answers)
# }

as_predicted_links <- aspredicted_links(xml)

# Keep only unique links
as_predicted_links <- as_predicted_links %>%
  distinct(text, .keep_all = TRUE)

# Keep only unique links
as_predicted_links <- as_predicted_links %>%
  filter(!str_detect(text, regex("blind.php", ignore_case = TRUE)))

# Add .pdf to links that might miss it
as_predicted_links <- as_predicted_links %>%
  mutate(text = case_when(
    grepl("/[A-Za-z0-9]{7}$", text) ~ text,                      # Ends with / + 7 chars → keep as is
    grepl("[A-Za-z0-9]{5}\\.pdf$", text) ~ text,                # Ends with 5 chars + .pdf → keep as is
    grepl("[A-Za-z0-9]{5}$", text) ~ paste0(text, ".pdf"),      # Ends with 5 chars only → add .pdf
    TRUE ~ text                                                 # Otherwise leave unchanged
  ))

rm(xml)

# Initialize the dataframe for storing results
aspredicted <- data.frame(
  id = as_predicted_links$id,
  link = as_predicted_links$text,
  title = rep(NA_character_, nrow(as_predicted_links)),
  created = rep(NA_character_, nrow(as_predicted_links)),
  authors = rep(NA_character_, nrow(as_predicted_links)),
  Q1 = rep(NA_character_, nrow(as_predicted_links)),
  Q2 = rep(NA_character_, nrow(as_predicted_links)),
  Q3 = rep(NA_character_, nrow(as_predicted_links)),
  Q4 = rep(NA_character_, nrow(as_predicted_links)),
  Q5 = rep(NA_character_, nrow(as_predicted_links)),
  Q6 = rep(NA_character_, nrow(as_predicted_links)),
  Q7 = rep(NA_character_, nrow(as_predicted_links)),
  Q8 = rep(NA_character_, nrow(as_predicted_links)),
  version = rep(NA_character_, nrow(as_predicted_links)),
  stringsAsFactors = FALSE
)

n <- nrow(as_predicted_links)

for (i in 111:116) {
  url <- as_predicted_links$text[i]
  print(url)
  print(i)
  # Process the URL and get answers
  answers <- process_aspredicted_url(url)
  
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
  
  Sys.sleep(161)
}

save(aspredicted, file = "psychscience_aspredicted_18112025.RData")
