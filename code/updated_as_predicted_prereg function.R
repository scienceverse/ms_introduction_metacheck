aspredicted_links <- function(paper) {
  # Step 1: Search for "aspredicted"
  RGX_ASPREDICTED <- "/aspredicted\\s*\\.?\\s*org"
  found_ap <- search_text(paper, RGX_ASPREDICTED)
  
  # Step 2: Fix blind.php? with x=abcdef in the next sentence
  blind <- grep("blind\\.php\\?$", found_ap$text)
  expanded <- expand_text(found_ap[blind, ], paper, plus = 1)
  found_ap$text[blind] <- expanded$expanded
  
  # Step 3: Fix spacing issues
  found_ap$text <- gsub(RGX_ASPREDICTED, "/aspredicted.org", x = found_ap$text)
  found_ap$text <- gsub("blind\\.php\\s*\\?\\s*x\\s*=\\s*", "blind.php?x=", x = found_ap$text)
  
  # Step 4: Match full links
  RGX_FULL <- "/aspredicted\\.org/(?:[A-Z0-9]{3}_[A-Z0-9]{3}|[a-zA-Z0-9]{5}(?:\\.pdf)?|blind\\.php\\?x=[A-Z0-9_]+)"
  match_ap <- search_text(found_ap, RGX_FULL, return = "match")
  
  # Step 5: Clean up the text
  match_ap$text <- match_ap$text |>
    gsub("\\s", "", x = _) |>
    gsub("[\\.;:)]*$", "", x = _) |>  # removes trailing punctuation
    paste0("https:/", x = _)
  
  # Step 6: Remove trailing blind links
  unique_matches <- match_ap |>
    dplyr::filter(text != "https://aspredicted.org/blind.php?") |>
    unique()
  
  return(unique_matches)
}
