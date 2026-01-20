library(httr)
library(jsonlite)

# Paste in browser: https://api.osf.io/v2/preprints/?sort=date_created&page=1
# See how many total pages there are (the last you can browse through at the top) and enter below. 

number_of_pages <- 18227            # total number of pages up to date_created 5-24-2024
page_i <- 18227                        # page index for the loop
preprint_data <- list()             # vector with all registration IDs (empty at this point)
project_id <- c()                  # vector with all project IDs (empty at this point)
res_temp <- c()                    # temporary vector used to store data in loop (empty at this point)
registration_date_created <- c()   # vector with date_created of all registrations

page_i = 1
res <- GET(paste("https://api.osf.io/v2/preprints/?sort=date_created&page=", page_i, sep = "")) # access page i
preprints <- fromJSON(rawToChar(res$content))
recent_10_preprints <- preprints$data   # temporarily store data of current page
View(recent_10_preprints)

preprint_id <- recent_10_preprints$id[1]

download_url <- paste0("https://osf.io/",preprint_id,"/download/")
hdr <- HEAD(download_url)
if(!is.null(headers(hdr)$`x-waterbutler-metadata`)){
  filename <- fromJSON(gsub(".*name=", "", headers(hdr)$`x-waterbutler-metadata`))$attributes$name
  file_extension <- sub(".*\\.(.*)$", "\\1", filename)
  destfile1 <- paste0("psyarxiv_preprint/",preprint_id[[i]],".",file_extension)
  download.file(download_url, destfile = destfile1, mode="wb")
}

pdf2grobid("SpatialThermal_preprint.pdf")

paper <- read("SpatialThermal_preprint.xml")
report(
  paper,
  modules = c("code_check", "reference_check", "effect_size", "nonsignificant_pvalue", "marginal", "exact_p", "prereg_check", "causal_claims"),
  output_file = paste0(paper$id, "_report.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report.html"))




# Install RDCOMClient if not already installed
# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")

library(RDCOMClient)

download_and_convert <- function(download_url, dest_dir = "preprint") {
  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) dir.create(dest_dir)
  
  # Extract filename from Content-Disposition header
  resp <- httr::GET(download_url, httr::write_disk(tempfile(), overwrite = TRUE))
  cd_header <- httr::headers(resp)[["content-disposition"]]
  
  if (is.null(cd_header)) stop("Filename not found in headers.")
  
  filename <- sub('.*filename="([^"]+)".*', '\\1', cd_header)
  dest_file <- file.path(dest_dir, filename)
  
  # Move downloaded file to destination
  file.rename(resp$content, dest_file)
  
  # Check extension
  ext <- tolower(tools::file_ext(dest_file))
  
  if (ext %in% c("doc", "docx")) {
    # Convert to PDF using Word COM automation
    pdf_file <- file.path(dest_dir, sub("\\.(docx|doc)$", ".pdf", filename))
    
    wordApp <- COMCreate("Word.Application")
    wordApp[["Visible"]] <- FALSE
    
    doc <- wordApp$Documents()$Open(normalizePath(dest_file))
    doc$SaveAs(normalizePath(pdf_file), FileFormat = 17) # 17 = PDF
    doc$Close()
    wordApp$Quit()
    
    message("Converted to PDF: ", pdf_file)
    return(pdf_file)
  } else {
    message("Downloaded file is not Word format: ", dest_file)
    return(dest_file)
  }
}

# Example usage:
# pdf_path <- download_and_convert("https://osf.io/abcd1/download/")
