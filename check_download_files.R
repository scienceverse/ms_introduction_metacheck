library(papercheck)
library(tidyverse)
library(osfr)
library(httr)
library(jsonlite)
library(openxlsx2)

# paper <- psychsci[[103]] will not read, because the xls file is not a well structured datafile. That is a nice check in itself
# paper <- psychsci[88] this has a responseID but I do not think that is the same across surveys - some say yes, some no.
# paper <- psychsci[81] has a .sav file
# paper <- psychsci[181] has csv file


# download_and_summarize_osf_data(psychsci[88]) nice example for multiple excel files
# download_and_summarize_osf_data(psychsci[199]) nice example for multiple files.
# download_and_summarize_osf_data(psychsci[191]) example to explore, has .sav files, does not work well

read_data_file <- function(file_name) {
  if (!file.exists(file_name)) {
    stop("File not found: ", file_name)
  }

  ext <- tolower(tools::file_ext(file_name))

  if (ext %in% c("csv")) {
    # Peek first line to detect delimiter
    first_line <- readLines(file_name, n = 1)

    if (grepl(";", first_line)) {
      # Semicolon delimiter with comma decimals (common in some locales)
      return(read.table(file_name, sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE))
    } else {
      # Default comma delimiter with dot decimals
      return(read.csv(file_name, header = TRUE, stringsAsFactors = FALSE))
    }
  }

  switch(ext,
         xlsx = , xls = readxl::read_excel(file_name),
         sav = haven::read_sav(file_name),
         dta = haven::read_dta(file_name),
         tsv = read.delim(file_name),
         stop("Unsupported file type: .", ext)
  )
}

check_excel_color_coding <- function(file) {
  if (!file.exists(file)) stop("File does not exist: ", file)

  wb <- loadWorkbook(file)
  sheets <- names(wb)
  result <- list()

  for (sheet in sheets) {
    # Read data to get sheet dimensions
    data <- read.xlsx(wb, sheet = sheet)
    if (nrow(data) == 0 || ncol(data) == 0) {
      result[[sheet]] <- FALSE
      next
    }

    styles <- getStyles(wb, sheet = sheet)
    if (length(styles) == 0) {
      # No styles found at all
      result[[sheet]] <- FALSE
      next
    }

    # Check if any cell has a fill color
    has_fill <- any(sapply(styles, function(style) {
      fill <- style$fill
      !is.null(fill) && !is.null(fill$fgFill) && nzchar(fill$fgFill)
    }))

    result[[sheet]] <- has_fill
  }

  return(result)
}

download_and_summarize_osf_data <- function(paper) {
  cat("\n📄 Processing paper:", as.character(paper[[1]]$info$title), "\n")

  # Get OSF links and summaries
  links <- osf_links(paper)
  info <- osf_retrieve(links, recursive = TRUE, find_project = TRUE)
  osf_files_summary <- summarize_contents(info)

  # Filter only usable data files
  osf_files_summary_data <- as.data.frame(subset(
    osf_files_summary,
    best_guess == "data" & osf_type != "nodes" & kind != "folder"
  ))

  # Handle no files case
  if (nrow(osf_files_summary_data) == 0) {
    cat("❌ No usable data files found.\n")
    return(NULL)
  }

  cat("📦 Found", nrow(osf_files_summary_data), "data file(s):\n")
  print(osf_files_summary_data$name)

  # List to store summaries
  summary_list <- list()

  for (i in seq_len(nrow(osf_files_summary_data))) {
    cat("🔁 Iteration", i, "of", nrow(osf_files_summary_data), "...\n")
    file_id <- osf_files_summary_data$osf_id[i]
    file_name <- osf_files_summary_data$name[i]
    project_id <- osf_files_summary_data$parent[i]

    # Determine download URL
    if (nchar(file_id) == 5) {
      file_url <- paste0("https://osf.io/download/", file_id)
    } else {
      api_url <- paste0("https://api.osf.io/v2/nodes/", project_id, "/files/osfstorage/", file_id)
      res <- httr::GET(api_url)
      data <- jsonlite::fromJSON(httr::content(res, "text"))
      file_url <- data$data$links$download
    }

    # Download file to temp file
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_name)))
    cat("⬇️  Downloading:", file_name, "\n")
    download.file(file_url, destfile = temp_file, mode = "wb", method = "libcurl")

    df <- tryCatch({
      data <- read_data_file(temp_file)

      if (is.null(data)) {
        cat("⚠️  Data is NULL.\n")
        data
      } else {
        cat("✅ Loaded:", file_name, "—", nrow(data), "rows,", ncol(data), "columns\n")
        cat("🧐 Preview (first 3 rows, first 5 cols):\n")
        print(head(data[, 1:min(5, ncol(data))], 3))

        # If Excel file, check for color coding
        ext <- tolower(tools::file_ext(file_name))
        if (ext %in% c("xlsx", "xls")) {
          cat("🎨 Checking for color coding in Excel file...\n")
          color_info <- check_excel_color_coding(temp_file)

          if (length(color_info) > 0) {
            for (sheet_name in names(color_info)) {
              n_colored <- color_info[[sheet_name]]$colored_cells
              cat(sprintf("   Sheet '%s' has %d colored cell(s).\n", sheet_name, n_colored))
            }
          } else {
            cat("   No colored cells detected in any sheet.\n")
          }
        }

        data
      }
    }, error = function(e) {
      cat("❌ Error reading", file_name, ":", conditionMessage(e), "\n")
      NULL
    })

    # Store summary
    if (!is.null(df)) {
      summary_list[[file_name]] <- summary(df)
    }
  }

  return(summary_list)
}

res <- download_and_summarize_osf_data(psychsci[88])

