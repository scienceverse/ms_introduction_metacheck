# To make a module our of this we should also include the OSF view only check, and the aspredicted blind link check. 

all_url <- metacheck::module_run(psychsci, "all_urls")
all_url <- unique(all_url$table["text"])

regex_data_repositories <- "https://(?:(?:www\\.)?github(?:\\.com)?|(?:www\\.)?osf\\.io|(?:dx\\.)?doi\\.org10\\.[0-9]+/[^/\\s]*|(?:www\\.)?figshare\\.com|(?:www\\.)?zenodo\\.org|(?:www\\.)?datadryad\\.org|(?:www\\.)?kaggle\\.com|(?:www\\.)?cdc\\.gov|data\\.mendeley\\.com|hdl\\.handle\\.net|nhiss\\.nhis\\.or\\.kr|(?:www\\.)?gitlab(?:\\.com)?|dep\\.mohw\\.gov\\.tw|(?:www\\.)?ukbiobank\\.ac\\.uk|(?:www\\.)?ices\\.on\\.ca|knhanes|(?:www\\.)?openicpsr(?:\\.org)?|(?:www\\.)?who\\.int|(?:www\\.)?stats\\.gov|proteomecentral(?:\\.org)?|seer\\.cancer\\.gov|portal\\.gdc\\.cancer\\.gov|databank(?:\\.org)?|archive\\.ics\\.uci\\.edu(?:m)?|biolincc\\.nhlbi\\.nih\\.gov|physionet\\.org|(?:www\\.)?hcup-us\\.ahrq\\.gov|(?:www\\.)?datatilsynet(?:\\.dk)?|bitbucket\\.org|gwas\\.mrcieu\\.ac\\.uk|nhird\\.nhri(?:\\.org\\.tw)?|mics\\.unicef\\.org|massive\\.ucsd\\.edu|data\\.world|ghdx\\.healthdata\\.org|(?:www\\.)?census\\.gov|(?:www\\.)?ibge\\.gov\\.br|datacompass\\.lshtm\\.ac\\.uk|(?:www\\.)?scb\\.se|etikprovning(?:\\.se)?|mimic(?:\\.physionet\\.org)?|simtk\\.org|microdat(?:\\.no)?|doi\\.pangaea\\.de|(?:www\\.)?icddrb\\.org|(?:www\\.)?measuredhs(?:\\.com)?|beta\\.ukdataservice\\.ac\\.uk|reshare\\.ukdataservice\\.ac\\.uk|flowrepository\\.org|(?:www\\.)?mhlw\\.go\\.jp|dataview\\.ncbi\\.nlm\\.nih(?:\\.gov)?|(?:www\\.)?share-project\\.org|(?:www\\.)?socialstyrelsen\\.se|pubmlst\\.org|(?:www\\.)?clinicalstudydatarequest\\.com|(?:www\\.)?ddbj\\.nig\\.ac\\.jp|bigd\\.big\\.ac\\.cn|data\\.csmar\\.com|data\\.humdata\\.org|ec\\.europa\\.eu(?:/)?eurostat|(?:www\\.)?cpc\\.unc\\.edu|(?:www\\.)?resdac\\.org|digital\\.nhs(?:\\.uk)?|fieldresearch\\.msf\\.org|(?:www\\.)?tropmedres\\.ac(?:\\.th)?|(?:www\\.)?morphosource\\.org|(?:www\\.)?uniprot\\.org|datasus\\.saude\\.gov(?:\\.br)?|ourworldindata\\.org|(?:www\\.)?pmadata\\.org|(?:www\\.)?naturalearthdata\\.com|(?:www\\.)?ncbi\\.nlm\\.nih\\.gov(?:geo|genbank)?|data\\.(?:[a-z]+\\.)?[a-z]+|dataverse\\.(?:[a-z]+\\.)?[a-z]+|opendata(?:\\.[a-z]+\\.[a-z]+)?|research(?:\\.[a-z]+\\.[a-z]+)?)"

data_repositories <- search_text(all_url, regex_data_repositories, return = "sentence")





filtered_url <- all_url %>%
  mutate(text = str_trim(text)) %>% 
  filter(str_detect(text, regex("^(https?://|www\\.)", ignore_case = TRUE))) %>%
  filter(!coalesce(str_detect(text, regex("osf\\.io|aspredicted|/doi|dx.doi|orcid.org|sagepub|psychologicalscience.org/publications", 
                                          ignore_case = TRUE)), FALSE))

# Can we fix some links, by replacing > . with .? 

# ---- Helper: robust check for a single URL
check_url_live <- function(raw_url, timeout_sec = 5) {
  # trim whitespace
  url <- trimws(raw_url)
  
  # Return a consistent "failure" object on empty/NA
  if (is.na(url) || identical(url, "")) {
    return(list(ok = FALSE, status = NA_integer_, final_url = NA_character_))
  }
  
  # Build candidate URLs:
  # - If starts with "www.", try https then http
  # - If already has http/https, use as-is
  # - Otherwise, try both https and http (defensive)
  if (grepl("^www\\.", url, ignore.case = TRUE)) {
    candidates <- c(paste0("https://", url), paste0("http://", url))
  } else if (grepl("^https?://", url, ignore.case = TRUE)) {
    candidates <- c(url)
  } else {
    candidates <- c(paste0("https://", url), paste0("http://", url))
  }
  
  # Common request config: follow redirects, set UA, set timeout
  ua <- user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) R/httr"
  )
  cfg <- config(followlocation = TRUE)
  to  <- timeout(timeout_sec)
  
  request_once <- function(u, method = "HEAD") {
    # Return NULL on any network/SSL/DNS error
    res <- tryCatch(
      {
        if (identical(method, "HEAD")) {
          httr::HEAD(u, ua, cfg, to)
        } else {
          httr::GET(u, ua, cfg, to)
        }
      },
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    list(status = httr::status_code(res), final_url = res$url)
  }
  
  # Try each candidate: HEAD first, then GET fallback
  for (u in candidates) {
    # 1) HEAD (fast; but some servers block or misconfigure it)
    head_res <- request_once(u, "HEAD")
    if (!is.null(head_res) && head_res$status < 400) {
      return(list(ok = TRUE, status = head_res$status, final_url = head_res$final_url))
    }
    
    # 2) Fallback to GET if HEAD is blocked (405/403) or failed
    need_get <- is.null(head_res) || head_res$status %in% c(405, 403)
    if (need_get) {
      get_res <- request_once(u, "GET")
      if (!is.null(get_res) && get_res$status < 400) {
        return(list(ok = TRUE, status = get_res$status, final_url = get_res$final_url))
      }
    }
    
    # Otherwise, move to the next candidate (e.g., try http after https)
  }
  
  # Nothing worked
  list(ok = FALSE, status = NA_integer_, final_url = NA_character_)
}

# Preallocate vectors for speed and clean types
n <- nrow(filtered_url)
checked      <- integer(n)
http_status  <- rep(NA_integer_, n)
resolved_url <- rep(NA_character_, n)

# Loop through rows
for (i in seq_len(n)) {
  res <- check_url_live(filtered_url$text[i], timeout_sec = 5)
  checked[i]      <- if (isTRUE(res$ok)) 1 else 0
  http_status[i]  <- if (is.null(res$status)) NA_integer_ else res$status
  resolved_url[i] <- if (is.null(res$final_url)) NA_character_ else res$final_url
  
  # (optional) simple progress every 200 rows:
  message("Checked ", i, " / ", n)
}

# Add columns to your tibble
filtered_url$checked      <- checked
filtered_url$http_status  <- http_status
filtered_url$resolved_url <- resolved_url

save(filtered_url, file = "links_down_psych_science.RData")

# Above we remove AsPredicted and OSF links. For those, we should check if they are view only. 


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

RGX_ASPREDICTED_BLIND <- "http://aspredicted\\.org/blind\\.php\\?x=s*[a-zA-Z0-9]{6}"
text_found_as_predicted_blind <- search_text(text_found, RGX_ASPREDICTED_BLIND, return = "match")

# Remove duplicates
text_found_as_predicted_blind <- text_found_as_predicted_blind %>% distinct(text, .keep_all = TRUE)

# Define the regular expression pattern for detecting OSF URLs 
RGX_VIEWONLY <- "view_only"
text_found <- search_text(xml, RGX_VIEWONLY, return = "paragraph")
text_found$text <- gsub("/\\? view_only", "/?view_only", text_found$text)

text_found <- text_found %>% distinct(text, .keep_all = TRUE)


# And I want to also fold in the existing module "OSF_check" into this module. 

#' Check Status of OSF Links
#'
#' @description
#' List all OSF links and whether they are open, closed, or do not exist.
#'
#' @author Daniel Lakens
#'
#' @import httr
#' @import dplyr
#' @import progress
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci[1:10], "osf_check")
osf_check <- function(paper, ...) {
  # test private: url = "https://osf.io/5tbm9"
  # test public: url = "https://osf.io/629bx"
  
  # check for OSF link ----
  found_osf <- osf_links(paper)
  
  if (nrow(found_osf) == 0) {
    table <- data.frame(id = character(0))
    summary_table <- data.frame(id = character(0))
  } else {
    # detailed table of results ----
    table <- osf_retrieve(found_osf)
    table$status <- dplyr::case_when(
      is.na(table$osf_id) ~ "invalid",
      table$osf_type == "too many requests" ~ "too many requests",
      table$osf_type == "unfound" ~ "unfound",
      table$osf_type == "private" ~ "closed",
      table$public == FALSE ~ "closed",
      table$public == TRUE ~ "open",
      !is.na(table$osf_type) ~ "open",
      .default = ""
    )
    
    # summary output for paperlists ----
    summary_table <- dplyr::count(table, id, status) |>
      tidyr::pivot_wider(names_from = status,
                         names_prefix = "osf.",
                         values_from = n, values_fill = 0)
  }
  
  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(table) == 0 ~ "na",
    all(table$status == "error") ~ "fail",
    all(table$status == "unknown") ~ "fail",
    any(table$status == "unfound") ~ "red",
    any(table$status == "invalid") ~ "red",
    any(table$status == "closed") ~ "red",
    all(table$status == "open") ~ "green",
    .default = "yellow"
  )
  
  report = c(
    na = "No OSF links were detected",
    red = "We detected closed OSF links",
    yellow = "There may be problems with some OSF links",
    green = "All OSF links are open",
    fail = "All attempts to check OSF links failed; check if you are offline."
  )
  
  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}