# bib references add dois
bib_add_dois("references.bib", strict = FALSE)

#' Add DOIs to a bib file 
#' 
#' Uses OpenAlex to search for items that match the title and journal of bibtex entries that don't have a DOI and adds them in.
#'
#' @param bibfile The file path to the .bib file
#' @param save_to The file to save the results to; if NULL, saves to bibfile name with _doi appended
#' @param strict Should there be a single exact match for title and journal, if FALSE, gives the best match
#'
#' @returns a bib table in the bib2df format
#' @export
#'
#' @examples
bib_add_dois <- function(bibfile, 
                         save_to = NULL, 
                         strict = TRUE,
                         email = "debruine@gmail.com") {
  if (is.null(save_to)) {
    save_to <- sub("\\.bib$", "_doi.bib", bibfile)
  }

  df <- suppressMessages(
    suppressWarnings(
      bib2df::bib2df(bibfile)
    )
  )
  
  if (!"DOI" %in% names(df)) df$DOI <- NA_character_
  
  old_doi <- df$DOI
  
  fields <- c(
    "id",
    "doi",
    "relevance_score",
    "display_name",
    "publication_year",
    "primary_location",
    # "authorships", # not using authors yet
    "type",
    "biblio"
  ) |>
    paste0(collapse = ",")
  
  df$DOI <- sapply(seq_along(df$TITLE), \(i) {
    # message(i, ": ", substr(df$TITLE[[i]], 1, 60), "...")
    
    if (!is.na(df$DOI[i])) return(df$DOI[[i]])
    if (df$CATEGORY[i] != "ARTICLE") return(df$DOI[[i]])
    
    url <- paste0(
      "https://api.openalex.org/works?filter=title.search:",
      utils::URLencode(gsub(",", "", df$TITLE[[i]])), 
      "&mailto=", email, 
      "&select=", fields
    )
    
    j <- tryCatch(jsonlite::read_json(url),
                  error = \(e) {})
    
    if (is.null(j$results) || length(j$results) == 0) {
      message(i, ": no results from OpenAlex")
      return(NA_character_)
    }
    
    info <- lapply(j$results, \(res) {
      res$journal <- res$primary_location$source$display_name
      res$primary_location <- NULL
      # res$authors <- sapply(res$authorships, \(a) a$raw_author_name)
      # res$authorships <- NULL
      unlist(res)
    }) |> do.call(dplyr::bind_rows, args = _) |>
      dplyr::arrange(dplyr::desc(relevance_score))
    
    required_cols <- c("display_name", "journal")
    for (rq in required_cols) {
      if (!rq %in% names(info)) info[[rq]] <- ""
    }
    
    info$title_match <- tolower(info$display_name) == tolower(df$TITLE[[i]])
    info$journal_match <- tolower(info$journal) == tolower(df$JOURNAL[[i]])
    
    matches <- dplyr::filter(info, title_match, journal_match)
    
    if (nrow(matches) == 1) {
      return(matches$doi[[1]])
    } else if (nrow(matches) > 1) {
      message(i, ": multiple title/journal matches")
      if (strict) return(NA_character_)
      return(matches$doi[[1]])
    }
    
    matches <- dplyr::filter(info, title_match)
    if (nrow(matches) == 1) {
      message(i, ": matches title, not journal")
      if (strict) return(NA_character_)
      return(matches$doi[[1]])
    } else if (nrow(matches) > 1) {
      message(i, ": multiple title matches")
      if (strict) return(NA_character_)
      return(matches$doi[[1]])
    }
    
    message(i, ": no title/journal exact matches")
    if (strict) return(NA_character_)
    return(info$doi[[1]])
  })
  
  new_doi <- df$DOI
  
  dois_added <- sum(!is.na(new_doi)) - sum(!is.na(old_doi))
  
  message(dois_added, " new DOIs added")
  
  bib2df::df2bib(df, save_to, )
  
  invisible(df)
}
