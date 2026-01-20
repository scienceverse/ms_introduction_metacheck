# New function, needs to be added to metacheck. 
get_doi <- function(reference) {
  options(crossref_email = "d.lakens@tue.nl")
  tryCatch({
    res <- cr_works(query = reference, limit = 1)
    if (nrow(res$data) > 0) {
      return(res$data$doi[1])
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}