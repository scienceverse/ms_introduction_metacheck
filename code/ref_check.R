#' P Value Check
#'
#' @description
#' This module checks for imprecisely reported p values. If p > .05 is detected, it warns for misinterpretations. 
#'
#' @author  Lisa DeBruine (\email{lisa.debruin@glasgow.ac.uk}) and Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @references
#' # Appelbaum, M., Cooper, H., Kline, R. B., Mayo-Wilson, E., Nezu, A. M., & Rao, S. M. (2018). Journal article reporting standards for quantitative research in psychology: The APA Publications and Communications Board task force report. American Psychologist, 73(1), 3–25. https://doi.org/10.1037/amp0000191
#' # Murphy, S. L., Merz, R., Reimann, L.-E., & Fernández, A. (2025). Nonsignificance misinterpreted as an effect’s absence in psychology: Prevalence and temporal analyses. Royal Society Open Science, 12(3), 242167. https://doi.org/10.1098/rsos.242167
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "nonsig")
nonsig <- function(paper) {
    # detailed table of results ----
    res_p <- module_run(paper, "all_p_values")
    
    res_p$table$significance <- ifelse((res_p$table$p_value < .05), "significant", "nonsignificant")
    res_p$table <- subset(res_p$table, significance == "nonsignificant")
    
    # Expand the sentences so the full sentence can be seen
    res_p$table <- expand_text(
      res_p$table,
      paper,
      expand_to = c("sentence")
    )

    table <- res_p$table
    
  # summary output for paperlists ----
  # must have id column as the id of each paper, one row per paper
  # further columns to be added to a master summary table
  summary_table <- table %>%
    group_by(id) %>%
    summarise(
      n_significant = sum(significance == "significant", na.rm = TRUE),
      n_nonsignificant = sum(significance == "nonsignificant", na.rm = TRUE)
    )  
  # determine the traffic light ----
  # possible values: na, info, red, yellow, green, fail
  tl <- if (sum(summary_table$n_nonsignificant) > 0) "info" else "green"
  
  if (nrow(table) == 0) {
    report <- "We detected no nonsignificant p-values"
  } else {

    report <- "We found non-significant p-values. \n\nMeta-scientific research has shown non-significant p-values are commonly misinterpreted. It is incorrect to infer that there is 'no effect', 'no difference', or that groups are 'the same' after p > 0.05. \n\nIt is possible that there is a true non-zero effect, but that the study did not detect it. Make sure your inference acknowledges that it is possible that there is a non-zero effect. It is correct to include the effect is 'not significantly' different, which just restates that p > 0.05. \n\nMetacheck does not yet analyze automatically whether sentences which include non-significant p-values are correct, but we recommend manually checking the sentences below for possible misinterpreted non-significant p-values."
    sentences <- paste(table$expanded, collapse = "\n")
    # report text 
    report <- sprintf("%s\n\n**%s**",
                    report, sentences)
  }
    
  
  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report
  )
}
