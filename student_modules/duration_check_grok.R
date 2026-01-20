#' Duration Check with Grok
#'
#' @description
#' Extracts mentions of experimental session duration and stimulus exposure duration.
#'
#' @author Dana Badeen (\email{d.m.badeen@student.tue.com})
#'
#' @details
#' This module helps identify whether papers report both the duration of the overall
#' experimental session and the duration of individual stimulus presentations.
#' It does this by detecting time expressions like "4 minutes" or "ten seconds" and
#' then classifies them based on nearby context words.
#' Classification is done by the LLM Grok.
#'
#' * Experiment/session duration examples:
#'   - "The testing session lasted 40 minutes."
#'
#' * Stimulus duration examples:
#'   - "Each image was shown for 30 seconds."
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_detect
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' duration_check_grok(psychsci)


duration_check_grok <- function(paper, ...) {


  # --- Part 1 (unchanged from non-LLM version)
  # Find any mentions of time
  #
  # mentions must be in this format <word break><number><space or hyphen><time word><word break>
  # options used:
  #     (?i)        -any capitalization
  #     \b          -word boundary
  #     \d+\.?\d*   -find digit and optional decimal and digits after decimal,
  #     |\b(one|two|three|four|five|six|seven|eight|nine|ten)\b  -or a number word with spaces on either side e.g. " one "
  #     \s?[-–]?    -include hypen or space after number
  #
  pattern <- "(?i)\\b(\\d+\\.?\\d*|\\b(one|two|three|four|five|six|seven|eight|nine|ten)\\b)\\s?[-–]?(milliseconds?|ms\\b|seconds?|secs?|s\\b|mins?|minutes?|hours?|hrs?|h)\\b"

  # make a table of all returned sentences
  duration_table <- search_text(paper, pattern, return = "sentence")



  # --- Part 2 (using Grok)
  # categorize times as experiment or stimulus
  
  # get the surrounding paragraphs, for LLM context lookup
  paragraph_table <- search_text(paper, pattern, return = "paragraph") |>
    dplyr::select(id, paragraph_text = text)

  # match sentences to paragrpahs and merge into one row of the table
  duration_table <- duration_table |>
    rowwise() |>
    mutate(
      paragraph_text = {
        # find the paragraph containing this sentence
        p <- paragraph_table$paragraph_text[stringr::str_detect(paragraph_table$paragraph_text, stringr::fixed(text))]
        if (length(p) > 0) p[1] else NA_character_
      }
    ) |>
  ungroup()

  # Set Grok API key (for testing only)
  GROQ_API_KEY <- "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"
  Sys.setenv(GROQ_API_KEY = GROQ_API_KEY)

  # use Grok to categorize times as experiment or stimulus
  # set a safe cap on LLM calls per paper
  llm_max_calls(100)

  if (nrow(duration_table) == 0) {
    duration_table <- dplyr::tibble(
      id = paper$id,
      section = NA_character_,
      text = NA_character_,
      match = NA_character_,
      duration_type = NA_character_
    )
  } else {

  query <- "These are paragraphs from scientific papers using VR headsets.
      Each includes one or more mentions of time. For the provided time phrase,
      decide whether it refers to the total duration of the experiment, the duration
      of the stimulus, something unrelated (such as rest time, time of day, or setup),
      or if it's unclear.

      Respond only with one of these exact labels:
      experiment_duration, stimulus_duration, neither, unknown.

      If you are unsure or the paragraph does not clearly indicate the type of duration,
      it is better to respond with 'unknown' than to make an incorrect classification."

  # add the full grok input to the table (for debugging)
  duration_table <- duration_table |>
    dplyr::mutate(
      duration_value = purrr::map_chr(text, ~stringr::str_extract(.x, pattern)),
      combined_input = paste0(
        "Time phrase: ", duration_value,
        "\nContext paragraph: ", substring(paragraph_text, 1, 4000)
      )
    )

  # print a count so you can see how many calls are going to the LLM
  message("Sending ", nrow(duration_table), " time mentions to the LLM...")

  # send each row to to the LLM, get a result back, add it to the table
  duration_table$duration_type <- purrr::map_chr(seq_len(nrow(duration_table)), function(i) {
    if (i %% 5 == 0) message("Processed ", i, " of ", nrow(duration_table))
      tryCatch({
        res <- llm(
          data.frame(text = duration_table$combined_input[i]),
          query,
          text_col = "text",
          API_KEY = Sys.getenv("GROQ_API_KEY"),
          maxTokens = 256,
          temperature = 0,
          top_p = 0.95
        )
       
        type_result <- tolower(trimws(res$answer[1]))

        if (type_result %in% c("experiment_duration", "stimulus_duration", "neither", "unknown")) {
          type_result
        } else {
          "unknown"
        }
      }, error = function(e) "unknown")
    })
  }

  #remove any occurrences of "neither" from the table
  #table <- table |> dplyr::filter(type != "neither")


  # --- Part 3 (unchanged from non-LLM version)
  # summary output for paperlists ---- and traffic lights
  # must have id column as the id of each paper, one row per paper
  # further columns to be added to a master summary table
  # summary_table <- dplyr::count(table, id, name = "n_duration")

  # create a summary table (per paper)
  names(duration_table) <- trimws(names(duration_table))
  summary_table <- duration_table |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      n_duration = dplyr::n(),
      n_experiment = sum(duration_type == "experiment_duration", na.rm = TRUE),
      n_stimulus = sum(duration_type == "stimulus_duration", na.rm = TRUE),
      n_unknown = sum(duration_type == "unknown", na.rm = TRUE),
      n_neither = sum(duration_type == "neither", na.rm = TRUE)
    ) |>

    # determine the traffic light (for each paper)
    dplyr::mutate(
      # if theres at least 1 experiment and 1 stimulus duration then green
      # if theres either 1 experiment or 1 stimulus duration then yellow
      # or if its not green and has unknown durations then yellow
      # if theres no experiment or stimulus duration then red
      traffic_light = dplyr::case_when(
        n_experiment >= 1 & n_stimulus >= 1 ~ "green",
        !(n_experiment >= 1 & n_stimulus >= 1) & n_unknown > 0 ~ "yellow_typeB",
        (n_experiment >= 1 & n_stimulus == 0) | (n_experiment == 0 & n_stimulus >= 1) ~ "yellow_typeA",
        n_experiment == 0 & n_stimulus == 0 ~ "red",
        TRUE ~ "na"
      ),

      # add text explanation for the light to the table
      report_text = dplyr::case_match(
        traffic_light,
        "green" ~ "Both experimental session and stimulus durations are reported.",
        "yellow_typeA" ~ "Only one type of duration (experiment or stimulus) is reported.",
        "yellow_typeB" ~ "Durations were mentioned, but their type could not be determined.",
        "red" ~ "No duration information found.",
        "na" ~ "Not applicable.",
        .default = "This table is provided for your information."
      )

    ) |> #close mutate

    # merge yellow_TypeA and yellow_typeB back to just yellow for the table
    dplyr::mutate(
      traffic_light = dplyr::case_when(
        traffic_light %in% c("yellow_typeA", "yellow_typeB") ~ "yellow",
        TRUE ~ traffic_light)
    )


  # determine module level traffic light
  # possible values: na, info, red, yellow, green, fail
  if (all(summary_table$traffic_light == "green")) {
    tl <- "green"
  } else if (any(summary_table$traffic_light == "red")) {
    tl <- "red"
  } else if (any(summary_table$traffic_light == "yellow")) {
    tl <- "yellow"
  } else {
    tl <- "na"
  }

  # report module level text for each possible traffic light ----
  report <- c(
    na = "Not applicable",
    info = "This table is provided for your information",
    red = "This is a potential problem",
    yellow = "There may be a problem",
    green = "No problems found",
    fail = "The check failed, sorry"
  )

  #reorder list
  duration_table <- duration_table |>
  dplyr::select(id, duration_value, duration_type, text, section, header, div, p, s)

  # return a list ----
  list(
    table = duration_table,
    summary= summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}
