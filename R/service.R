#' Build a gtsummary table with optional grouping, formatting, p-values, and image saving
#'
#' @description
#' Creates a `gtsummary::tbl_summary` object from a data frame with optional
#' grouping variable, column inclusion, statistic formats, p-values, and bold labels.
#' Also returns plain text output and metadata about variables used.
#' Additionally, this function saves the table as an image file (`PNG` by default) using [gt::gtsave()]. # ADDED
#'
#' @param data A data frame containing the variables to summarize.
#' @param by Optional. Name of a grouping variable (character string) or `NULL`.
#' @param include Optional. Vector of column names to include; `NULL` means all columns.
#' @param stat_cont String specifying the statistic format for continuous variables.
#' @param stat_cat String specifying the statistic format for categorical variables.
#' @param digits_cont Integer, number of decimal places for continuous variables.
#' @param missing_policy String indicating how missing data are handled.
#' @param add_p Logical, whether to add p-values to the summary table.
#' @param bold Logical, whether to bold variable labels.
#'
#' @return A list containing:
#' \describe{
#'   \item{tbl}{`gtsummary` object.}
#'   \item{text}{Plain-text table.}
#'   \item{vars_used}{Vector of variable names used.}
#'   \item{vars_info}{String with variable name and class pairs.}
#'   \item{by}{Name of grouping variable (or `NULL`).}
#'   \item{image}{Saved PNG file of the table.} # ADDED
#' }
#'
#' @details # ADDED
#' The image is saved to a fixed file name (`table.png`) in the working directory by default.
#' You can modify the `gt::gtsave()` call inside the function if you want custom file names or formats.
#'
#' @importFrom gtsummary tbl_summary add_p bold_labels all_continuous all_categorical as_tibble as_gt
#' @importFrom gt gtsave
#' @importFrom tibble as_tibble
#' @importFrom utils capture.output
#' @importFrom assertthat assert_that
#' @export
gts_build <- function(data,
                      by = NULL,              # name of grouping variable (string) or NULL
                      include = NULL,         # vector of column names for analysis; NULL = all
                      stat_cont = "{mean} ± {sd}",
                      stat_cat  = "{n} ({p}%)",
                      digits_cont = 1,
                      missing_policy = "no",
                      add_p = TRUE,
                      bold  = TRUE) {

  # ---- Input checks ----
  assertthat::assert_that(is.data.frame(data), msg = "`data` must be a data frame.")
  assertthat::assert_that(is.null(by) || is.character(by) && length(by) == 1,
                          msg = "`by` must be NULL or a single string.")
  assertthat::assert_that(is.null(include) || is.character(include),
                          msg = "`include` must be NULL or a character vector.")
  assertthat::assert_that(is.character(stat_cont), length(stat_cont) == 1)
  assertthat::assert_that(is.character(stat_cat), length(stat_cat) == 1)
  assertthat::assert_that(is.numeric(digits_cont), length(digits_cont) == 1)
  assertthat::assert_that(is.character(missing_policy), length(missing_policy) == 1)
  assertthat::assert_that(is.logical(add_p), length(add_p) == 1)
  assertthat::assert_that(is.logical(bold), length(bold) == 1)

  df <- data

  # 1) Narrow the set of columns if `include` is specified
  if (!is.null(include)) {
    miss <- setdiff(include, names(df))
    if (length(miss)) stop("Not in data: ", paste(miss, collapse = ", "))
    df <- df[, include, drop = FALSE]
  }

  # 2) Strict handling of the `by` variable
  if (!is.null(by)) {
    if (!by %in% names(data)) {
      stop(sprintf("Grouping variable '%s' not found in original data.", by))
    }
    if (!is.null(include) && !by %in% names(df)) {
      include <- c(by, include)
      df <- data[, include, drop = FALSE]
    }
    if ((is.character(df[[by]]) || is.logical(df[[by]])) && !is.factor(df[[by]])) {
      df[[by]] <- as.factor(df[[by]])
    }
  }

  # 3) Formats for statistics
  cont_fmt <- gtsummary::all_continuous()  ~ stat_cont
  cat_fmt  <- gtsummary::all_categorical() ~ stat_cat

  # 4) Build the summary table
  if (!is.null(by)) {
    call_tbl <- substitute(
      gtsummary::tbl_summary(
        data = DF,
        by   = BY,
        statistic = list(CONT, CAT),
        digits    = gtsummary::all_continuous() ~ DIG,
        missing   = MISS
      ),
      list(
        DF   = df,
        BY   = as.name(by),
        CONT = cont_fmt,
        CAT  = cat_fmt,
        DIG  = digits_cont,
        MISS = missing_policy
      )
    )
    tbl <- eval(call_tbl)
  } else {
    tbl <- gtsummary::tbl_summary(
      data = df,
      statistic = list(cont_fmt, cat_fmt),
      digits    = gtsummary::all_continuous() ~ digits_cont,
      missing   = missing_policy
    )
  }

  if (isTRUE(add_p)) tbl <- gtsummary::add_p(tbl)
  if (isTRUE(bold))  tbl <- gtsummary::bold_labels(tbl)

  # 5) Plain-text version
  tmp <- gtsummary::as_tibble(tbl, col_labels = TRUE)
  tmp <- as.data.frame(tmp)
  tbl_text <- paste(capture.output(print(tmp, row.names = FALSE)), collapse = "\n")

  # Save table as image # ADDED
  gt::gtsave(
    gtsummary::as_gt(tbl),
    "table.png",
    vwidth = 800,
    vheight = 1000,
    zoom = 1,
    expand = 5
  )

  # 6) Metadata
  vars_used <- names(df)
  vars_info <- paste0(vars_used, ":", vapply(df, function(x) class(x)[1], ""), collapse = ", ")

  list(
    tbl       = tbl,
    text      = tbl_text,
    vars_used = vars_used,
    vars_info = vars_info,
    by        = by,
    image     = "table.png" # ADDED
  )
}


#' Interpret a gtsummary table using an LLM (e.g., GPT model)
#'
#' This function takes a gtsummary table, formats it into text, and sends it to an OpenAI model
#' for interpretation based on the specified style, language, verbosity, and other parameters.
#'
#' @param tbl_summary_in A gtsummary table or list-like object to be interpreted.
#' @param style Character. Output style, one of `"publication"`, `"clin_report"`, `"report"`, `"popular"`.
#' @param language Character. Language for the LLM response. Default is `"en"`.
#' @param sure Logical. If `TRUE`, the LLM should answer confidently; otherwise, cautiously.
#' @param verbosity Character. Controls the verbosity of the LLM's output.
#' @param instructions Character. Additional instructions for the LLM.
#' @param varriabes_for_stats Character vector. Variables to focus on for statistical analysis.
#' @param verbose_function_mode Logical. If `TRUE`, prints debugging information.
#' @param model Character. The LLM model to use (e.g., `"gpt-4o"`).
#' @param formality Numeric (0–1). Controls creativity/formality in the LLM output.
#' @param ai_key Character. Either the OpenAI API key or a file path containing the key.
#'
#' @return A character string containing the interpretation from the LLM.
#' @examples
#' \dontrun{
#' interpret_table(my_tbl, style = "publication", language = "en", ai_key = "my_api_key")
#' }
#' @importFrom assertthat assert_that
#' @importFrom gtsummary as_tibble
#' @importFrom tibble as_tibble
#' @importFrom utils capture.output
#' @importFrom openai create_chat_completion
#' @export
interpret_table <- function(tbl_summary_in,
                            style = c("publication", "clin_report", "report", "popular"),
                            language = "en", sure = TRUE,
                            verbosity = "moderate", instructions = "",
                            varriabes_for_stats = c(), verbose_function_mode = FALSE,
                            model = "gpt-4o", formality = 0.2, ai_key = "Evgeny") {

  # --- Input validation (added) ------------------------------------------------
  assertthat::assert_that(!missing(tbl_summary_in),
                          msg = "`tbl_summary_in` must be provided.")
  assertthat::assert_that(is.list(tbl_summary_in) || inherits(tbl_summary_in, "gtsummary"),
                          msg = "`tbl_summary_in` should be a gtsummary object or list-like object produced by gtsummary.")
  assertthat::assert_that(is.character(style),
                          msg = "`style` must be character (one of the permitted styles).")
  assertthat::assert_that(is.character(language), length(language) == 1,
                          msg = "`language` must be a single string.")
  assertthat::assert_that(is.logical(sure), length(sure) == 1,
                          msg = "`sure` must be a logical scalar.")
  assertthat::assert_that(is.character(verbosity), length(verbosity) == 1,
                          msg = "`verbosity` must be a single string.")
  assertthat::assert_that(is.character(instructions), length(instructions) == 1,
                          msg = "`instructions` must be a single string.")
  assertthat::assert_that(is.character(varriabes_for_stats) || length(varriabes_for_stats) == 0,
                          msg = "`varriabes_for_stats` must be a character vector (or empty).")
  assertthat::assert_that(is.logical(verbose_function_mode), length(verbose_function_mode) == 1,
                          msg = "`verbose_function_mode` must be a logical scalar.")
  assertthat::assert_that(is.character(model), length(model) == 1, nchar(model) > 0,
                          msg = "`model` must be a non-empty string.")
  assertthat::assert_that(is.numeric(formality), length(formality) == 1,
                          !is.na(formality), formality >= 0, formality <= 1,
                          msg = "`formality` must be numeric in [0, 1].")
  assertthat::assert_that(is.character(ai_key), length(ai_key) == 1,
                          msg = "`ai_key` must be a single string (API key or path).")
  # ------------------------------------------------------------------------------

  ## ---- API key handling -----------------------------------------------------
  if (is_file_or_key(ai_key) == "file") {
    # If ai_key is a file, read the key from it
    ai_key <- readLines(ai_key)
  } else if (is_file_or_key(ai_key) == "chatgpt_key") {
    # If ai_key is a key, use it directly
    ai_key <- ai_key
  } else if (is_file_or_key(ai_key) == "unknown") {
    stop("Invalid OpenAI API key or file path provided.")
  }

  assertthat::assert_that(is.character(ai_key), length(ai_key) >= 1, nchar(ai_key[1]) > 0,
                          msg = "OpenAI API key could not be read or is empty.")
  Sys.setenv(OPENAI_API_KEY = ai_key)

  ## ---- Define the style -----------------------------------------------------
  style <- match.arg(style, c("publication", "clin_report", "report", "popular"))
  if (style == "publication") {
    context <- "For a research paper"
  } else if (style == "clin_report") {
    context <- "For a clinical report"
  } else if (style == "report") {
    context <- "For an activity report"
  } else if (style == "popular") {
    context <- "For a popular science article"
  }

  ## ---- Convert table to text for LLM ----------------------------------------
  tbl_text <- tbl_summary_in %>%
    as_tibble(col_labels = TRUE) %>%
    as.data.frame()
  tbl_text <- paste(capture.output(print(tbl_text, row.names = FALSE)), collapse = "\n")

  ## ---- Adding sure or unsure mode -------------------------------------------
  if (sure) {
    sure_mode <- "Always say in a self-confident manner what you think about the data. NEVER use words like presumably (IMPORTANT)"
  } else {
    sure_mode <- "Be careful and do not make any assumptions about the data. If you are not sure, say so"
  }

  ## ---- Compose the prompt ---------------------------------------------------
  # Make up a message to ChatGPT from the user
  user_message <- paste0(
    "Output shall not include any preface from you, just description of the data. This is the context output should suit: ", context,
    "; The verbosity of your answer should be: ", verbosity,
    "; ", sure_mode,
    "; Here are special instructions (if any): ", instructions,
    "; Please make the answer in the following language: ", language
  )

  if (verbose_function_mode) {
    print("Now the user message:")
    print(user_message)
    print("# Table text:")
    print(tbl_text)
  }

  ## ---- Call the LLM ---------------------------------------------------------
  res <- openai::create_chat_completion(
    model = model,
    messages = list(
      list(role = "system", content = "You are a medical statistician."), # Assigns the model's role
      list(role = "user", content = paste(
        "Please interpret the gtsummary table (significant differences):\n\n", tbl_text,
        "\n\n regarding the following columns: ",
        user_message
      ))
    ),
    temperature = formality
  )

  return(res$choices[["message.content"]])
}
