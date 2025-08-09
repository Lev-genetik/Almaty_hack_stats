#' Interpret a summary table and generate a Word report
#'
#' Reads a CSV file, builds a grouped descriptive table with
#' `gtsummary::tbl_summary()`, optionally narrows the table to a set of
#' variables, asks an LLM to draft an interpretation, and saves a .docx
#' report containing the table and the generated narrative.
#'
#' @param file_path Character scalar. Path to the input CSV file.
#' @param output_file Character scalar. Path to the output Word file
#'   (should end with `.docx`).
#' @param context Character scalar. High-level context for the LLM prompt
#'   (e.g., "For a research paper").
#' @param language Character scalar. Output language code or name (e.g., "en").
#' @param verbosity Character scalar. Desired verbosity of the LLM response.
#' @param instructions Character scalar. Any special instructions to pass through.
#' @param varriabes_for_stats Character vector of variable names to keep in the
#'   short table, or `NA` to keep all variables (note the original parameter
#'   spelling is preserved).
#' @param model Character scalar. Model name for `openai::create_chat_completion()`.
#' @param formality Numeric scalar in \[0, 1\]. Passed to `temperature`.
#' @param ai_key Character scalar. OpenAI API key.
#'
#' @return Character scalar: the generated interpretation text.
#'
#' @details
#' The input CSV is expected to contain a column named `group`, which is
#' used for the `by = group` argument in `gtsummary::tbl_summary()`.
#' This function assumes required packages are installed and accessible.
#'
#' @examples
#' \dontrun{
#' out <- interpret_table(
#'   file_path = "data/example.csv",
#'   output_file = "report.docx",
#'   context = "For a research paper",
#'   language = "en",
#'   verbosity = "moderate",
#'   instructions = "",
#'   varriabes_for_stats = NA,
#'   model = "gpt-4o",
#'   formality = 0.2,
#'   ai_key = Sys.getenv("OPENAI_API_KEY")
#' )
#' }
#'
#' @importFrom readr read_csv
#' @importFrom gtsummary tbl_summary add_p bold_labels as_flex_table
#' @importFrom dplyr select all_of
#' @importFrom tibble as_tibble
#' @importFrom flextable fontsize autofit
#' @importFrom openai create_chat_completion
#' @importFrom assertthat assert_that
#' @export
interpret_table <- function(file_path,
                            output_file,
                            context = "For a research paper", language = "en",
                            verbosity = "moderate", instructions = "",
                            varriabes_for_stats = NA,
                            model = "gpt-4o", formality = 0.2, ai_key) {

  ## ---- Input checks (do not alter downstream code) -------------------------
  # Basic type/length checks
  assertthat::assert_that(is.character(file_path), length(file_path) == 1,
                          nchar(file_path) > 0,
                          msg = "`file_path` must be a non-empty character scalar.")
  assertthat::assert_that(file.exists(file_path),
                          msg = "Input `file_path` does not exist.")
  assertthat::assert_that(is.character(output_file), length(output_file) == 1,
                          nchar(output_file) > 0,
                          msg = "`output_file` must be a non-empty character scalar.")
  assertthat::assert_that(grepl("\\.docx$", output_file, ignore.case = TRUE),
                          msg = "`output_file` should end with '.docx'.")
  assertthat::assert_that(is.character(context), length(context) == 1)
  assertthat::assert_that(is.character(language), length(language) == 1)
  assertthat::assert_that(is.character(verbosity), length(verbosity) == 1)
  assertthat::assert_that(is.character(instructions), length(instructions) == 1)
  assertthat::assert_that(is.numeric(formality), length(formality) == 1,
                          !is.na(formality), formality >= 0, formality <= 1,
                          msg = "`formality` must be numeric in [0, 1].")
  assertthat::assert_that(is.character(model), length(model) == 1,
                          nchar(model) > 0, msg = "`model` must be a non-empty string.")
  assertthat::assert_that(!missing(ai_key), is.character(ai_key),
                          length(ai_key) == 1, nchar(ai_key) > 0,
                          msg = "`ai_key` must be provided as a non-empty string.")

  # Package availability (quiet check to avoid altering code below)
  .pkgs_needed <- c("readr", "gtsummary", "dplyr", "tibble", "flextable", "officer", "openai")
  .missing <- .pkgs_needed[!vapply(.pkgs_needed, requireNamespace, logical(1), quietly = TRUE)]
  assertthat::assert_that(length(.missing) == 0,
                          msg = paste0("Missing required packages: ", paste(.missing, collapse = ", ")))

  ## ---- Auth setup -----------------------------------------------------------
  Sys.setenv(OPENAI_API_KEY = ai_key)

  ## ---- Read data ------------------------------------------------------------
  # Чтение данных
  fl <- read_csv(file_path)

  # Ensure the 'group' column exists for tbl_summary(by = group)
  assertthat::assert_that("group" %in% names(fl),
                          msg = "Input data must contain a 'group' column for `tbl_summary(by = group)`.")

  ## ---- Build gtsummary table ------------------------------------------------
  # Создание таблицы
  tbl <- fl %>%
    tbl_summary(
      by = group,
      statistic = all_categorical() ~ "{n} ({p}%)"
    ) %>%
    add_p() %>%
    bold_labels()
  tbl

  ## ---- Optional short table selection --------------------------------------
  # making a short table that includes variables for interpretation
  tbl_full <- tbl

  # If a subset is requested, it should be a character vector of names
  if (!is.na(varriabes_for_stats)) {
    assertthat::assert_that(is.character(varriabes_for_stats),
                            msg = "`varriabes_for_stats` must be a character vector of variable names or NA.")
    tbl_short <- tbl_full %>%
      select(all_of(varriabes_for_stats))
  } else {
    tbl_short <- tbl_full
  }
  tbl_short

  ## ---- Convert table to text for LLM ---------------------------------------
  tbl_formatted <- tbl %>%
    as_tibble(col_labels = TRUE) %>%
    as.data.frame()
  tbl_formatted <- paste(capture.output(print(tbl_formatted, row.names = FALSE)), collapse = "\n")

  ## ---- Compose user message -------------------------------------------------
  # make up a message to chatgpt from the user
  user_message = paste0("This is the context output should suit: ",context,
                        "; The vervosity of your answer should be: ", verbosity,
                        "; Here are specal instructions (if any): ", instructions,
                        "; Please make the answer in the following language: ", language)

  ## ---- Call LLM -------------------------------------------------------------
  res <- openai::create_chat_completion(
    model = model,
    messages = list(
      list(role = "system", content = "You are a biomedical statistician"),#gives the model role
      list(role = "user",   content = paste(
        "You write academic conclusions for peer-reviewed biomedical journals. Interpret the gtsummary table (significant differences, clinical meaning):\n\n", tbl_formatted, "\n\n",
        user_message
      ))
    ),
    temperature = formality
  )

  out <- res$choices[["message.content"]][1]

  ## ---- Build Word document --------------------------------------------------
  # Создание Word-документа
  tbl_ft <- as_flex_table(tbl) %>%
    fontsize(size = 10, part = "all") %>%
    autofit()

  doc <- read_docx() %>%
    body_add_par("Statistical Report", style = "heading 1") %>%
    body_add_par(paste("Generated:", Sys.Date()), style = "Normal") %>%
    body_add_flextable(tbl_ft) %>%
    body_add_par("Interpretation:", style = "heading 2") %>%
    body_add_par(out, style = "Normal")

  ## ---- Save document --------------------------------------------------------
  # Сохранение документа
  print(doc, target = output_file)
  message("Document '", output_file, "' successfully created!")

  ## ---- Return ---------------------------------------------------------------
  return(out)
}





#' Interpret a gtsummary table and return an LLM-written narrative
#'
#' Builds a grouped descriptive table from an in-memory data frame or tibble
#' with `gtsummary::tbl_summary()`, optionally limits interpretation to a set
#' of variables, requests a textual interpretation from an LLM, and returns the
#' generated text. This variant accepts a pre-built table input rather than a file.
#'
#' @param table_input A data.frame or tibble containing the data to summarize.
#'   The default illustrates subsetting (`opt[1:10, 1:10]`) and assumes `opt`
#'   exists in the calling environment.
#' @param by Character scalar: name of the grouping column present in `table_input`.
#' @param context Character scalar giving high-level context for the LLM (e.g., "For a research paper").
#' @param language Character scalar specifying the output language (e.g., "en").
#' @param verbosity Character scalar describing how verbose the LLM output should be.
#' @param instructions Character scalar with any special instructions for the LLM.
#' @param varriabes_for_stats Character vector of column names to include in the summary;
#'   if length is 1, the current code interprets this as "use all columns".
#'   (Original parameter spelling preserved.)
#' @param model Character scalar model name passed to `openai::create_chat_completion()`.
#' @param formality Numeric scalar in [0, 1], passed to `temperature`.
#' @param ai_key Character scalar OpenAI API key. If equal to `"Evgeny"` (default),
#'   the function reads the key from a local file `"ai_key.txt"`.
#'
#' @return Character scalar with the generated interpretation text.
#'
#' @details
#' The input data must contain a grouping column named by `by`. The summary is built
#' with `gtsummary::tbl_summary(by = by, include = varriabes_for_stats)`, followed by
#' `add_p()` and `bold_labels()`. The resulting table is converted to text and sent
#' to the LLM for interpretation.
#'
#' @examples
#' \dontrun{
#' interpret_table_v2(
#'   table_input = iris,
#'   by = "Species",
#'   varriabes_for_stats = names(iris),
#'   language = "en",
#'   context = "For a research paper",
#'   ai_key = Sys.getenv("OPENAI_API_KEY")
#' )
#' }
#'
#' @importFrom gtsummary tbl_summary add_p bold_labels
#' @importFrom tibble as_tibble
#' @importFrom openai create_chat_completion
#' @importFrom assertthat assert_that
#' @importFrom utils capture.output
#' @importFrom magrittr %>%
#' @export
interpret_table_v2 <- function(table_input = opt[1:10,1:10], by,
                               context = "For a research paper", language = "en",
                               verbosity = "moderate", instructions = "",
                               varriabes_for_stats = c(),
                               model = "gpt-4o", formality = 0.2, ai_key = "Evgeny") {

  ## ---- Input checks (do not alter existing logic) ---------------------------
  # Basic type/length checks
  assertthat::assert_that(is.character(by), length(by) == 1, nchar(by) > 0,
                          msg = "`by` must be a non-empty character scalar naming a column in `table_input`.")
  assertthat::assert_that(is.character(context), length(context) == 1)
  assertthat::assert_that(is.character(language), length(language) == 1)
  assertthat::assert_that(is.character(verbosity), length(verbosity) == 1)
  assertthat::assert_that(is.character(instructions), length(instructions) == 1)
  assertthat::assert_that(is.character(model), length(model) == 1, nchar(model) > 0)
  assertthat::assert_that(is.numeric(formality), length(formality) == 1,
                          !is.na(formality), formality >= 0, formality <= 1,
                          msg = "`formality` must be numeric in [0, 1].")
  assertthat::assert_that(is.character(ai_key), length(ai_key) == 1)

  ## ---- API key handling -----------------------------------------------------
  if (ai_key == "Evgeny") {
    ai_key <- readLines("ai_key.txt") #this should be in gitignore
  }
  assertthat::assert_that(is.character(ai_key), length(ai_key) >= 1, nchar(ai_key[1]) > 0,
                          msg = "OpenAI API key could not be read or is empty.")
  Sys.setenv(OPENAI_API_KEY = ai_key)

  ## ---- Table type checks (original checks preserved) ------------------------
  #  Check if the table_input is a data frame or tibble
  if (!is.data.frame(table_input) && !is_tibble(table_input)) {
    stop("table_input must be a data frame or tibble.")
  }

  ## ---- Variable selection checks -------------------------------------------
  # Check if the variables for stats are in the table_input
  if (length(varriabes_for_stats) == 1) {
    varriabes_for_stats <- colnames(table_input)
  } else{
    #let us check if any of the variables_for_stats are not in the table colnames
    if (!all(varriabes_for_stats %in% colnames(table_input))) {
      variables_absent <- paste(varriabes_for_stats[!varriabes_for_stats %in% colnames(table_input)])
      warning(paste0("Some of the variables_for_stats are not in the table colnames. Here are the ones missing: ",variables_absent))
      varriabes_for_stats <- varriabes_for_stats[varriabes_for_stats %in% colnames(table_input)]
    }
  }

  ## ---- 'by' column existence check (original preserved) --------------------
  # Check if the 'by' variable is in the table_input
  if (!by %in% colnames(table_input)) {
    stop(paste("The 'by' variable", by, "is not in the table_input."))
  }

  ## ---- Build gtsummary table -----------------------------------------------
  #making tbl__summary table
  tbl <- table_input %>%
    tbl_summary(
      by = by,
      include = varriabes_for_stats
    ) %>%
    add_p() %>%
    bold_labels()
  #print(tbl)

  ## ---- Convert table to text for LLM ---------------------------------------
  tbl_text <- tbl %>%
    as_tibble(col_labels = TRUE) %>%
    as.data.frame()
  tbl_text <- paste(capture.output(print(tbl_text, row.names = FALSE)), collapse = "\n")

  ## ---- Compose the prompt ---------------------------------------------------
  #make up a message to chatgpt from the user
  user_message = paste0("This is the context output should suit: ",context,
                        "; The vervosity of your answer should be: ", verbosity,
                        "; The variables for which I need interpretation are: ", paste(varriabes_for_stats, collapse = ", "),
                        "; Here are specal instructions (if any): ", instructions,
                        "; Please make the answer in the following language: ", language)

  ## ---- Call the LLM ---------------------------------------------------------
  res <- openai::create_chat_completion(
    model = model,
    messages = list(
      list(role = "system", content = "You are a medical statistician"),#gives the model role
      list(role = "user",   content = paste(
        "Please interpret the gtsummary table (significant differences):\n\n", tbl_text,
        "\n\n regarding the follwoing columns: ",
        user_message
      ))
    ),
    temperature = formality
  )

  #out <- res$choices[["message.content"]][1]
  return(res$choices[["message.content"]])
}

