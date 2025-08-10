gts_build <- function(data,
                      by = NULL,              # имя группирующей переменной (строка) или NULL
                      include = NULL,         # вектор имён столбцов для анализа; NULL = все
                      stat_cont = "{mean} ± {sd}",
                      stat_cat  = "{n} ({p}%)",
                      digits_cont = 1,
                      missing_policy = "no",
                      add_p = TRUE,
                      bold  = TRUE) {
  stopifnot(is.data.frame(data))
  df <- data

  # 1) Сузим набор колонок при необходимости
  if (!is.null(include)) {
    miss <- setdiff(include, names(df))
    if (length(miss)) stop("Not in data: ", paste(miss, collapse = ", "))
    df <- df[, include, drop = FALSE]
  }

  # 2) Жёсткая обработка by
  if (!is.null(by)) {
    if (!by %in% names(data)) {
      stop(sprintf("Grouping variable '%s' not found in original data.", by))
    }
    # если include задан и в нём НЕТ by — добавим его и пересузим df
    if (!is.null(include) && !by %in% names(df)) {
      include <- c(by, include)
      df <- data[, include, drop = FALSE]
    }
    # приведение by к factor, если это char/logical
    if ((is.character(df[[by]]) || is.logical(df[[by]])) && !is.factor(df[[by]])) {
      df[[by]] <- as.factor(df[[by]])
    }
  }

  # 3) Форматы статистик
  cont_fmt <- gtsummary::all_continuous()  ~ stat_cont
  cat_fmt  <- gtsummary::all_categorical() ~ stat_cat

  # 4) Сборка таблицы
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

  # 5) Plain-text версия
  tmp <- gtsummary::as_tibble(tbl, col_labels = TRUE)
  tmp <- as.data.frame(tmp)
  tbl_text <- paste(capture.output(print(tmp, row.names = FALSE)), collapse = "\n")

  # 6) Метаданные
  vars_used <- names(df)
  vars_info <- paste0(vars_used, ":", vapply(df, function(x) class(x)[1], ""), collapse = ", ")

  list(
    tbl       = tbl,        # объект gtsummary
    text      = tbl_text,   # plain-text таблицы
    vars_used = vars_used,  # имена задействованных переменных
    vars_info = vars_info,  # имя:класс через запятую
    by        = by          # имя группирующей переменной (или NULL)
  )
}

#example:

#cancer <- readr::read_csv("bladdercancer.csv", show_col_types = FALSE) |>
#  dplyr::mutate(tumorsize = as.factor(tumorsize))

#gt <- gts_build(
#  data    = cancer,
#  by      = "tumorsize",
#  include = c("number", "tumorsize")
#)

#gt$tbl
#cat(gt$text)

#gt$vars_used
#gt$vars_info

#Функция gts_build() будет полностью отвечать за статистическую таблицу и метаданные (имена и типы колонок, имя группирующей переменной).

#принимает data, by (группирующую переменную) и include (какие колонки анализировать);

#строит gtsummary::tbl_summary;

#возвращает саму таблицу tbl,plain-text версию text (для GPT),список задействованных переменных vars_used, строку с краткими описаниями типов vars_info (имя:класс) — удобно вставлять в промпт GPT, имя группирующей переменной by.

#В GPT-чанк берем gt$text, gt$vars_used, gt$vars_info, gt$by — и вставляем в промпт.



#################################################################################
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
#' Comments for the user
#' style:
#' publication = For a research paper
#' clin_report = For a clinical report
#' report = Activity report style
#' popular = For a popular science article



interpret_table <- function(tbl_summary_in,
                               style = c("publication","clin_report","report","popular"),
                               language = "en",sure = T,
                               verbosity = "moderate", instructions = "",
                               varriabes_for_stats = c(),verbose_function_mode = F,
                               model = "gpt-4o", formality = 0.2, ai_key = "Evgeny") {

  ## ---- Input checks (do not alter existing logic) ---------------------------
  # Basic type/length checks
  assertthat::assert_that(is.character(language), length(language) == 1)
  assertthat::assert_that(is.character(verbosity), length(verbosity) == 1)
  assertthat::assert_that(is.character(instructions), length(instructions) == 1)
  assertthat::assert_that(is.character(style), length(model) == 1, nchar(model) > 0)
  assertthat::assert_that(is.numeric(formality), length(formality) == 1,
                          !is.na(formality), formality >= 0, formality <= 2,
                          msg = "`formality` must be numeric in [0, 1].")
  assertthat::assert_that(is.character(ai_key), length(ai_key) == 1)

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



  ## ---- Define the style ------------------------------------------------
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
  ## ---- Convert table to text for LLM ---------------------------------------
  tbl_text <- tbl_summary_in %>%
    as_tibble(col_labels = TRUE) %>%
    as.data.frame()
  tbl_text <- paste(capture.output(print(tbl_text, row.names = FALSE)), collapse = "\n")

  ## ---- Adding sure or unsure mode ------------------------------------------
  if (sure) {
    sure_mode <- "Always say in a self-confident manner what you think about the data. NEVER use words like presumably (IMPORTANT)"
  } else {
    sure_mode <- "Be careful and do not make any assumptions about the data. If you are not sure, say so"
  }
  ## ---- Compose the prompt ---------------------------------------------------
  #make up a message to chatgpt from the user
  user_message = paste0("Output shall not include any preface from you, jusr description of the data. This is the context output should suit: ",context,
                        "; The vervosity of your answer should be: ", verbosity,
                        "; ", sure_mode,
                        "; Here are specal instructions (if any): ", instructions,
                        "; Please make the answer in the following language: ", language)
if (verbose_function_mode) {
  print("now the user message")
  print(user_message)
  print("#table text")
  print(tbl_text)
}


  ## ---- Call the LLM ---------------------------------------------------------
  res <- openai::create_chat_completion(
    model = model,
    messages = list(
      list(role = "system", content = "You are a medical statistician."),#gives the model role
      list(role = "user",   content = paste(
        "Please interpret the gtsummary table (significant differences):\n\n", tbl_text,
        "\n\n regarding the follwoing columns: ",
        user_message
      ))
    ),
    temperature = formality
  )

  #out <- res$choices[["message.content"]][1]
  #return(res)
  return(res$choices[["message.content"]])
}

