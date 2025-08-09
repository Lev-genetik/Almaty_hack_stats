library(gtsummary)
library(flextable)
library(officer)
library(dplyr)
library(readr)
library(openai)

interpret_table <- function(file_path,
                            output_file,
                            context = "For a research paper", language = "en",
                            verbosity = "moderate", instructions = "",
                            varriabes_for_stats = NA,
                            model = "gpt-4o", formality = 0.2, ai_key) {
  
  Sys.setenv(OPENAI_API_KEY = ai_key)  
  
  # Чтение данных
  cardio <- read_csv(file_path)
  
  # Проверка наличия колонки 'group'
  if(!"group" %in% names(cardio)) {
    stop("В данных отсутствует колонка 'group', необходимая для группировки")
  }
  
  # Создание таблицы
  tbl <- cardio %>%
    tbl_summary(
      by = group,                       
      statistic = all_categorical() ~ "{n} ({p}%)"
    ) %>%
    add_p() %>%
    bold_labels()
  tbl
  
  #making a short table that includes variables for interpretation
  tbl_full <- tbl
  if (!is.na(varriabes_for_stats)) {
    tbl_short <- tbl_full %>%
      select(all_of(varriabes_for_stats))
  } else {
    tbl_short <- tbl_full
  }
  tbl_short
  
  tbl_formatted <- tbl %>%
    as_tibble(col_labels = TRUE) %>%
    as.data.frame()
  tbl_formatted <- paste(capture.output(print(tbl_formatted, row.names = FALSE)), collapse = "\n")
  
  #make up a message to chatgpt from the user
  user_message = paste0("This is the context output should suit: ",context,
                        "; The vervosity of your answer should be: ", verbosity,
                        "; Here are specal instructions (if any): ", instructions,
                        "; Please make the answer in the following language: ", language)
  
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
  
  # Сохранение документа
  print(doc, target = output_file)
  message("Document '", output_file, "' successfully created!")
  
  return(out)
}

# Пример использования
interpret_table(
  file_path = "cpr_survival_tbl_df.csv",
  output_file = "analysis3_report.docx",
  context = "данные, содержащий информацию из исследования, изучающего влияние препаратов, разжижающих кровь, на показатели выживаемости пациентов с искусственным дыханием. В ходе исследования 90 пациентов были случайным образом распределены либо на прием препаратов, разжижающих кровь (группа лечения), либо на отказ от их приема (контрольная группа), результатом чего была выживаемость не менее 24 часов.",
  ai_key = "..."
)
