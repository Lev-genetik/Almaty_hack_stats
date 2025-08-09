library(gtsummary)
library(dplyr)
library(readr)
cardio <- read_csv("cpr_survival_tbl_df.csv")
head(cardio)
str(cardio)
tbl <- cardio %>%
  tbl_summary(
    by = group,                       
    statistic = all_categorical() ~ "{n} ({p}%)"
  ) %>%
  add_p() %>%
  bold_labels()
tbl

tbl_cardio <- tbl %>%
  as_tibble(col_labels = TRUE) %>%
  as.data.frame()
tbl_cardio <- paste(capture.output(print(tbl_cardio, row.names = FALSE)), collapse = "\n")

cat(tbl_cardio)

library(openai)
Sys.setenv(OPENAI_API_KEY = "sk-...") 

res <- openai::create_chat_completion(
  model = "gpt-4o",
  messages = list(
    list(role = "system", content = "Ты медицинский статистик."),
    list(role = "user",   content = paste(
      "Интерпретируй таблицу gtsummary (значимые различия, клинический смысл):\n\n", tbl_cardio
    ))
  ),
  temperature = 0.2
)

out <- res$choices[["message.content"]][1]
cat(out, sep = "\n")

####
neuro <- read_csv("epilepsy_RCT_tbl_df.csv")
head(neuro)

neuro <- neuro %>% mutate(treat = factor(treat)) 

tbl_neuro <- neuro %>%
  select(treat, base, y1, y2, y3, y4, age) %>%
  tbl_summary(
    by = treat,
    statistic = all_continuous() ~ "{mean} ± {sd}",
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels()
tbl_neuro

neuro <- neuro %>% mutate(total = y1 + y2 + y3 + y4)

fit_lm <- lm(total ~ treat + base, data = neuro)

tbl_lm <- tbl_regression(
  fit_lm,
  intercept = FALSE
) %>%
  add_glance_table() %>%
  bold_labels()

tbl_lm

to_plain_text <- function(tbl) {
  x <- tbl %>% as_tibble(col_labels = TRUE) %>% as.data.frame()
  paste(capture.output(print(x, row.names = FALSE)), collapse = "\n")
}

txt_neuro <- to_plain_text(tbl_neuro)
txt_lm   <- to_plain_text(tbl_lm)

cat(txt_neuro)
cat(txt_lm)

library(openai)
Sys.setenv(OPENAI_API_KEY = "sk-...") 

res <- openai::create_chat_completion(
  model = "gpt-4o",
  messages = list(
    list(role = "system", content = "Ты медицинский статистик."),
    list(role = "user",   content = paste(
      "Интерпретируй таблицу gtsummary (значимые различия, клинический смысл):\n\n", txt_lm
    ))
  ),
  temperature = 0.2
)


out <- res$choices[["message.content"]][1]
cat(out, sep = "\n")
