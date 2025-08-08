install.packages("gtsummary")
library(gtsummary)
library(dplyr)
data("trial")
head(trial)

#cheat_sheet
trial %>%
  select(trt, age, grade, response)%>%
  tbl_summary(
    by = trt,
    label = list(age ~ "Age (years)”,
 grade ~ “Tumor grade"),
    percent = "row",
    digits = list(age ~ 2),
    statistic = list(age ~ "{mean} ({sd})",
                     response ~ "{n}/{N} ({p}%)"),
    type = list(response ~ "categorical"),
    missing = "always",
    missing_text= "Missing",
  )

trial %>%
  select(trt, age, 
         response)%>%
  tbl_summary(
    by = trt, 
    missing= "no"
)%>%
 add_n() %>%
 add_overall() %>%
 add_p()

#2 вар
tbl <- trial %>%
  select(age, grade, response, trt) %>%  # выбираем интересные переменные
  tbl_summary(
    by = trt,        # группировка по лечению
    missing = "no"   # не показывать пропуски
  ) %>%
  add_p() %>%        # добавляем p-value
  modify_header(label = "**Characteristic**") %>%
  bold_labels()
tbl

#sheet_text
text_trial <- as_tibble(trial, col_labels = FALSE)
cat(capture.output(print(text_tbl)), sep = "\n")

#2 вар_text
text_tbl <- as_tibble(tbl, col_labels = FALSE)
cat(capture.output(print(text_tbl)), sep = "\n")

#однофакторная логистическая регрессия
trial %>%
  select(response, age, marker) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial)
  )
