#' ---
#' output: reprex::reprex_document
#' ---

# install.packages("reprex")
# library(reprex)

install.packages("styler")

first_column <- c("B", "A", "B")
second_column <- c("A", "B", "B")

df <- data.frame(Ferr = first_column, VDM = second_column)

df <- df %>%
  mutate(Ferr_bos = ifelse(str_detect(Ferr, "B"), 1, 0)
         , VDM_bos = ifelse(str_detect(VDM, "B"), 1, 0)
  ) %>%
  mutate(Boshis_sam = paste0(Ferr_bos, VDM_bos))

write_csv2(df, here::here("test.csv"), quote = "all")

reprex(input = here::here("Scripts/test.R"))
