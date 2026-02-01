library(dplyr)
library(openxlsx)

fp_final %>%
  select(pos, category) %>%
  distinct() %>%
  arrange(pos, category) %>%  # Alfabetik sıraya düzülür
  write.xlsx("fp_final_pos_category_unique_sorted.xlsx")

cografim_sayi <- fp_final %>%
      count(cografim, sort = TRUE)

catg_say <- fp_final %>%
  count(category, sort = TRUE)


fp_final <- fp_final %>%
  mutate(
    pos = toupper(pos),
    category = toupper(category),
    cografim = toupper(cografim))

library(dplyr)
library(stringr)

fp_final <- fp_final %>%
  mutate(
    product_name = case_when(
      product_name == "SAPUN (ED)" ~ "SAMPUN (ED)",
      product_name == "X ST D N AVANS" ~ "XESTEDEN AVANS",
      TRUE ~ product_name
    )
  )


