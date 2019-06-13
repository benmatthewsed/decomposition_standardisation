library(tidyverse)
library(pdftools)
library(here)

dg_pdf <- pdf_text(here("documentation", "gas_gupta-1993_standardization_decomposition.pdf"))

# finding a relevant page (using value from the table)
str_detect(dg_pdf, "3.005")


# filtering only this page from the document

matches_search <- as.tibble(dg_pdf) %>% 
  filter(str_detect(value, "2.877"))

table_page <- 
matches_search %>% 
  filter(str_detect(value, "Chapter 6"))
  

table_page[[1]]

tmp <- str_split(table_page, pattern = "\\r\\n")

# look at this tmp object to see where the table is
# and then filter by the index number

tmp2 <- 
as.data.frame(tmp) %>% 
  mutate(index = row_number()) %>% 
  filter(index >= 41 & index <= 48) %>% 
  as.tibble()

# it seems to be missing off the first columns for some reason...?
# because they're not OCR text - it's an old document!

dg_egs <- 
tmp2 %>% 
  rename(words = 1) %>% 
  separate(words, 
           into = c(as.character(seq(1, 11))),
           sep = "       ",
           extra = "merge", fill = "left") %>%
  select(-1:-3, -8, -9) %>% # then remove whitespace
  mutate_if(is.character, str_trim) %>% 
  # remove OCR errors
  mutate_if(is.character, str_replace_all, pattern = " ", replacement = "") %>% 
  mutate_if(is.character, str_replace_all, pattern = "O", replacement = "0") %>% 
  mutate_if(is.character, str_replace_all, pattern = "I", replacement = "1") %>% 
  mutate_if(is.character, as.numeric) %>% 
  # clean up the names
  rename(college_pop_12 = 1,
         diff_hs_col_pops_12 = 2,
         not_hs_pop31 = 3,
         college_pop_13 = 4,
         not_hs_pop32 = 5,
         hs_pop_32 = 6) %>% 
  select(-index)

extra_cols <- 
tribble(~hs_pop_21, ~diff_not_hs_col_pop_31, ~diff_not_hs_hs_pop_32,
        2.871,      .035,                   .008,
        2.846,      .042,                   .065,
        2.868,      .074,                   .037,
        2.887,      .036,                   .014,
        2.925,      .183,                   .055,
        2.877,      .013,                   .038,
        2.948,      .198,                   .206,
        3.005,      .581,                   .423)

dg_egs <- bind_cols(dg_egs, extra_cols)
