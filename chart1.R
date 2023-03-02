# Load libraries
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

spl_ten_df <- read.csv("C:\\Users\\chenk\\Desktop\\INFO 201\\2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

audiobooks_df <- spl_ten_df %>%
  filter(str_detect(MaterialType,"AUDIOBOOK"))

#How has the number of Jane Austen's audiobooks checkouts changed over time?
author_df <- audiobooks_df %>%
  filter(str_detect(Creator,"Jane")) %>%
  filter(str_detect(Creator,"Austen")) 

checkouts_per_month <- author_df %>% 
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  complete(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")) %>%
  fill(total_checkouts)

ggplot(checkouts_per_month, aes(x = date, y = total_checkouts)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Jane Austen Checkouts 2022",
       x = "Month",
       y ="Total Checkouts")