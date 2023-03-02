# Load libraries
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

spl_ten_df <- read.csv("C:\\Users\\chenk\\Desktop\\INFO 201\\2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

#Filtering

ebooks_df <- spl_ten_df %>%
  filter(str_detect(MaterialType,"EBOOK"))

audiobooks_df <- spl_ten_df %>%
  filter(str_detect(MaterialType,"AUDIOBOOK"))

#Author filter

riordan_ebooks_df <- ebooks_df %>%
  filter(str_detect(Creator,"Rick")) %>%
  filter(str_detect(Creator,"Riordan"))

riordan_audiobooks_df <- audiobooks_df %>%
  filter(str_detect(Creator,"Rick")) %>%
  filter(str_detect(Creator,"Riordan"))

#checkouts

riordan_ebook_checkouts_per_month <- riordan_ebooks_df %>% 
  mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01"))) %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  complete(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")) %>%
  fill(total_checkouts)

riordan_audiobook_checkouts_per_month <- riordan_audiobooks_df %>% 
  mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01"))) %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  complete(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")) %>%
  fill(total_checkouts)

#graph

ggplot() +
  geom_point(data = riordan_ebook_checkouts_per_month, aes(x = date, y = total_checkouts, color = "EBOOK")) +
  geom_point(data = riordan_audiobook_checkouts_per_month, aes(x = date, y = total_checkouts, color = "AUDIOBOOK")) +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(title = "Rick Riordan Checkouts By Material",
       x = "Month",
       y ="Total Checkouts",
       colors = "MaterialType",
       legend.title = "Material") +
  scale_color_manual(values = c("EBOOK" = "darkseagreen4", "AUDIOBOOK" = "darkblue"))
