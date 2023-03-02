# Load libraries
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

spl_ten_df <- read.csv("C:\\Users\\chenk\\Desktop\\INFO 201\\2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

audiobooks_df <- spl_ten_df %>%
  filter(str_detect(MaterialType,"AUDIOBOOK"))

#Mythology genre
mythology_audiobooks_df <- audiobooks_df %>% filter(str_detect(Subjects, "Mythology")) 

mythology_audiobooks_df  <- mythology_audiobooks_df %>% mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01")))

mythology_checkouts_per_month <- mythology_audiobooks_df  %>% 
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  complete(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")) %>%
  fill(total_checkouts)

#Fantasy genre
fantasy_audiobooks_df <- audiobooks_df %>% filter(str_detect(Subjects, "Fantasy")) 

fantasy_audiobooks_df  <- fantasy_audiobooks_df %>% mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01")))

fantasy_checkouts_per_month <- fantasy_audiobooks_df  %>% 
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  complete(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")) %>%
  fill(total_checkouts)

#Science Fiction genre
scifi_audiobooks_df <- audiobooks_df %>% filter(str_detect(Subjects, "Science Fiction")) 

scifi_audiobooks_df  <- scifi_audiobooks_df %>% mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01")))

scifi_checkouts_per_month <- scifi_audiobooks_df  %>% 
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  complete(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")) %>%
  fill(total_checkouts)

#plot
ggplot() +
  geom_line(data = mythology_checkouts_per_month, aes(x = date, y = total_checkouts, color = "Mythology")) +
  geom_line(data = fantasy_checkouts_per_month, aes(x = date, y = total_checkouts, color = "Fantasy")) +
  geom_line(data = scifi_checkouts_per_month, aes(x = date, y = total_checkouts, color = "Science Fiction")) +
  scale_y_continuous(limits = c(0, 10000)) +
  labs(title = "Audio Books Checkouts Over Time by Genre",
       x = "Time",
       y ="Total Checkouts",
       colors = "Subject",
       legend.title = "Genre") +
  scale_color_manual(values = c("Mythology" = "deeppink", "Fantasy" = "darkseagreen4", "Science Fiction" = "darkblue"))
