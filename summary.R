# Load libraries
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

spl_ten_df <- read.csv("C:\\Users\\chenk\\Desktop\\INFO 201\\2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

audiobooks_df <- spl_ten_df %>%
  filter(str_detect(MaterialType,"AUDIOBOOK"))

checkouts_by_year <- audiobooks_df %>%
  group_by(CheckoutYear) %>%
  summarise(checkouts_total = sum(Checkouts))

num_rows <- nrow(spl_ten_df)

num_col <- ncol(spl_ten_df)

#What is the average number of checkouts for audio books?
avg_checkouts <- mean(audiobooks_df$Checkouts)

cat("The average number of checkouts for audio books is:", avg_checkouts)

#What is the highest amount of checkouts for audio books?
max_checkouts <- max(audiobooks_df$Checkouts)

cat("The greatest number of checkouts for audio books is:", max_checkouts)

#What is the year with the most audio books checkouts?

most_checkout_year <- checkouts_by_year %>%
  filter(checkouts_total == max(checkouts_total)) %>%
  select(CheckoutYear)

cat("The year with the most audio book checkouts is:", most_checkout_year$CheckoutYear)

#What is the year with the least audio books checkouts?

least_checkout_year <- checkouts_by_year %>%
  filter(checkouts_total == min(checkouts_total)) %>%
  select(CheckoutYear)

cat("The year with the least audio book checkouts is:", least_checkout_year$CheckoutYear)

#What is the year with the least audio books checkouts besides 2023?

least_checkout_year <- checkouts_by_year %>%
  filter(CheckoutYear !=2023) %>%
  filter(checkouts_total == min(checkouts_total)) %>%
  select(CheckoutYear)

cat("The year with the least audio book checkouts besides 2023 is:", least_checkout_year$CheckoutYear)

#How has the number of audio books checkouts changed over time?
audiobooks_df <- audiobooks_df %>% 
  mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01")))

checkouts_by_date <- audiobooks_df %>%
  group_by(date) %>%
  summarise(total_checkouts = sum(Checkouts))

ggplot(checkouts_by_date, aes(x = date, y = total_checkouts)) +
  geom_line() +
  labs(title = "Audiobook Checkouts Over Time",
       x = "Date",
       y = "Total Checkouts")
