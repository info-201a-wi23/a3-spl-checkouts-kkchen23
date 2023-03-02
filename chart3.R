# Load libraries
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

spl_ten_df <- read.csv("C:\\Users\\chenk\\Desktop\\INFO 201\\2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

audiobooks_df <- spl_ten_df %>%
  filter(str_detect(MaterialType,"AUDIOBOOK"))

#How many checkouts does each of his Percy Jackson audio books have?
author_df <- audiobooks_df %>%
  filter(str_detect(Creator,"Rick")) %>%
  filter(str_detect(Creator,"Riordan"))

pj_books <- author_df %>%
  filter(str_detect(Title, "Percy Jackson and the Olympians")) %>%
  group_by(Title) %>%
  summarize(total_checkouts = sum(Checkouts))

pj_books$Title <- gsub("\\(.*\\)", "", pj_books$Title)
pj_books$Title <- gsub("[[:punct:]]", "", pj_books$Title)
pj_books$Title <- gsub("Series", "", pj_books$Title)
pj_books$Title <- gsub("\\s+", " ", pj_books$Title)

pj_books <- pj_books %>%
  group_by(Title) %>%
  summarize(total_checkouts = sum(total_checkouts))

pj_books <- filter(pj_books, Title != "The Demigod Files Percy Jackson and the Olympians Book 45 ")
pj_books$Title <- gsub("Percy Jackson and the Olympians", " ", pj_books$Title)
pj_books$Title <- gsub("Book", "", pj_books$Title)
pj_books$Title <- gsub("[0-5]", "", pj_books$Title)

pj_books$Title <- factor(pj_books$Title, levels = pj_books$Title[order(pj_books$total_checkouts, decreasing = TRUE)])


ggplot(pj_books, aes(x = Title, y = total_checkouts)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  xlab("Book Title") +
  ylab("Total Checkouts") +
  ggtitle("Percy Jackson Audiobook Checkouts by Title") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

