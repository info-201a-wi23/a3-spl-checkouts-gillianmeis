#Load Library
library("tidyverse")
library("dplyr")
library("ggplot2")
library("RColorBrewer")

# Load dataset
data <- na.omit(read.csv("2017-2023-10-Checkouts-SPL-Data.csv"))

#   What year were there the most number of overall checkouts?
year <- data %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

# At what date were there the highest number of checkouts for books with "Hunger Games" in the title?
hg_data <- data %>%
  filter(grepl('Hunger Games', Title)) %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))

hg_most <- hg_data %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) 

highest_co_date_hg <- hg_most %>%
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>%
  pull(date)
  
# What is the date with the most checkouts for books with the words "Harry Potter" included in the title?
hp_data <- data %>%
  filter(grepl('Harry Potter', Title)) %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))

hp_most <- hp_data %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) 

highest_co_date_hp <- hp_most %>%
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>%
  pull(date)

# What is the Publisher Company with the most checkouts in 2022?
highest_publisher <- data %>% 
  filter(CheckoutYear %in% c('2020')) %>%
  group_by(Publisher) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  filter(total_checkouts == max(total_checkouts)) %>%
  pull(Publisher)

# What is the year with the highest E-Book to Book ratio in checkouts?
ebook_per_year <- data %>%
  filter(MaterialType %in% c('EBOOK')) %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

book_per_year <- data %>%
  filter(MaterialType %in% c('BOOK')) %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

ebook_to_book <- merge(x=book_per_year, y=ebook_per_year, by="CheckoutYear")

highest_ratio <- ebook_to_book %>% 
  mutate(ratio = round(total_checkouts.y / total_checkouts.x, 2)) %>%
  filter (ratio == max(ratio)) %>%
  pull(CheckoutYear)
  
  
