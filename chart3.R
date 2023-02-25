#Load Library
library("tidyverse")
library("dplyr")
library("ggplot2")
library("RColorBrewer")

# Load dataset
data <- na.omit(read.csv("2017-2023-10-Checkouts-SPL-Data.csv"))
                
audiobook <- data %>%
  filter(MaterialType %in% c("AUDIOBOOK"))
audiobook_co <- audiobook %>% 
  group_by(CheckoutYear) %>%
  summarize(audiobook_checkouts = sum(Checkouts))

ebook <- data %>%
  filter(MaterialType %in% c("EBOOK"))
ebook_co <- ebook %>% 
  group_by(CheckoutYear) %>%
  summarize(ebook_checkouts = sum(Checkouts))

# Create Plot
options(scipen = 999)
ggplot() + 
  geom_line(data = audiobook_co, aes(x = CheckoutYear, y = audiobook_checkouts, color='AudioBook')) + 
  geom_line(data = ebook_co, aes(x = CheckoutYear, y = ebook_checkouts, color='EBook')) +
  labs(title="Audiobook vs. Ebook Checkouts from 2017-2023", x ="Year",
  y ="Number of Checkouts") +
  scale_color_manual(name='Type of Checkout',
                     breaks=c('AudioBook', 'EBook'),
                     values=c('AudioBook'='#6279B8', 'EBook'='#90C290')) +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) 
