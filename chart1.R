#Load Library
library("tidyverse")
library("dplyr")
library("ggplot2")
library("RColorBrewer")

# Load dataset
hg_data <- na.omit(read.csv("Checkouts_by_Hunger_Games.csv"))
hg_summarized <- hg_data %>%
  group_by(CheckoutYear) %>% 
  summarize(totalCheckouts = sum(Checkouts))

hp_data <- na.omit(read.csv("Checkouts_by_Harry_Potter.csv"))
hp_summarized <- hp_data %>%
  group_by(CheckoutYear) %>% 
  summarize(totalCheckouts = sum(Checkouts))

# Create Plot
options(scipen = 999)
ggplot() + 
  geom_line(data = hg_summarized, aes(x = CheckoutYear, y = totalCheckouts, color='Hunger Games')) + 
  geom_line(data = hp_summarized, aes(x = CheckoutYear, y = totalCheckouts, color='Harry Potter')) +
  labs(title="Harry Potter vs. Hunger Games Checkouts from 2008-2023", x ="Year",
       y ="Number of Checkouts") +
  scale_color_manual(name='Book',
                     breaks=c('Hunger Games', 'Harry Potter'),
                     values=c('Hunger Games'='#D65780', 'Harry Potter'='#FFBD00')) +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_x_continuous(breaks = seq(2008, 2023, 1), limits = c(2008, 2023))

  
  

