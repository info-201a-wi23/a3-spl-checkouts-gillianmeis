#Load Library
library("tidyverse")
library("dplyr")
library("ggplot2")
library("RColorBrewer")

# Load dataset
hp_data <- na.omit(read.csv("Checkouts_by_Harry_Potter.csv"))
hp_summarized <- hp_data %>%
  group_by(CheckoutYear) %>% 
  summarize(totalCheckouts = sum(Checkouts))

# Create Plot
hp_data %>%
  ggplot(aes(x = CheckoutYear, fill= MaterialType)) +
  geom_histogram(position="stack", binwidth = 0.5) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Checkouts for Harry Potter Books from 2005-2023",
       x = "Year",
       y = "Number of Checkouts") +
  scale_x_continuous(breaks = seq(2005, 2023, 1)) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
  guides(fill=guide_legend(title="Material Type"))

