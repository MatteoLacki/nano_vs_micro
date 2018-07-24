library(readr)
library(tidyverse)
library(ggthemes)

all_data = read_delim("data/all_signals.csv", ";", escape_double = FALSE, trim_ws = TRUE)
all_data[all_data == 'NULL'] = NA
D = all_data %>% filter(!is.na(sequence))
D %>% summary

D %>% filter(run == 1) %>% qplot(rt, dt, data=., geom='density2d') + facet_grid(.~charge)


D = read_delim("~/Projects/Nano_VS_Micro/data/all_signals.csv", ";", escape_double = FALSE, trim_ws = TRUE)
D = preprocess_peptides(D)
save(D, file='~/Projects/Nano_VS_Micro/data/preprocessed_data.Rd')
