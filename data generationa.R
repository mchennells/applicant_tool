# Generating data for use in the Applied tool

library(dplyr)
library(simstudy)
library(ggplot2)
library(readr)

rm(list=ls())

# See here for simstudy beginning: 
# https://kgoldfeld.github.io/simstudy/
# https://www.r-bloggers.com/2017/05/it-can-be-easy-to-explore-data-generating-mechanisms-with-the-simstudy-package/

# Females scores first
def_f <- defData(varname = "q1", formula = 70, variance = 12, dist = "normal" ) 
def_f <- defData(def_f, varname = "q2", formula = 77, variance = 5, dist = "normal" ) 
def_f <- defData(def_f, varname = "q3", formula = 58, variance = 25, dist = "normal" ) 
def_f <- defData(def_f, varname = "q4", formula = 62, variance = 8, dist = "normal" ) 

# Males scores
def_m <- defData(varname = "q1", formula = 50, variance = 12, dist = "normal" ) 
# Gender: difference in scores, no difference in variance
# Total: average scores, medium score variance

def_m <- defData(def_m, varname = "q2", formula = 82, variance = 5, dist = "normal" ) 
# Gender: no difference in scores, no difference in variance
# Total: high scores, low score variance

def_m <- defData(def_m, varname = "q3", formula = 62, variance = 25, dist = "normal" ) 
# Gender: no difference in scores, no difference in variance
# Total: average scores, high variance

def_m <- defData(def_m, varname = "q4", formula = 58, variance = 40, dist = "normal" ) 
# Gender: no difference in scores, large difference in variance
# Total: average scores, medium score variance

# Assume: 200 applicants, 4 questions, male and female factors, scores normalised out of 100

# Generate datasets

## Females: 60 applicants
d_f <- genData(60, def_f, id = "id")
d_f <- d_f %>% mutate(Gender = "Female") # create indicator for female

d_f_sample <- d_f %>% slice_sample(n = length(d_f$id) * 0.6)  # 70% of female applicants progress
d_f <- d_f %>% mutate(Round = ifelse(id %in% d_f_sample$id, "Round 2", "Round 1"))
d_f %>% count(Round)

## Males: 140 applicants
d_m <- genData(140, def_m)
d_m <- d_m %>% mutate(id = id+100)
d_m <- d_m %>% mutate(Gender = "Male") # create indicator for male

d_m_sample <- d_m %>% slice_sample(n = length(d_m$id) * 0.3)  # 40% of male applicants progress
d_m <- d_m %>% mutate(Round = ifelse(id %in% d_m_sample$id, "Round 2", "Round 1"))
d_m %>% count(Round)

# Join datasets together
d_full <- rbind(d_f, d_m)
d_full %>% count(Gender)
d_full %>% count(Round)
table(d_full %>% select(Gender, Round))
#xtabs(data=d_full, ~Gender + Round2)
#CrossTable(d_full$Gender, d_full$Round2)

class(d_full)
write_csv(d_full, "generated_data_010722.csv")

