# Job app tool: plot some graphs!

library(tidyverse)
library(ggplot2)
library(ggside)
library(scales)

rm(list = ls())

d_gen <- read_csv("generated_data_010722.csv")
d_gen <- d_gen %>% mutate(
  Round = as.factor(Round),
  Gender = as.factor(Gender),
  id = as.factor(id)
)

# ALSO: need to add in that each SCORE is made up of several scores, which we can dive into as well

## --- PROPORTION OF RESPONSES
## This is all incorrect! Round2 separates people out, it doesn't show those who made it or not

d_prop <-d_gen %>% 
  group_by(Round, Gender) %>%
  summarise(
    n = n()
  ) %>% ungroup()  

d_prop_wide <- d_prop %>% 
  pivot_wider(names_from = Gender, values_from = n) %>%
  group_by(Round) %>%
  mutate(Total = sum(c(Female,Male))) %>% 
  ungroup()

d_prop_long <-d_prop_wide %>% 
  pivot_longer(cols = c(Female, Male, Total), names_to = "Gender", values_to = "n")

# Figures
ggplot(data = d_prop_long, aes(x = Round, y = n, group = Gender, colour = Gender)) +
  geom_point(size = 3) +
  geom_line(size = 1, lineend = "round") + 
  labs(
    y = "Number",
    title = "Number of of applicants by gender"
  ) +
  scale_y_continuous(
    limits = c(0, (max(d_prop_long$n)+20)),
    breaks = seq(0, max(d_prop_long$n)+20, 20)) +
  scale_colour_brewer(palette = "Set2") +
  theme_minimal()

# I think have the bar graph underneath, flipped as easy to read % (size of box) and have the total numbers on top
ggplot(data = d_gen, aes(x = Round, fill = Gender)) +
  geom_bar(position = "fill", width = 0.6, alpha = 0.9) +
#  geom_label(aes(x= Round, label=..count..), stat='count') +
  labs(
    y = "%",
    title = "Proportion of of applicants by gender"
    ) +
  scale_y_continuous(breaks = seq(0, 1, .2), label = percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()

# Use "fill" to get each bar representing 100%
# To add % in, see 4.1.4 here: https://rkabacoff.github.io/datavis/Bivariate.html
# See here for labels: https://bookdown.org/aschmi11/RESMHandbook/data-visualization-with-ggplot.html

## --- SCORES

d_scores <- d_gen %>%
  pivot_longer(cols = c(q1:q4), names_to = "Question", values_to = "Scores") %>%
  select(-Round) %>%
  mutate(Question = as.factor(Question))

plot <- ggplot(data = d_scores, aes (x = Question, y = Scores, colour = Gender)) +
  geom_violin(trim = TRUE, position = position_dodge(width = 1)) +
  geom_point(aes(group = Gender), 
             position = position_jitterdodge(dodge.width = 1), alpha = 0.15 ) +
  stat_summary(
    aes(fill = Gender), 
    fun=mean, geom="point", shape=4, size=2, color="black",
    position = position_dodge2(width = 1, preserve = "single")
    ) +
  scale_y_continuous(
    limits = c(
      max( min(d_scores$Scores) - 10, 0),
      min( max(d_scores$Scores) + 10, 100)
      ),
    breaks = seq(0, 100, 10)
  ) + 
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_minimal() +
  stat_summary(fun = mean, geom="point", shape=20, size=3, color="black") +
  stat_summary(fun.data = mean_se, geom="errorbar", width = 0.2, color="black") # can also use mean_cl_boot in place of mean_se

plot + geom_ysidedensity(
  aes(
    x=after_stat(density), 
    fill = Gender
  ), alpha = 0.7) +
  scale_ysidex_continuous(labels = NULL) + 
  theme_ggside_classic() +
  theme(ggside.panel.scale = .2)

##ggside insights: 
# https://github.com/jtlandis/ggside
# https://www.r-bloggers.com/2021/05/visualization-graphs-ggside-with-ggplot/
# https://cran.r-project.org/web/packages/ggside/vignettes/ggside_basic_usage.html

#position_dodge2(width = 0.75, preserve = "single", padding = 0.5)

## >> What about a graph on the side showing overall male and female score distribution / variance differences? (which can thenn be looked at in the data to see better)
ggplot(data = d_scores) +
  geom_density(aes(x = Scores, y = ..density.., fill = Gender), alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  scale_y_continuous(breaks = NULL) +
  labs(x = NULL, y = NULL) +
#  labs(x = NULL, y = NULL, fill = element_blank(), color = element_blank()) +
  theme_minimal() +
  theme(legend.position = "None") 
 



# ggplot2 specs: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# adding straight lines: http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
# and https://www.geeksforgeeks.org/add-vertical-and-horizontal-lines-to-ggplot2-plot-in-r/
  
# Here for box and violin plots (especially adding means and SD): http://www.sthda.com/english/articles/32-r-graphics-essentials/132-plot-grouped-data-box-plot-bar-plot-and-more/

## --- LOOKING FOR OUTLIERS AND ISSUES
# Those who might have got a particular question wrong but did well otherwise?
# Those who might have only answered fewer than max questions





## --- WEIGHTING SCORES FOR PROGRESSION

wgt_equal <- 0.25
wgt_heavy <- 0.4
wgt_light <- 0.2
# Surely can just do a 'weights' list and then matrix multiply each row

d_weight <- d_gen %>% 
  group_by(id) %>%
  mutate(
    mean_equal = (q1 + q2 + q3 + q4)/4, 
    mean_w_q1 = sum(q1*wgt_heavy + q2*wgt_light + q3*wgt_light + q4*wgt_light), 
    mean_w_q2 = sum(q1*wgt_light + q2*wgt_heavy + q3*wgt_light + q4*wgt_light), 
    mean_w_q3 = sum(q1*wgt_light + q2*wgt_light + q3*wgt_heavy + q4*wgt_light),
    mean_w_q4 = sum(q1*wgt_light + q2*wgt_light + q3*wgt_light + q4*wgt_heavy)
  ) %>%
  ungroup() %>%
  select(id, Gender, mean_equal, starts_with("mean_w_"))

# 2 ways: get rank according to a column

# First way: get rank and then next use table that inputs only the top rank for each
d_weight <- d_weight %>% 
  mutate(
    rank_e = rank(-mean_equal),
    rank_q1 = rank(-mean_w_q1),
    rank_q2 = rank(-mean_w_q2),
    rank_q3 = rank(-mean_w_q3),
    rank_q4 = rank(-mean_w_q4)
  )

# Input: just need the input for 'mean_equal'

input_text <- c('mean_equal')

d_equal <- d_weight %>% select(id, Gender, all_of(input_text)) %>% # can also use {{input_text}} in 'select' fn
  mutate(
    rank_id = rank(-get(input_text))   # Note that {{}} only works in select; otherwise we have to use get() or convert to symbol, as below
#    rank_id  := rank(- !! rlang::sym(input_text)) # uses !! to 'unquote' arguments
    ) %>% 
  arrange(rank_id) %>%
  mutate( # Add summary stats of genderedness
    pct_female_top10 = mean(Gender[rank_id <=10] == "Female"),
    pct_female_top20 = mean(Gender[rank_id <=20] == "Female"),
    pct_female_top30 = mean(Gender[rank_id <=30] == "Female"),
    pct_female_top40 = mean(Gender[rank_id <=40] == "Female")
    # Can also try something along these lines: mutate(v3 = if_else(dyad == "Inf",v1 + v2,0))
    ) %>%
  filter(rank_id <= 10)   # Get Top 10

# > Then, insert table here that has the top rank (ID, Gender, Score, Rank) and below the pct_female in boxes


# Doing the above in a loop


input_options <- c("mean_equal", "mean_w_q1")

d_final <- data.frame(matrix(ncol = 2, nrow = 0))
for (i in input_options) {
  print(i)
  d_test <- d_weight %>%
    mutate(
      option_in_question = i,
      rank_id = rank(-get(i))
    ) %>% 
    arrange(rank_id) %>%
    filter(rank_id <= 3)   # Get Top 10
  print(d_test)
  d_final <- rbind(d_final, d_test)
  print(d_final)
}
# Different with shiny: make the above a function, not a for loop, and then change the input each time depending on the opion chosen


# For passing a string as column name in dplyr: 
# See here under 'select': https://www.vishalkatti.com/posts/2021-07-17-programmingwithdplyr/
# https://community.rstudio.com/t/trouble-passing-string-to-represent-variable-name-in-function/84872
# https://stackoverflow.com/questions/26003574/use-dynamic-name-for-new-column-variable-in-dplyr
# Can also use "all_of" function, when names of variables are characters in a vector: https://tidyselect.r-lib.org/reference/all_of.html
# And tidy_evaluation + use of rlang here: https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/

# Second way: sort and then take head(10)
d_2_equal <- d_weight %>% arrange(desc(mean_equal)) %>% head(20)


# # Making a function would be easier / more efficient; or could use purrr:map
# weights_list <- d_weight %>% select(-id, -Gender) %>% colnames()
# weights_list
# for (i in weights_list) {
#   print(i)
#   head(arrange(d_weight, (desc[[i]])),2)
#   }

w_list_e <- d_weight %>% 
  select(id,)
  



  

  
  
  
  
  
  