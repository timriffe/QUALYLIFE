# this script is for a different but related paper. The file
# Employment_trajectories2004-2020.csv was prepared by Maria Andree
library(tidyverse)
library(janitor)
library(vroom)
library(collapse)
library(tidyfast)
A <- vroom("Data/Employment_trajectories2004-2020_v2.csv")
A <-
  A |> 
  clean_names() |> 
  fsubset(state_from != "Died")
A

# A |> 
#   select(-id) |> 
#   # group_by(year, `_sex`, ocup_isco_H, ocup_isco_R, age_yr, state_from, cat_from,   state_to, cat_to) |> 
#   distinct()

# N <- nrow(A)
# A$ocup_isco_R |> unique()
# A$state_from |> unique()
# A$cat_from |> unique()
# A$cat_to |> unique() # missings all in 2020
# is.na(A$state_to)
# is.na(A$cat_from) |> sum() / N
# is.na(A$cat_to) |> sum() / N
# 
# is.na(A$ocup_isco_R) |> sum()
# 
# A |> filter(year <2020) |> 
#   pull(state_to) |> unique()


# empirical transition probabilities

# vectors for complete()
all_years       <- A |> pull(year) |> unique() |> sort()
all_ages        <- A |> pull(age_yr) |> unique() |> sort()
all_sexes       <- A |> pull(sex) |> unique() |> sort()
all_ocup_isco_h <- A |> pull(ocup_isco_h) |> unique() |> sort()
all_state_from  <- A |> pull(state_from) |> unique() |> sort()
all_state_to    <- A |> pull(state_to) |> unique() |> sort()



# B |> 
#   filter(is.nan(p))
# all_ocup_isco_h


# B |> colnames()
# A |> colnames()
# A |> pull(year) |> range()





all_years - all_years %% 2
all_year2 <- seq(2004,2020,by=2)
all_ages <- 25:75


D <-
  A  |> 
  fmutate(year2 = year - year %% 2)  |> 
  fsubset(!is.na(state_to)) |> 
  count(year2, 
    sex, age_yr, ocup_isco_h, state_from, state_to,
    name = "transitions")  |> 
  complete(year2 = all_year2,
    age_yr = all_ages,
    sex = all_sexes,
    ocup_isco_h = all_ocup_isco_h,
    state_from = all_state_to,
    state_to = all_state_to,
    fill = list(transitions = 0)) |>  
  add_count(year2, 
    sex, age_yr, ocup_isco_h, state_from,
    name = "denom",
    wt = transitions)  |> 
  fmutate(p = transitions / denom,
         p = if_else(transitions == 0, 0, p),
         p = if_else(state_from == "Died" & state_to == "Died", 1, p)) |> 
  fsubset(between(age_yr, 25, 75)) 
D
D |> pull(p) |> '>'(1) |> any()
D |> 
  group_by(year2,age_yr,sex,ocup_isco_h, state_from) |> 
  fsummarise(p = sum(p),keep.group_vars = FALSE) |> 
  group_by(p) |> 
  fsummarise(n=n())

D |> 
  fsubset(state_from == "Disability" &
         state_to == "Died" &
         year2 == 2004) |> 
  ggplot(aes(x = age_yr, y = p, color = sex, alpha = year2, group = interaction(sex,year2))) +
  geom_line() +
  facet_wrap(~ocup_isco_h)


# 
# optimize(fr, c(-10,10))
