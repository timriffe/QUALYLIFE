# this script is for a different but related paper. The file
# Employment_trajectories2004-2020.csv was prepared by Maria Andree
library(readr)
library(tidyverse)
library(janitor)
A <- read_csv("Data/Employment_trajectories2004-2020_v2.csv")
A <-
  A %>% 
  clean_names() %>% 
  filter(state_from != "Died")


# A %>% 
#   select(-id) %>% 
#   # group_by(year, `_sex`, ocup_isco_H, ocup_isco_R, age_yr, state_from, cat_from,   state_to, cat_to) %>% 
#   distinct()

# N <- nrow(A)
# A$ocup_isco_R %>% unique()
# A$state_from %>% unique()
# A$cat_from %>% unique()
# A$cat_to %>% unique() # missings all in 2020
# is.na(A$state_to)
# is.na(A$cat_from) %>% sum() / N
# is.na(A$cat_to) %>% sum() / N
# 
# is.na(A$ocup_isco_R) %>% sum()
# 
# A %>% filter(year <2020) %>% 
#   pull(state_to) %>% unique()


# empirical transition probabilities

# vectors for complete()
all_years       <- A %>% pull(year) %>% unique() %>% sort()
all_ages        <- A %>% pull(age_yr) %>% unique() %>% sort()
all_sexes       <- A %>% pull(sex) %>% unique() %>% sort()
all_ocup_isco_h <- A %>% pull(ocup_isco_h) %>% unique() %>% sort()
all_state_from  <- A %>% pull(state_from) %>% unique() %>% sort()
all_state_to    <- A %>% pull(state_to) %>% unique() %>% sort()


# B: ocup_isco_h, sex, year, age state_from, state_to

B <-
  A %>% 
  count(#year, 
        sex, age_yr, ocup_isco_h, state_from, state_to,
        name = "transitions") %>% 
  complete(#year = all_years,
           age_yr = all_ages,
           sex = all_sexes,
           ocup_isco_h = all_ocup_isco_h,
           
           # Maybe we actually want to pad 0s for
           # from_state = "Died", for sake of creating U 
           # in elegant way
           state_from = all_state_from,
           state_to = all_state_to,
           fill = list(transitions = 0)) %>% 
  add_count(#year, 
            sex, age_yr, ocup_isco_h, state_from,
            name = "denom",
            wt = transitions) %>% 
  mutate(p = transitions / denom,
         p = if_else(transitions == 0, 0, p)) %>% 
  filter(between(age_yr, 25, 75))

B %>% 
  filter(is.nan(p))
all_ocup_isco_h
pdf("Figs/all_ocup_isco_h_transitions.pdf")
for (ocup in all_ocup_isco_h){
  for (st_from in all_state_from){
    for (st_to in all_state_to){
      chunk <- 
        B %>% 
        filter(ocup_isco_h == ocup,
               state_from == st_from,
               state_to == st_to)
      N <- chunk %>% pull(transitions) %>% sum()
      p <-
        chunk %>% 
        ggplot(aes(x = age_yr,
                   y = p,
                   color = sex,
                   group = sex)) +
        geom_line() +
        scale_color_manual(values = c(Hombre = "blue", Mujer = "red")) +
        geom_smooth() +
        labs(title = paste0("ocup_isco_h = ",ocup,
                            "\nfrom ", st_from,
                            " to ", st_to),
             subtitle = paste0("total transitions = ",N))
      print(p)
    }
  }
}
dev.off()
B %>% 
  filter(ocup_isco_h == "Routine",
         state_from == "Disability",
         state_to == "Died") %>% 
  ggplot(aes(x = age_yr,
             y = p,
             color = sex,
             group = sex)) +
  geom_line() +
  scale_color_manual(values = c(Hombre = "blue", Mujer = "red"))


B %>% colnames()
A %>% colnames()
A %>% pull(year) %>% range()

A %>% 
  filter(state_to == "Died") %>% 
  count(#year, 
    sex, age_yr,
    name = "transitions") %>% 
  ggplot(aes(x = age_yr, y = transitions, fill = sex)) +
  geom_col()
  

A %>% 
  count(sex, state_from, state_to, name = "transitions") %>% 
  pivot_wider(names_from = state_to, values_from = transitions) %>% 
  write_csv("Data/total_transitions.csv")

all_years - all_years %% 2
all_year2 <- seq(2004,2020,by=2)
D <-
  A |>
  mutate(year2 = year - year %% 2) |> 
  count(year2, 
    sex, age_yr, ocup_isco_h, state_from, state_to,
    name = "transitions") |> 
  complete(year2 = all_year2,
    age_yr = all_ages,
    sex = all_sexes,
    ocup_isco_h = all_ocup_isco_h,
    state_from = all_state_to,
    state_to = all_state_to,
    fill = list(transitions = 0)) |> 
  add_count(#year, 
    sex, age_yr, ocup_isco_h, state_from,
    name = "denom",
    wt = transitions) |> 
  mutate(p = transitions / denom,
         p = if_else(transitions == 0, 0, p)) |> 
  filter(between(age_yr, 25, 75))

D %>% 
  filter(state_from == "Unemployed",
         state_to == "Inactive") %>% 
  ggplot(aes(x = age_yr, y = p, color = sex, alpha = year2, group = interaction(sex,year2))) +
  geom_line() +
  facet_wrap(~ocup_isco_h)



