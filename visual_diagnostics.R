
library(readr)
library(tidyverse)
library(janitor)
library(magrittr)
A <- read_csv("Data/Employment_trajectories2004-2020_v2.csv")
A <-
  A %>% 
  clean_names() %>% 
  filter(state_from != "Died")

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


pdf("Figs/all_ocup_isco_h_transitions.pdf")

for (st_from in all_state_from){
  for (st_to in all_state_to){
    chunk <- 
      B %>% 
      filter(state_from == st_from,
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
      #geom_smooth() +
      labs(title = paste0("\nfrom ", st_from,
                          " to ", st_to),
           subtitle = paste0("total transitions = ",N)) +
      facet_wrap(~ocup_isco_h)
    print(p)
  }
}
dev.off()


# spot checks
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

# how many deaths are there in here?
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

library(tidyverse)

IN <- read_csv("Data/MCVL_2004-2020.csv", n_max = 100)
IN %>% View()



