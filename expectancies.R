
source("R/ms_functions.R")

if (!exists("D")){
  source("R/transition_probs.R")
}
E <-
  D %>% 
  group_by(ocup_isco_h,
           sex,
           year2) %>% 
  group_modify(~make_ex(data_chunk = .x, 
                        init_lower = 25, 
                        init_upper = 29))
E
# View provisional expectancies
E %>% 
  filter(year2 < 2020) %>% 
  ggplot(aes(x = year2, 
             y = Ex, 
             color = ocup_isco_h, 
             linetype = sex)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y")
