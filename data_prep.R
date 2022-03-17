source("functions.R")
# Individual-level covariates for > 1million people
Per <- read_dta("Data/MCVL2020PERSONA.dta") %>% 
  dplyr::filter(sexe > 0) %>% 
  mutate(IPF = as.integer(ident),
         # get proper dates
         dob = lubridate::dmy(paste("15",sprintf("%02d",mesnac),anynac, sep = ".")),
         any_mort = ifelse(any_mort == 0, NA, any_mort),
         dod = lubridate::dmy(paste("15",sprintf("%02d",mes_mort),any_mort, sep = ".")),
         
         # group education categories
         estudis = as.integer(estudis),
         edu = case_when(between(estudis,10,21) ~ "sin estudios",
                         between(estudis,22,31) ~ "primarios",
                         between(estudis,32, 42) ~ "secundarios",
                         between(estudis,43,48) ~ "universitarios",
                         TRUE ~ "desconocido")) %>% 
  select(IPF, dob, sex = sexe, edu, dod, anynac)

# employment trajectories to be merged with the above
Cot <- read_dta("Data/COTITZACIONS2020.dta",
                # We explicitly select particular columns
                # There are more columns that could also be explored in the
                # future
                col_select = c(IPF,
                               regimen, # we'll filter to regimen general
                               grupo,
                               contrato,
                               parcial,
                               alta, # date
                               baja,  # date
                               causa,
                               CCC,
                               CNAE_2009,
                               CNAE93,
                               trabajadores,
                               TRL
                               ),
                n_max = 3e5) %>% 
  # get date formats
  mutate(alta = ymd(alta),
         baja = ymd(baja),
         alta2 = baja,
         # alta should precede baja.
         # if it doesn't then we swap
         swap_ = alta > baja,
         alta = if_else(swap_, baja, alta),
         baja = if_else(swap_, alta2, baja),
         baja = if_else(year(baja) > 2021, NA_Date_, baja),
         baja = if_else(alta == baja, baja + 1, baja),
         duration = my_date_diff(date_earlier = alta, date_later = baja)) %>% 
  dplyr::filter(!is.na(alta),
                !is.na(baja)) %>% 
  select(-swap_, -alta2)

# Join and keep all rows where we have a matching person id (IPF)
LC1 <- 
  Per %>% 
  inner_join(Cot, by = "IPF") %>% 
  # filter to general regimen workers 
  # (could this explain some gaps that are later padded?)
  dplyr::filter(between(regimen, 111, 150)) %>% 
  select(-regimen) %>% 
  # group occupation categories
  mutate(occ = case_when(
    between(grupo, 1,3) ~ "skilled non manual",
    between(grupo, 4, 7) ~ "unskilled non manual",
    between(grupo, 8, 9) ~ "skilled manual",
    between(grupo, 10, 11) ~ "unskilled manual",
    grupo == 0 ~ "no consta",
    grupo >= 12 ~ "16 years old" # also other ways to end 
                                 # up here, merely indicative
                                 # don't use as age filter!
  )) %>% 
  select(-grupo) %>% 
  # Create main contract indicator: temp, perm, and unemp
  mutate(temp = case_when(
    contrato == 0 & between(TRL,700,799) ~ "unemp",
    #contrato == 0 & (TRL < 700 | TRL >= 800) ~ "no consta",
    between(contrato, 1, 3) ~ "perm", # permanent
    contrato == 9 ~ "perm",
    contrato == 20 ~ "perm",
    contrato == 28 ~ "perm",
    contrato == 40 ~ "perm",
    contrato == 35 ~ "perm",
    contrato == 42 ~ "perm",
    between(contrato, 44, 49) ~ "perm",
    between(contrato, 60, 61) ~ "perm",
    contrato == 69 ~ "perm",
    between(contrato, 70,71) ~ "perm",
    contrato == 100 ~ "perm",
    between(contrato, 109, 157) ~ "perm",
    between(contrato, 189, 389) ~ "perm",
    duration > 4 ~ "perm",
    is.na(baja) ~ "no consta",
    contrato == 0 & (TRL < 700 | TRL >= 800) ~ "no consta",
    TRUE ~ "temp"))%>% 
  select(-contrato) %>% 
  
  # turn work time per mil into a binary for full and part
  # however, we may reconsider keeping the full info if needed
  # to aggregate to marginality classes (short contracts / low %, etc)
  mutate(part = case_when(
    parcial == 0 ~ "full",
    parcial >= 800 ~ "full",
    TRUE ~ "part"
  )) %>% 
  select(-parcial) %>% 
  # group sector categories. Knarly recoding based on two different tables
  mutate(sector = case_when(between(TRL, 700,799) ~ "unemployed",
    year(alta) > 2008 & between(CNAE_2009, 11, 99) ~ "agriculture",
    year(alta) > 2008 & between(CNAE_2009, 501, 592) ~ "industrial",
    year(alta) > 2008 & between(CNAE_2009, 101, 192) ~ "industrial",
    year(alta) > 2008 & between(CNAE_2009, 301, 392) ~ "T&E",
    year(alta) > 2008 & between(CNAE_2009, 401, 439) ~ "construction",
    year(alta) > 2008 & between(CNAE_2009, 451, 479) ~ "services",
    year(alta) > 2008 & between(CNAE_2009, 491, 495) ~ "T&E",
    year(alta) > 2008 & between(CNAE_2009, 551, 639) ~ "services",
    year(alta) > 2008 & between(CNAE_2009, 641, 683) ~ "finance",
    year(alta) > 2008 & between(CNAE_2009, 691, 702) ~ "R&D",
    year(alta) > 2008 & between(CNAE_2009, 711, 829) ~ "services",
    year(alta) > 2008 & between(CNAE_2009, 841, 854) ~ "public",
    year(alta) > 2008 & between(CNAE_2009, 861, 889) ~ "medical",
    year(alta) > 2008 & between(CNAE_2009, 900, 960) ~ "services",
    year(alta) > 2008 & between(CNAE_2009, 970, 982) ~ "domestic",
    year(alta) > 2008 & CNAE_2009 == 990 ~ "public",
    year(alta) <= 2008 & between(CNAE93, 10, 92) ~ "agriculture", 
    year(alta) <= 2008 & between(CNAE93, 101, 343) ~ "industrial", 
    year(alta) <= 2008 & between(CNAE93, 351, 455) ~ "construction", 
    year(alta) <= 2008 & between(CNAE93, 501, 555) ~ "services", 
    year(alta) <= 2008 & between(CNAE93, 601, 672) ~ "T&E", 
    year(alta) <= 2008 & between(CNAE93, 701, 714) ~ "finance", 
    year(alta) <= 2008 & between(CNAE93, 721, 726) ~ "services", 
    year(alta) <= 2008 & between(CNAE93, 731, 732) ~ "R&D", 
    year(alta) <= 2008 & between(CNAE93, 741, 752) ~ "services", 
    year(alta) <= 2008 & between(CNAE93, 801, 804) ~ "public", 
    year(alta) <= 2008 & between(CNAE93, 851, 853) ~ "medical", 
    year(alta) <= 2008 & between(CNAE93, 900, 913) ~ "public", 
    year(alta) <= 2008 & between(CNAE93, 921, 930) ~ "services", 
    year(alta) <= 2008 & CNAE93 == 950 ~ "domestic", 
    year(alta) <= 2008 & CNAE93 == 990  ~ "public", 
    TRUE ~ "unknown")) %>% 
  select(-TRL, -CNAE93, -CNAE_2009) %>% 
  mutate(causa = if_else(is.na(baja), 98, causa),
         age_at_start = my_date_diff(date_earlier = dob, date_later = alta)) %>% 
  arrange(IPF, alta)

# here we 
# 1. eliminate instances of ere, 
# and similar with delete_contained_unemp()
# as long as they are fully within an envelope of another spell
# 2. add 1-day death episodes, truncating other episodes as needed
# 3. infer inactivity spells between incoming episodes,
# where a few types are inferred from causa or similar
# 4. detect and merge runs of long term unemployment and inactivity 
# (over 1yr threshold).

# 28 min on laptop for ca 37000 trajectories
tic()
cluster <- new_cluster(7)
cluster_copy(cluster, c("pad_inactivity","merge_ltu","my_date_diff","pad_deaths","delete_contained_unemp"))
cluster_library(cluster,packages=c("tidyverse","lubridate"))
LC_padded <- 
  LC1 %>% 
  arrange(IPF,alta) %>% 
  group_by(IPF) %>% 
  partition(cluster) %>% 
  do(delete_contained_unemp(X = .data) %>% 
     pad_deaths() %>% 
     pad_inactivity() %>% 
     merge_ltu()) %>% 
  collect() %>% 
  ungroup()
toc()
rm(cluster);gc()
# rm(LC1,LC2);gc()


tic()
cluster <- new_cluster(7)
cluster_copy(cluster, "discretize_trajectory")
cluster_library(cluster,"tidyverse")
random_draws <- sample(LC_padded$IPF %>% unique(), size = 1000, replace = FALSE)
LC_discrete <- 
  LC_padded %>% 
  # dplyr::filter(IPF %in% random_draws) %>% 
  mutate(alta = decimal_date(alta),
         baja = decimal_date(baja),
         baja = ifelse(is.na(baja),2021, baja)) %>% 
  dplyr::filter(baja > alta) %>% 
  arrange(IPF,alta) %>% 
  group_by(IPF) %>% 
  partition(cluster) %>% 
  do(discretize_trajectory(X = .data, step_size = .5)) %>% 
  collect() %>% 
  ungroup()
toc()
rm(cluster);gc()
# 12.5 minutes for 1000 on 7 cores.
# 7.367425 hours 37147 that were matched to 
# the first 1 million cotizaciones
readr::write_csv(LC_discrete, file = "Data/LC_discrete.csv")
LC_discrete$IPF %>% unique() %>% length()
LC_discrete <- read_csv("Data/LC_discrete.csv")
# using quarters only implies 15% growth in rows vis a vis
# the original date (exact) trajectories. An acceptable penalty
nrow(LC_discrete) / nrow(LC_padded)

# ----------------------------------------------#
## temporary/ experimental code from here down ##
# ----------------------------------------------#

# which states follow unemp

# if unemp is approx 1 yr and followed by temp %in% c("no consta", "labor gap") then merge to
# form temp = "ltu".
LC1 %>% 
  filter(temp == "unemp") %>% 
  ggplot(aes(x=duration)) +
  geom_density(bandwidth = .01,fill = gray(.5))+
  xlim(0,5)
LC1 %>% 
  filter(temp == "unemp") %>% 
  pull(duration) %>% 
  cut(breaks = seq(0,35,by=5)) %>% table()
LC_padded$duration %>% '=='(0) %>% sum()
LC_padded %>% 
  dplyr::filter(temp %in% c("unemp","ltu")) %>% 
  ggplot(aes(y = temp, x = duration)) +
  geom_density_ridges(fill = gray(.5),bandwidth=.01) +
  xlim(0,5)
LC_padded$temp %>% table()
LC_padded %>% 
  filter(temp == "ltu" & duration < 1)
X <-
  LC_padded %>% 
  filter(IPF == 531) 

merge_ltu(X)
LC_padded %>% filter(temp == "unemp",
                     duration < 1) %>% 
  ggplot(aes(x=duration)) + 
  geom_density(fill = gray(.5)) +
  xlim(0,1)

LC_padded %>% 
  group_by(IPF) %>% 
  mutate(ltu = temp == "unemp" & duration > .97 & lead(temp) %in% c("no consta","labor gap") & !is.na(lead(occ)),
         temp = if_else(ltu, "ltu", temp), 
         dur_old = duration,
         baja = if_else(ltu, lead(baja), baja),
         duration = my_date_diff(date_earlier = alta, date_later = baja),
         ltu_drop = lag(temp) == "ltu") %>% 
  filter(temp == "ltu") %>% pull(duration) %>% sum()
ungroup() %>% 
  dplyr::filter(!ltu_drop,
                temp %in% c("unemp","ltu")) %>% 
  ggplot(aes(y = temp, x = duration)) +
  geom_density_ridges(fill = gray(.5)) +
  xlim(0,5)


LC_padded %>% filter(temp == "unemp",
                     duration >= 1) %>% 
  pull(duration) %>% 
  cut(breaks = seq(0,40,by=5)) %>% table()


LC_padded$IPF %>% unique() %>% length()

# remotes::install_github("timriffe/Spells/R/Spells", force = TRUE)
# test applying alignments
library(Spells)
LC_aligned <- 
  LC_discrete %>% 
  group_by(IPF) %>% 
  mutate(next_temp = lead(temp),
         keep_ = any(start > 1997 & next_temp == "perm" & temp != "perm")) %>% 
  ungroup() %>% 
  dplyr::filter(keep_) %>% 
  group_by(IPF) %>% 
  mutate(first_perm = Spells::align(temp, state = "perm", type = "left", spell = "first", step_size = .25),
         longest_perm = Spells::align(temp,  state = "perm", type = "left", spell = "longest", step_size = .25))


LC_aligned %>% 
  mutate(coh10 = year(dob) - year(dob) %% 10) %>% 
  dplyr::filter(between(coh10, 1940, 1980)) %>% 
  group_by(IPF) %>% 
  mutate(period = start[first_perm == 0] > 2008) %>% 
  group_by(period, sex,first_perm) %>% 
  summarize(n = n(),
            labor_gap = sum(temp == "labor gap") / n(),
            temp_job = sum(temp == "temp") / n(),
            care_gap = sum(temp == "care gap") / n(),
            unemp = sum(temp == "unemp") / n(),
            retired = sum(temp == "retirement") / n(),
            perm = sum(temp == "perm") / n(),
            .groups = "drop") %>% 
  pivot_longer(labor_gap:perm, names_to = "temp", values_to = "prev") %>% 
  # filter(first_perm >= 0) %>% 
  ggplot(aes(x = first_perm, y = prev, color = temp, group = temp)) +
  geom_line() +
  facet_grid(vars(period), vars(sex))
  # filter(decade == 2020, coh10 == 1980) %>% View()

random_draws_m <- LC_padded %>% filter(sex == 1) %>% pull(IPF) %>% sample(100, replace=FALSE)
pm <- 
LC_discrete %>% 
  dplyr::filter(IPF %in% random_draws_m) %>% 
  plot_traj_2() +
  labs(title = "100 randomly selected men")+
  theme_minimal()

random_draws_f <- LC_padded %>% filter(sex == 2) %>% pull(IPF) %>% sample(100, replace=FALSE)
pf <-
LC_discrete %>% 
  dplyr::filter(IPF %in% random_draws_f) %>% 
  plot_traj_2()  +
  labs(title = "100 randomly selected women") +
  theme_minimal()
plot_grid(pm,pf)
  
LC_padded %>% 
  dplyr::filter(IPF %in% random_draws) %>% 
  plot_traj_1()
# note example case 31012
LC_discrete %>% 
  group_by(IPF) %>% 
  mutate(has_dead = any(temp =="dead")) %>% 
  dplyr::filter(has_dead) %>% 
  plot_traj_2() 



LC %>% 
  group_by(IPF) %>% 
  mutate(keep_ = any(temp == "perm"),
         age = decimal_date(alta) - decimal_date(dob),
         age5 = age - age %% 5,
         coh5 = year(dob) - year(dob) %% 5) %>% 
  ungroup() %>% 
  dplyr::filter(keep_) %>% 
  group_by(IPF) %>% 
  summarize(ttp = (alta[temp == "perm"] %>% min() %>% decimal_date()) -
            (alta %>% min() %>% decimal_date()), 
            age5 = age5[temp == "perm"][which.min(decimal_date(alta)[temp == "perm"])],
            sex =sex[1],
            coh5 = coh5[1],
            ttpmax = 2021 - (alta %>% min() %>% decimal_date()),
            .groups = "drop") %>% 
  dplyr::filter(coh5 < 1990) %>% 
  mutate(ttp = floor(ttp)) %>% 
  ggplot(aes(x = ttp, color = coh5, fill = coh5)) +
  geom_bar(position = "stack")





LC %>% 
  ggplot(aes(x = alta, y = dob)) +
  geom_density2d_filled() +
  facet_wrap(~temp)

LC %>% 
  filter(IPF == 32) %>% 
  arrange(alta) %>% 
  View()


  filter(temp == "perm") %>% 
  ggplot(aes(x = duration)) +
  geom_density() +
  facet_wrap(~age5) +
  xlim(0,12)

LC %>% 
  group_by(IPF) %>% 
  summarize(turb = n() / length(unique(CCC)),
            n = decimal_date(max(baja)) - decimal_date(min(alta)), .groups = "drop") %>% 
  ggplot(aes(x=n)) +
  geom_density() +
  scale_x_log10()
X <- LC %>% 
  filter(IPF == 32)

random_draws <- 
LC %>% 
  pull(IPF) %>% unique() %>% sample(size = 1, replace = FALSE)

# X <-
# LC %>% 
#   filter(IPF %in% random_draws) 
  

pad_inactivity(X) %>% 
  plot_traj()



# discretize trajectory
X <- 
  LC_padded %>% 
  filter(IPF %in% random_draws) %>% 
  mutate(alta = decimal_date(alta),
         baja = decimal_date(baja))

step_size <- 1/4

orig <- 
  X %>% 
  plot_traj_1() +
  theme_void() +
  xlim(1967,2013)

d.1 <-
  discretize_trajectory(X, .1) %>% 
  plot_traj_2() +
  guides(fill = "none") +
  theme_void()+
  xlim(1967,2013)

d.25 <- 
  discretize_trajectory(X, .25) %>% 
  plot_traj_2() +
  guides(fill = "none") +
  theme_void() +
  xlim(1967,2013)
d.5 <- 
discretize_trajectory(X, .5) %>% 
  plot_traj_2() +
  guides(fill = "none") +
  theme_void() +
  xlim(1967,2013)
d1 <-
discretize_trajectory(X, 1) %>% 
  plot_traj_2()+
  guides(fill = "none") +
  theme_void()+
  xlim(1967,2013)
pcol <- plot_grid(
  orig + guides(fill = "none"),
  d.1, 
  d.25, 
  d.5, 
  d1,
  ncol = 1
)

legend_b <- get_legend(
  orig + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
pcol <-
  pcol +
  draw_plot_label(label = c("exact","0.1","0.25","0.5","1"),
                  x = rep(0.05,5),
                  y = (rep(.9,1) + 4:0)/ 5,
                  hjust = 0,
                  vjust = .5)
library(cowplot)
plot_grid(pcol, legend_b, ncol = 1, rel_heights = c(1, .15))
