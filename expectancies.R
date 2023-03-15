library(tidyverse)
library(collapse)
library(tidyfast)
source("ms_functions.R")

if (!exists("D")){
  # source("transition_probs.R")
  D <- read_csv("Data/transitions_13-3-2023.csv", 
                show_col_types = FALSE)
}
data_chunk <-
D |> 
  fsubset(ocup_H == "Managerial"&
          sex=="Hombre"&
          year==2008&
            stateto!="Died") |> 
  fmutate(p = replace_na(p))
lower = 30
upper = 75
init_lower = 30
init_upper = 34


E30 <-
  D %>% 
  fsubset(stateto!="Died") |> 
  fmutate(p=replace_na(p)) |> 
  group_by(ocup_H,
           sex,
           year) %>% 
  group_modify(~make_ex(data_chunk = .x, 
                        lower = 30,
                        upper = 75,
                        init_lower = 30,
                        init_upper = 34))
# quick look
E30
write_csv(E30,"Results/E30.csv")
# View provisional expectancies
E30 <- read_csv("Data/Results/E30.csv")
E30 %>% 
  #filter(year2 < 2020) %>% 
  mutate(state = factor(state, levels = c("Employed", "Unemployed","Inactive","Disability","Disability-retirement","Retired")),
         Gender = if_else(sex == "Hombre","Men","Women"),
         Ocupation = factor(ocup_H,levels = c("Managerial","Intermediate","Routine"))) %>% 
  rename(`expected years (scales vary)` = Ex) %>% 
  ggplot(aes(x = year, 
             y = `expected years (scales vary)`, 
             color = Ocupation, 
             linetype = Gender)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  theme_minimal()


E30 |> 
  group_by(year,sex,ocup_H) |> 
  summarise(LE=sum(Ex)) |> 
  ggplot(aes(x=year,y=LE,color=ocup_H,linetype=sex))+geom_line() +
  theme_minimal()

flt <- HMDHFDplus::readHMDweb("ESP","fltper_1x1",
                       username = Sys.getenv("us"),
                       password = Sys.getenv("pw")) |> 
  filter(Year >= 2012) |> 
  mutate(sex = "Mujer",.before=1) |> 
  filter(between(Age,30,74)) |> 
  group_by(sex,Year) |> 
  summarize(LE = sum(Lx) / lx[1],.groups="drop")
mlt <- HMDHFDplus::readHMDweb("ESP","mltper_1x1",
                              username = Sys.getenv("us"),
                              password = Sys.getenv("pw")) |> 
  filter(Year >= 2012) |> 
  mutate(sex = "Hombre",.before=1) |> 
  filter(between(Age,30,74)) |> 
  group_by(sex,Year) |> 
  summarize(LE = sum(Lx) / lx[1],.groups="drop")

bind_rows(flt,mlt) |> 
ggplot(aes(x=Year,y=LE,color=sex))+
  geom_line()+
  ylim(37,46)+
  theme_minimal()
# repeat for age 50 to compare with Stanek & Requena
E50 <-
  D %>% 
  group_by(ocup_isco_h,
           sex,
           year2) %>% 
  group_modify(~make_ex(data_chunk = .x, 
                        lower = 50,
                        upper = 75,
                        init_lower = 50,
                        init_upper = 54))
E50

E50 %>% 
  filter(year2 < 2020) %>% 
  ggplot(aes(x = year2, 
             y = Ex, 
             color = ocup_isco_h, 
             linetype = sex)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y")

E50 %>% 
  write_csv("Data/Results/E50.csv")
