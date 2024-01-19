library(tidyverse)
library(janitor)
library(vroom)
library(collapse)
library(tidyfast)
library(lubridate)
IN <- vroom("Data/MCVL_2004-2020_Feb16.zip", col_types = paste(rep("c",205),collapse=""))


# IN |> 
#   select(starts_with("dod2")) |> 
#   pivot_longer(everything(),names_to = "col",values_to="dod") |> 
#   fmutate(dod = dmy(dod)) |> 
#   count(dod) |> 
#   fsubset(!is.na(dod)) |> 
#   ggplot(aes(x=dod,y=n)) + 
#   geom_line() + 
#   labs(title = "Total deaths by month\nincrease due to repeated values I presume")
# dob is just repeated, can keep 2004 value
# dod is also repeated for all years <= yod, so we can keep the 2004 value...
# dataYYYY is always just 2020 ...
# _sexYYYY is awlays the same
# no deaths in 2020? dod12020

# what's ocup_isco H vs R ? is ocup same as cat?
# does cat_from2017 go to cat_to2017 or to cat_to2018?

# what the difference between st_from_old* and st_from*



chunks_endpoints <- c(seq(0,nrow(IN),by=100000),nrow(IN))
outL <- list()

# IN |> 
#   # didn't find replacement
#   slice(200000:300000) |> 
#   # fselect() doesn't work with the helper functions...
#   select(id,
#          dob = dob32004,
#          dod = dod22004,
#          sex = `_sex2004`,
#          ocup_H = ocup_isco_H2004,
#          ocup_R = ocup_isco_R2004,
#          starts_with("state_from"),
#          starts_with("state_to"),
#          starts_with("cat_from"),
#          starts_with("cat_to")) |> 
#   select(!ends_with("2020")) |> 
#   dt_pivot_longer(-(1:6), names_to = "states_and_stuff", values_to = "value") |> 
#   fsubset(!is.na(value)) |> 
#   fmutate(year = parse_number(states_and_stuff),
#           dob = dmy(dob),
#           dod = dmy(dod)) |> 
#   # could be replaced
#   arrange(id, year) |> 
#   fmutate(states_and_stuff = gsub(states_and_stuff, 
#                                   pattern = "state_from", 
#                                   replacement = "statefrom_"),
#           states_and_stuff = gsub(states_and_stuff, 
#                                   pattern = "state_to", 
#                                   replacement = "stateto_"),
#           states_and_stuff = gsub(states_and_stuff, 
#                                   pattern = "cat_to", 
#                                   replacement = "catto_"),
#           states_and_stuff = gsub(states_and_stuff, 
#                                   pattern = "cat_from", 
#                                   replacement = "catfrom_")) |> 
#   # couldn't get the new separate_wider_delim to work
#   separate(states_and_stuff, 
#            sep = "_",
#            into = c("statecat","year")) |> 
#   dt_pivot_wider(names_from = statecat, values_from = value) |>
#   fsubset(statefrom != "Died" &
#             !is.na(statefrom) &
#             !is.na(stateto)) |>  
#   fmutate(year = as.integer(year),
#           cohort = year(dob),
#           age = year - cohort + 1) |>
#   fsubset(between(age,30,75)) |> 
#   # couldn't get collapse::fcount to work
#   group_by(statefrom,stateto) |> 
#   count() |> 
#   pivot_wider(names_from = statefrom, values_from = n)


for (i in 1:(length(chunks_endpoints)-1)){
  
  indices_i <- chunks_endpoints[i]:chunks_endpoints[i+1]
  outL[[i]] <-
  
  IN |> 
  # didn't find replacement
  slice(indices_i) |> 
    # fselect() doesn't work with the helper functions...
  select(id,
         dob = dob32004,
         dod = dod22004,
         sex = `_sex2004`,
         ocup_H = ocup_isco_H2004,
         ocup_R = ocup_isco_R2004,
         starts_with("state_from"),
         starts_with("state_to"),
         starts_with("cat_from"),
         starts_with("cat_to")) |> 
  select(!ends_with("2020")) |> 
  dt_pivot_longer(-(1:6), names_to = "states_and_stuff", values_to = "value") |> 
  fsubset(!is.na(value)) |> 
  fmutate(year = parse_number(states_and_stuff),
          dob = dmy(dob),
          dod = dmy(dod)) |> 
  # could be replaced
  arrange(id, year) |> 
  fmutate(states_and_stuff = gsub(states_and_stuff, 
                                  pattern = "state_from", 
                                  replacement = "statefrom_"),
         states_and_stuff = gsub(states_and_stuff, 
                                 pattern = "state_to", 
                                 replacement = "stateto_"),
         states_and_stuff = gsub(states_and_stuff, 
                                 pattern = "cat_to", 
                                 replacement = "catto_"),
         states_and_stuff = gsub(states_and_stuff, 
                                 pattern = "cat_from", 
                                 replacement = "catfrom_")) |> 
  # couldn't get the new separate_wider_delim to work
  separate(states_and_stuff, 
                       sep = "_",
                       into = c("statecat","year")) |> 
  dt_pivot_wider(names_from = statecat, values_from = value) |>
  fsubset(statefrom != "Died" &
          !is.na(statefrom) &
          !is.na(stateto)) |>  
  fmutate(year = as.integer(year),
          cohort = year(dob),
          age = year - cohort + 1) |>
  fsubset(between(age,30,75)) |> 
    # couldn't get collapse::fcount to work
  group_by(year,sex,age,ocup_H,ocup_R,statefrom,stateto) |> 
  count() |> 
  fgroup_by(year,sex,age,ocup_H,ocup_R,statefrom) |> 
  fmutate(N = sum(n)) |> 
  fungroup() 
gc()
}
# highest occup is the main one
# sex, occup, year, ages 30 - 75

transitions_semifinal_H <-
  outL |> 
  bind_rows() |> 
  group_by(year,sex,age,ocup_H,statefrom,stateto) |> 
  summarise(n=sum(n,na.rm=TRUE),
            N = sum(N),.groups = "drop")
transitions_semifinal_R <-
  outL |> 
  bind_rows() |> 
  group_by(year,sex,age,ocup_R,statefrom,stateto) |> 
  summarise(n=sum(n,na.rm=TRUE),
            N = sum(N),.groups = "drop")

all_ocup_R    <- transitions_semifinal_R |> pull(ocup_R) |> unique()
# need to pad the data with 0s where needed
all_years     <- transitions_semifinal_H |> pull(year) |> unique()
all_ages      <- transitions_semifinal_H |> pull(age) |> unique()
all_ocup_H    <- transitions_semifinal_H |> pull(ocup_H) |> unique()
all_statefrom <- transitions_semifinal_H |> pull(statefrom) |> unique()
all_stateto   <- transitions_semifinal_H |> pull(stateto) |> unique()
all_sexes     <- transitions_semifinal_H |> pull(sex) |> unique()

transitions_seemingly_final_H <-
  transitions_semifinal_H |> 
  complete(year = all_years,
           age = all_ages,
           sex = all_sexes,
           ocup_H = all_ocup_H,
           statefrom = all_statefrom,
           stateto = all_stateto,
           fill = list(n=0)) |> 
  group_by(year,sex,age,ocup_H,statefrom) |> 
  fmutate(N = sum(n)) |> 
  ungroup() |> 
  fmutate(p = n / N,
          p = replace_na(p, 0))

transitions_seemingly_final_H |> 
  write_csv("Data/transitions_H_18-01-2024.csv")

transitions_seemingly_final_R <-
  transitions_semifinal_R |> 
  complete(year = all_years,
           age = all_ages,
           sex = all_sexes,
           ocup_R = all_ocup_R,
           statefrom = all_statefrom,
           stateto = all_stateto,
           fill = list(n=0)) |> 
  group_by(year,sex,age,ocup_R,statefrom) |> 
  fmutate(N = sum(n)) |> 
  ungroup() |> 
  fmutate(p = n / N,
          p = replace_na(p, 0))

transitions_seemingly_final_R |> 
  write_csv("Data/transitions_R_18-01-2024.csv")


transitions_seemingly_final_R |> 
  filter(stateto == "Died")

transitions_seemingly_final_R <- 
  transitions_seemingly_final_R |> 
  mutate(ocup_type = "most recent", .before = ocup_R) |> 
  rename(ocup = ocup_R)
transitions_seemingly_final_H <- 
  transitions_seemingly_final_H |> 
  mutate(ocup_type = "highest", .before = ocup_H) |> 
  rename(ocup = ocup_H)
transitions_seemingly_final <- 
  bind_rows(transitions_seemingly_final_R,
            transitions_seemingly_final_H)

transitions_seemingly_final |> 
  write_csv("Data/transitions_RH_18-01-2024.csv")

transitions_seemingly_final$stateto |> unique()
transitions_seemingly_final |> 
  filter(year == 2021,
         stateto == "Employed",
         statefrom == "Died") |> 
  ggplot(aes(x=age, y = p, color = ocup_type, linetype = sex)) +
  geom_line() +
  facet_wrap(~ocup)

# transitions_seemingly_final |> 
#   filter(stateto == "Disability-retirement",
#          ocup_type == "most recent") |> 
#   group_by(year, statefrom) |> 
#   summarize(n = sum(n),
#             N = sum(N),
#             .groups = "drop") |>
#   mutate(CDR = n / N) |> 
#   ggplot(aes(x=year, y = n, color = statefrom)) +
#   geom_line() +
#   labs(title = "total deaths by year and state",
#        y = "deaths") +
#   theme_minimal() 

read_csv("Data/transitions_13-3-2023.csv", show_col_types = FALSE) |> 
  group_by(statefrom, stateto) |> 
  summarize(n = sum(n)) |> 
  pivot_wider(names_from = statefrom, values_from = n)
NN = 1
all_yearsn <- unique(all_years - all_years %% NN)

pdf("Figs/all_transitions3.pdf")
for (i in 1:length(all_yearsn)){
  for (o in all_ocup_H){
    for (s in c("Hombre","Mujer")){
      p <-
      transitions_seemingly_final |> 
        fsubset(sex == s & ocup_H == o) |> 
        fmutate(year = year - year %% NN) |> 
        group_by(year,age,ocup_H,statefrom,stateto) |> 
        fsummarize(n=sum(n),N=sum(N)) |> 
        fmutate(p = n/N) |> 
        fsubset(year == all_yearsn[i]) |> 
        ggplot(aes(x = age, y = p, color = stateto, group = stateto))+
        geom_line() +
        facet_wrap(~statefrom) +
        labs(title = paste(all_yearsn[i],s,o,sep=","))
      print(p)
    }
  }
}
dev.off()
# 
# transitions_seemingly_final |> 
#   group_by(statefrom,stateto) |> 
#   fsummarise(n=sum(n)) |> 
#   pivot_wider(names_from = statefrom, values_from = n)
# 
# 
# 
# # mortality testing:
# qx_test <-
#   INlong |> 
#   fsubset(statefrom != "Died") |> 
#   fmutate(year = as.integer(year),
#           cohort = year(dob),
#           age = year - cohort + 1,
#           year = year - year %% 3,
#           age = age - age %% 3) |> 
#   fgroup_by(year,sex,age) |> 
#   fsummarise(D = sum(stateto == "Died",na.rm=TRUE),
#              P = sum(!is.na(statefrom)), 
#              keep.group_vars = TRUE) |> 
#   fmutate(qx = D / P) 
# 
#   ggplot(aes(x = age, y = qx, color = year,group=year)) +
#   geom_line() +
#   scale_y_sqrt()+
#   facet_wrap(~sex) +
#   xlim(25,100)
# 
# library(HMDHFDplus)
# flt <-readHMDweb("ESP","fltper_1x1",
#                  username = Sys.getenv("us"), 
#                  password = Sys.getenv("pw")) |> 
#   clean_names() |> 
#   fsubset(year %in% c(2004:2020)) |> 
#   fmutate(sex = "Mujer")
# mlt <-readHMDweb("ESP","mltper_1x1",
#                  username = Sys.getenv("us"), 
#                  password = Sys.getenv("pw"))|> 
#   clean_names() |> 
#   fsubset(year %in% c(2004:2020)) |> 
#   fmutate(sex = "Hombre") 
# 
# hmd_comparison <-
#   bind_rows(flt,mlt) |> 
#   fmutate(age = age - age %% 3) |> 
#   fgroup_by(sex,year,age) |> 
#   fsummarise(dx = sum(dx),
#              lx = sum(lx)) |> 
#   fmutate(qx = dx / lx,
#           source = "HMD") |> 
#   select(source, sex,year,age,qx) |> 
#   fsubset(year %in% seq(2005,2020,by=3)) |> 
#   fmutate(year = year - 1)
# hmd_comparison |> 
#   ggplot(aes(x=age,y=qx,color=year,group=year)) +
#   geom_line() +
#   scale_y_log10() +
#   facet_wrap(~sex) +
#   xlim(25,100)
# 
# qx_test <-
# qx_test |> 
#   fmutate(source = "MCVL") |> 
#   select(source, sex,year,age,qx)
# 
# 
#   bind_rows(hmd_comparison,
#             qx_test) |> 
#     ggplot(aes(x=age,y=qx,color = source)) +
#     geom_line()+
#     scale_y_log10()+
#     facet_grid(vars(year),vars(sex))
