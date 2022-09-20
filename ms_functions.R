
# p<- D %>% 
#   filter(year2 == 2006,
#          sex == "Hombre",
#          ocup_isco_h == "Routine",
#          state_from == "Employed",
#          state_to == "Employed") %>% 
#   pull(p)

pi_block <- function(p, state_from, state_to, age = 25:7){
  state_fromi  <- state_from[1]
  state_toi    <- state_to[1]
  
  age         <- c(age, max(age) + 1)
  P           <- diag(p)
  P           <- cbind(rbind(0, P), 0)
  from_names  <- paste(state_fromi, age, sep = "_")
  to_names    <- paste(state_toi, age, sep = "_")
  dimnames(P) <- list(to_names, from_names)
 
  P
}
pi_block_from_chunk <- function(chunk){
  pi_block(chunk[["p"]] %>% as.double(),
           chunk[["state_from"]],
           chunk[["state_to"]],
           chunk[["age_yr"]])  %>% 
    # as.data.frame() %>% 
    # rownames_to_column(var = "_age") %>% 
    as_tibble()
}
# chunk %>% 
#   filter(ocup_isco_h == "Routine",
#          sex == "Hombre") %>% 
# pi_block_from_chunk2() 

# chunk %>% 
#   filter(ocup_isco_h == "Routine")
# data <-D %>% 
#   filter(year2 == 2006,
#          sex == "Hombre",
#          ocup_isco_h == "Routine")

# D %>% 
#   filter(year2 == 2006,
#          age_yr == 71,
#          sex == "Mujer",
#          ocup_isco_h == "Routine",
#          state_from == "Disability")

make_U <- function(data_chunk){

  data_chunk %>% 
    group_by(state_from, state_to) %>% 
    nest() %>% 
    mutate(data = map(data,~.x %>% pi_block_from_chunk %>% mutate(age = 25:76))) %>% 
    pivot_wider(names_from = state_from, values_from = data)  %>% 
    unnest(cols = c(Died, Disability, `Disability-retirement`, Employed, Inactive, 
                    Retired, Unemployed),
           names_sep = "") %>% 
    ungroup() %>% 
    rename(edad = Diedage) %>% 
    select(-ends_with("age")) %>% 
    mutate(state_to = paste(state_to, edad,sep = "_")) %>% 
    select(-edad) %>% 
    column_to_rownames("state_to") %>% 
    as.matrix()
}
# init <- rep(0, ncol(U))
# names(init) <-  colnames(U)

make_init <- function(data_chunk, lower = 25, upper = 29){
  data_chunk %>% 
    filter(between(age_yr,lower,upper)) %>% 
    group_by(state_from) %>% 
    summarize(init = sum(transitions),
              .groups = "drop") %>% 
    mutate(init = init / sum(init)) %>% 
    mutate(state_from = paste0(state_from,paste0("_",lower))) %>% 
    pull(init, state_from)
}
make_lxs <- function(data_chunk, init_lower, init_upper){
  
  U <- data_chunk %>% 
    make_U()
  
  init <- rep(0, ncol(U))
  names(init) <-  colnames(U)
  
  init_entries <- data_chunk %>% 
    make_init(lower = init_lower, 
              upper = init_upper)
  init[names(init_entries)] <- 1
  k<- length(init_entries)
  for (i in 1:k){
    kk <- names(init_entries)[k]
    U[kk,kk] <- 1#init_entries[k]
  }
  
  n <- nrow(U) / length(init_entries)
  # ones <- rep(1,length(init))
  l_out <- (U %^% (n-1) ) %*% init
  # I <- diag(rep(1,nrow(U)))
  # N <- solve(I  - U)
  # N %*% init
  
  any(colSums(U) > 1)
  # U2 <- U  %^% 2
  # U2[,"Employed_40"]
  dim(l_out) <- c(n,length(init_entries))
  l_out
  states <- 
    init_entries %>% 
    names() %>% 
    str_split(pattern = "_") %>% 
    lapply('[[',1) %>% 
    unlist()
  colnames(l_out) <- states
  
  
  l_out %>% 
    as_tibble() %>% 
    mutate(age = 25:76, .before = 1) %>% 
    pivot_longer(-age, names_to = "state_to", values_to = "lxs")
}
E <-
D %>% 
  group_by(ocup_isco_h,
           sex,
           year2) %>% 
  group_modify(~make_lxs(data_chunk = .x, init_lower = 25, init_upper = 29))

E %>% 
  filter(state_to != "Died",
         year2 < 2020) %>% 
  group_by(state_to, sex, ocup_isco_h,year2) %>% 
  summarize(ex = sum(lxs)) %>% 
  ggplot(aes(x = year2, y = ex, color = ocup_isco_h, linetype = sex)) +
  geom_line() +
  facet_wrap(~state_to, scales = "free_y")
