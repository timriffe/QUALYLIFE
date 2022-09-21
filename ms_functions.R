library(expm)
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
  
  # for purposes of explicit unnesting...
states <- data_chunk$state_to %>% unique()

data_chunk %>% 
    group_by(state_from, state_to) %>% 
    nest() %>% 
    mutate(data = map(data,~.x %>% pi_block_from_chunk %>% mutate(age = 25:76))) %>% 
    pivot_wider(names_from = state_from, values_from = data)  %>% 
   # unnest()
    unnest(cols = states,
           names_sep = "") %>% 
    ungroup() %>% 
    rename(edad = Diedage) %>% 
    select(-ends_with("age")) %>% 
    mutate(state_to = paste(state_to, edad, sep = "_")) %>% 
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
make_ex <- function(data_chunk, init_lower=25, init_upper=29){
  
  U <- data_chunk %>% 
    # TR: this function is pure joy
    make_U()
  
  init <- rep(0, ncol(U))
  names(init) <-  colnames(U)
  
  init_entries <- data_chunk %>% 
    make_init(lower = init_lower, 
              upper = init_upper)

  # The only algebra we'll be seeing today...
  N <- solve(I - U)
  dimnames(N) <- dimnames(U)
  
  # The Dudel discount, always
  # worthy of an annotation
  N <- N - diag(N) / 2
  
  states <- 
    init_entries %>% 
    names() %>% 
    str_split(pattern = "_") %>% 
    lapply('[[',1) %>% 
    unlist()
  init <- tibble(init = init_entries, from = states)
  
  N %>% 
    as.data.frame() %>% 
    rownames_to_column("to") %>% 
    pivot_longer(-to, 
                 names_to = "from", 
                 values_to = "time") %>% 
    separate(col = "to", 
             sep = "_",
             into = c("to","age2"),
             convert = TRUE) %>% 
    separate(col = "from", 
             sep = "_",
             into = c("from","age1"),
             convert = TRUE) %>% 
    dplyr::filter(age1 == init_lower,
                  age2 >= age1,
                  from != "Died",
                  to != "Died") %>% 
    group_by(from, to) %>% 
    summarize(Ex_cond = sum(time), .groups = "drop") %>% 
    left_join(init, by = "from") %>% 
    mutate(Ex = Ex_cond * init) %>% 
    group_by(to) %>% 
    summarize(Ex = sum(Ex), .groups = "drop") %>% 
    rename(state = to) %>% 
    mutate(age = init_lower)
}

