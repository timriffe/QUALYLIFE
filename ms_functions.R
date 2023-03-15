library(expm)
# p<- D |> 
#   filter(year2 == 2006,
#          sex == "Hombre",
#          ocup_isco_h == "Routine",
#          statefrom == "Employed",
#          stateto == "Employed") |> 
#   pull(p)

pi_block <- function(p, statefrom, stateto, age = 25:7){
  statefromi  <- statefrom[1]
  statetoi    <- stateto[1]
  
  age         <- c(age, max(age) + 1)
  P           <- diag(p)
  P           <- cbind(rbind(0, P), 0)
  from_names  <- paste(statefromi, age, sep = "_")
  to_names    <- paste(statetoi, age, sep = "_")
  dimnames(P) <- list(to_names, from_names)
 
  P
}
pi_block_from_chunk <- function(chunk){
  pi_block(chunk[["p"]] |> as.double(),
           chunk[["statefrom"]],
           chunk[["stateto"]],
           chunk[["age"]])  |> 
    as.data.frame() |> 
    rownames_to_column(var = "_age") |> 
    as_tibble()
}
# chunk |> 
#   filter(ocup_isco_h == "Routine",
#          sex == "Hombre") |> 
# pi_block_from_chunk2() 

# chunk |> 
#   filter(ocup_isco_h == "Routine")
# data <-D |> 
#   filter(year2 == 2006,
#          sex == "Hombre",
#          ocup_isco_h == "Routine")

# D |> 
#   filter(year2 == 2006,
#          age_yr == 71,
#          sex == "Mujer",
#          ocup_isco_h == "Routine",
#          statefrom == "Disability")

make_U <- function(data_chunk, lower = 25, upper = 75){
  
  # for purposes of explicit unnesting...
states <- data_chunk$statefrom |> unique()

data_chunk |> 
    filter(between(age,lower,upper)) |> 
    group_by(statefrom, stateto) |> 
    nest() |> 
    mutate(data = map(data,~.x |> 
                        pi_block_from_chunk() )) |> 
    pivot_wider(names_from = statefrom, values_from = data) |> 
    unnest(cols = all_of(states),
           names_sep = "") |> 
    ungroup() |> 
    mutate(edad = `Disability_age`,.before=2) |> 
    select(-ends_with("age")) |> 
      mutate(stateto = paste(stateto, edad, sep = "")) |> 
    select(-edad) |> 
    column_to_rownames("stateto") |> 
    as.matrix()
}
# init <- rep(0, ncol(U))
# names(init) <-  colnames(U)

make_init <- function(data_chunk, lower = 25, upper = 29){
  data_chunk |> 
    filter(between(age,lower,upper)) |> 
    group_by(statefrom) |> 
    summarize(init = sum(n),
              .groups = "drop") |> 
    mutate(init = init / sum(init)) |> 
    mutate(statefrom = paste0(statefrom,paste0("_",lower))) |> 
    pull(init, statefrom)
}


# starting from an initial data chunk (a unique combination
# of ocupation class, sex, year), we derive U, calculate N,
# and reshape to tidy. Initial conditions are derived from
# empirical prevalence in a specified age range.
make_ex <- function(data_chunk, 
                    lower = 25, 
                    upper = 75,
                    init_lower = lower, 
                    init_upper = lower + 4
                    ){
  
  U <- data_chunk |> 
    mutate(p = replace_na(p,0)) |> 
    # TR: this function is pure joy
    make_U(lower = lower, upper = upper)
  
  init <- rep(0, ncol(U))
  names(init) <-  colnames(U)
  
  init_entries <- data_chunk |> 
    make_init(lower = init_lower, 
              upper = init_upper)

  # The only algebra we'll be seeing today...
  I <- diag(rep(1,nrow(U)))
  N <- solve(I - U)
  dimnames(N) <- dimnames(U)
  
  # The Dudel discount, always
  # worthy of an annotation
  N <- N - I / 2
  
  states <- 
    init_entries |> 
    names() |> 
    str_split(pattern = "_") |> 
    lapply('[[',1) |> 
    unlist()
  init <- tibble(init = init_entries, from = states)
  
  N |> 
    as.data.frame() |> 
    rownames_to_column("to") |> 
    dt_pivot_longer(-to, 
                 names_to = "from", 
                 values_to = "time") |> 
    separate(col = "to", 
             sep = "_",
             into = c("to","age2"),
             convert = TRUE) |> 
    separate(col = "from", 
             sep = "_",
             into = c("from","age1"),
             convert = TRUE) |> 
    fsubset(age1 == init_lower&
                  age2 >= age1) |> 
    group_by(from, to) |> 
    summarize(Ex_cond = sum(time), .groups = "drop") |> 
    left_join(init, by = "from") |> 
    fmutate(Ex = Ex_cond * init) |> 
    group_by(to) |> 
    summarize(Ex = sum(Ex), .groups = "drop") |> 
    rename(state = to) |> 
    fmutate(age = init_lower)
}

