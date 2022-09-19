
p<- D %>% 
  filter(year2 == 2006,
         sex == "Hombre",
         ocup_isco_h == "Routine",
         state_from == "Employed",
         state_to == "Employed") %>% 
  pull(p)

pi_block <- function(p, state_from, state_to, age = 25:75){
  state_from  <- state_from[1]
  state_to    <- state_to[1]
  
  age         <- c(age, max(age) + 1)
  P           <- diag(p)
  P           <- cbind(rbind(0, P), 0)
  from_names  <- paste(state_from, age, sep = "::")
  to_names    <- paste(state_to, age, sep = "::")
  dimnames(P) <- list(to_names, from_names)
  P
}
pi_block_from_chunk2 <- function(chunk){
  pi_block(chunk$p %>% as.double(),
           chunk$state_from,
           chunk$state_to,
           chunk$age_yr)  %>% 
    as.data.frame() 
}
pi_block_from_chunk(chunk) 
data <-D %>% 
  filter(year2 == 2006,
         sex == "Hombre",
         ocup_isco_h == "Routine")



make_U <- function(data, states = c("Employed", "Unemployed", "Inactive", "Disability", "Disability-retirement", "Retired", "`Died`")){
  fake_U <- matrix(ncol=52,nrow=52)
  matrix(states,length(states),length(states))
 
  n <- length(states)
  # nested_tibble_U <- tibble(state_to = states,
  #                           Employed = vector("list", n),
  #                           Unemployed =  vector("list", n),
  #                           Inactive =  vector("list", n),
  #                           Disability =  vector("list", n),
  #                           `Disability-retirement` =  vector("list", n),
  #                           Retired =  vector("list", n),
  #                           Died =  vector("list", n))
            
  my_data %>% 
                     group_by(state_from, state_to) %>% 
                     nest() %>% 
                     mutate(data = map(data,~.x %>% pi_block_from_chunk2)) %>% 
                     pivot_wider(names_from = state_from, values_from = data) %>% 
                     unnest() %>% colnames()
   
  for (from_state in states){
    # the outer loop cbinds from states
    
    for (to_state in states){
      # we'll do Caswell orientation
      
      # The inner loop appends block rows to a single
      # from state column
      this_p <- data %>% 
        filter(state_from == from_state,
               state_to == to_state) %>% 
        pull(p)
      this_P <- pi_block(p = this_p, 
                         state_from = from_state, 
                         state_to = to_state)
      nested_tibble_U
    }
  }
  
}