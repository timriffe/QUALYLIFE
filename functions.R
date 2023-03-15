
# pacman will help us get everything installed
if (!"pacman" %in% rownames(installed.packages())){
  install.packages("pacman")
}
library(pacman)
CRAN_packages <- c("haven","tidyverse","lubridate","tictoc","multidplyr","readr","ggridges")

# Install required CRAN packages if not available yet
if(!sum(!p_isinstalled(CRAN_packages))==0) {
  p_install(
    package = CRAN_packages[!p_isinstalled(CRAN_packages)], 
    character.only = TRUE
  )
}

# install from github 
if (!p_isinstalled("Spells")) {
  remotes::install_github("timriffe/Spells/R/Spells", build = FALSE)
}

# Load the required CRAN/github packages
p_load(CRAN_packages, character.only = TRUE)
library(Spells)

# TODO create new category called "long term unemployment",
# 1) spells of unempl > 1 year
# 2) spells of unempl followed by inactivity > 1 year total
# (same thing in 2 steps: first merge then check)
# unemp causa: 
# 54 benefit ran up; 
# 51 voluntary stop; 
# 65 temporary sick/ disabled
my_date_diff <- function(date_earlier, date_later){
  date_later[is.na(date_later)] <- lubridate::dmy("01.01.2021")
  lubridate::time_length(interval(date_earlier,date_later),unit="years")
}

# we should delete unemployment that is within a work spell.
delete_contained_unemp <- function(X){
  # cases of this are prevalent in 2021 and 2020,
  # interesting in general, but not for this study.
  X %>% 
    # sort episodes
    arrange(alta) %>% 
    mutate(
      baja = if_else(causa == 98, dmy("01.01.2021"), baja),
      last_baja1 = lag(baja),
      last_baja2 = lag(baja,2),
      last_baja3 = lag(baja,3),
      last_baja4 = lag(baja,4),
      last_baja5 = lag(baja,5),
      last_baja6 = lag(baja,6),
      last_baja7 = lag(baja,7),
      last_baja8 = lag(baja,8),
      max_earlier_baja = pmax(last_baja1,last_baja2,last_baja3,last_baja4,last_baja5,last_baja6,last_baja7,last_baja8,na.rm=TRUE)) %>% 
    # we know last_alta < alta, but if 
    dplyr::filter(temp == "unemp",
                  baja > max_earlier_baja) %>% 
    select(any_of(colnames(X))) %>% 
    bind_rows(dplyr::filter(X, temp != "unemp")) %>% 
    arrange(alta)
}
# X <- LC2 %>% filter(IPF == 991)

pad_deaths <- function(X){
  death_date <- X$dod[1]
  X <- X %>% 
    dplyr::filter(duration > 0)
  if (is.na(death_date) & any(X$causa == 56)){
    death_date <- X %>% 
      dplyr::filter(causa == 56) %>% 
      pull(baja) %>% 
      '['(1) 
  }
  if (!is.na(death_date)){
    death_row <- 
      X %>% 
      slice(n()) %>% 
      mutate(dod = death_date,
             alta = death_date,
             baja = death_date + 1,
             duration = my_date_diff(alta,baja),
             temp = "dead")
    # any later events?
    X <-
      X %>% 
      mutate(alta = if_else(alta >= death_date, death_date, alta),
             baja = if_else(baja >= death_date, death_date, baja),
             duration = my_date_diff(alta, baja))
    
    if (sum(X$duration == 0) > 0){
      IPF <- X$IPF[1]
      warning(IPF, "has events after death\nthese are discarded")
    }
    
    X <-
      X %>% 
      dplyr::filter(duration > 0) %>% 
      bind_rows(death_row)
  }
  X
}



pad_inactivity <- function(X){
  INAC <- 
    X %>% 
    # sort episodes
    arrange(alta) %>% 
    # detect gaps between episodes based on lag between 
    # current baja and next alta. Positives are gaps.
    # negatives are overlapping episodes which we don't handle here
    mutate(# gap = lead(alta) - baja,
      # test
           # gap = time_length(interval(baja,lead(alta)),unit="years"),
           gap = my_date_diff(date_earlier = baja, date_later = lead(alta)),
           next_alta = baja + gap,
           next_CCC = lead(CCC)) %>% 
    # only keep breaks with positive durations
    dplyr::filter(gap > 0) 
  if(nrow(INAC) > 0){
    INAC <- 
      INAC %>% 
      mutate(alta = baja,
             baja = next_alta,
             # if it's a within-company break, then keep company id
             CCC = ifelse(CCC == next_CCC, CCC, "999"),
             part = "full",
             sector = "inactivity",
             occ = "inactivity",
             # assign inactivity state based on "cause" of termination 
             # of last episode.
             temp = case_when(
               causa == 58 ~ "retirement",
               causa %in% c(68,73) ~ "care gap",
               causa == 56 ~ "dead",
               causa %in% c(51,52,54,55,60,65,69,74,77,91,92,93,94) ~ "labor gap",
               causa == 99 ~ "other"
             ),
             duration = gap,
             age_at_start = my_date_diff(date_earlier = dob, date_later = alta))
  }
  
  INAC %>% 
    select(any_of(colnames(X))) %>% 
    # append to incoming data
    bind_rows(X) %>% 
    arrange(alta)
}

# long term unemployment is coded after inactivity has been padded in
merge_ltu <- function(X){
  in_cols <- colnames(X)
  X_aug <-
    X %>% 
    arrange(alta) %>% 
    mutate(mergeable_maybe = temp %in% c("unemp", "no consta", "labor gap","other")) %>% 
    group_by(yy = {yy = rle(mergeable_maybe); rep(seq_along(yy$lengths), yy$lengths)}) %>% 
    mutate(has_unemp = any(temp == "unemp"),
           duration_merge = sum(duration)) %>% 
    ungroup() %>% 
    select(-mergeable_maybe) %>% 
    mutate(merge_these = has_unemp & duration_merge > 1)
  
  if (any(X_aug$merge_these)){
  ltu <- X_aug %>% 
    dplyr::filter(merge_these) %>% 
    group_by(yy) %>% 
    mutate(temp = "ltu",
           alta = min(alta),
           baja = max(baja),
           duration = sum(duration),
           sector = "unemployed",
           part = "full",
           occ = occ[occ!="inactivity"] %>% table() %>% sort(decreasing = TRUE) %>% names() %>% '['(1)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(any_of(in_cols)) 
  
  X_out <- 
    X_aug %>% 
    dplyr::filter(!merge_these) %>% 
    select(any_of(in_cols)) %>% 
    bind_rows(ltu) %>% 
    arrange(alta)
  } else {
    X_out <- X
  }
  X_out
}

# TODO
# This is the function that determines the state for each uniform discrete time step
# it needs a new states called "precarious" and "instability" or similar, defined on the basis
# instability: nr transitions
# precarious: short employment, under full time, unskilled, low pay
# long term unemployment
discretize_trajectory <- function(X, step_size = 1/4){
  
  end   <- X %>% 
    pull(baja) %>% 
    max() %>% 
    '*'(1/step_size) %>% 
    ceiling() %>% 
    '*'(step_size)
  
  start <- X %>% 
    pull(alta) %>% 
    min() %>% 
    '*'(1/step_size) %>% 
    floor() %>% 
    '*'(step_size)
  
  starts <- seq(start, end-step_size, by = step_size)
  
  Y <-
    tibble(IPF = X$IPF[1],
           dob = X$dob[1],
           sex = X$sex[1],
           dod = X$dod[1],
           start = starts,
           occ = NA_character_,
           temp = NA_character_,
           part = NA_character_,
           sector = NA_character_) %>% 
    mutate(end = start + step_size, .after = start)
  
  for (i in 1:nrow(Y)){
    XX <- X %>% 
      mutate(left_overlap = between(alta, Y$start[i], Y$end[i]),
             right_overlap = between(baja, Y$start[i], Y$end[i]),
             bin_within_episode = alta < Y$start[i] & baja > Y$end[i],
             episode_within_bin = Y$start[i] < alta & Y$end[i] > baja) %>% 
      dplyr::filter(left_overlap |
                      right_overlap |
                      bin_within_episode |
                      episode_within_bin) %>% 
      mutate(alta = ifelse(alta < Y$start[i], Y$start[i], alta),
             baja = ifelse(baja > Y$end[i], Y$end[i], baja),
             duration = baja - alta)
    
    if (any(XX$temp == "dead")){
      Y$occ[i]    <- "dead"
      Y$temp[i]   <- "dead"
      next
    }
    # case 1, if there is one and only one row with temp == perm,
    # we keep its information and toss the rest
    if (any(XX$temp == "perm")){
      XX <- XX %>% dplyr::filter(temp == "perm")
      
      # if there's more than 1 permanent job in the interval (really?), 
      # then keep the one that was in the interval for the longest
      if (nrow(XX) > 1){
        XX <-
          XX %>% 
          dplyr::filter(duration == max(duration))
      }
    }
    
    # case 2 if some episode takes up > .5 bin width, then we take its characteristics
    if (any(XX$duration > (step_size / 2))){
      XX <-
        XX %>% 
        dplyr::filter(duration == max(duration))
    }
    
    # case 3, if there is only one intersecting episode,
    # we carry over its info.
    if (nrow(XX) == 1){
      Y$occ[i]    <- XX$occ
      Y$temp[i]   <- XX$temp
      Y$part[i]   <- XX$part
      Y$sector[i] <- XX$sector
      next
    }
    
    # case 4 
    Y$occ[i] <- XX %>% 
      group_by(occ) %>% 
      summarize(duration = sum(duration)) %>% 
      dplyr::filter(duration == max(duration)) %>% 
      dplyr::pull(occ) %>% 
      first()
    Y$temp[i] <- XX %>% 
      group_by(temp) %>% 
      summarize(duration = sum(duration)) %>% 
      dplyr::filter(duration == max(duration)) %>% 
      dplyr::pull(temp) %>% 
      first()
    Y$part[i] <- XX %>% 
      group_by(part) %>% 
      summarize(duration = sum(duration)) %>% 
      dplyr::filter(duration == max(duration)) %>% 
      dplyr::pull(part) %>% 
      first()
    Y$sector[i] <- XX %>% 
      group_by(sector) %>% 
      summarize(duration = sum(duration)) %>% 
      dplyr::filter(duration == max(duration)) %>% 
      dplyr::pull(sector) %>% 
      first()
    next
  }
  Y
}


# helper for conceptualizing, uses exact dates;
# still needs color for "ltu"
plot_traj_1 <- function(X){
  X %>% 
    group_by(IPF) %>% 
    mutate(first = min(alta)) %>% 
    ungroup() %>% 
    arrange(first, alta) %>% 
    group_by(first, IPF) %>% 
    mutate(ymin = cur_group_id() - 1,
           ymax = ymin + 1) %>%  
    ggplot(aes(xmin = alta, 
               xmax = baja, 
               ymin = ymin, 
               ymax = ymax, 
               fill = temp))  +
    geom_rect() +
    scale_fill_manual(values =c("no consta" = gray(.8), 
                                "labor gap" = "#ebc567", 
                                "unemp" = "#cf8908", 
                                "perm" = "#a368a8",
                                "temp" = "#ed6639",
                                "other" = "#b9c97f",
                                "retirement" = "#9ed9d4",
                                "care gap" = "#1952a8",
                                "dead" = "red"))
}
# same thing, but uses discretized state steps
# still needs color for "ltu"
plot_traj_2 <- function(X){
  X %>% 
    group_by(IPF) %>% 
    mutate(first = min(start)) %>% 
    ungroup() %>% 
    arrange(first, start) %>% 
    group_by(first, IPF) %>% 
    mutate(ymin = cur_group_id() - 1,
           ymax = ymin + 1) %>%  
    ggplot(aes(xmin = start, 
               xmax = end, 
               ymin = ymin, 
               ymax = ymax, 
               fill = temp))  +
    geom_rect() +
    scale_fill_manual(values =c("no consta" = gray(.8), 
                        "labor gap" = "#ebc567", 
                        "unemp" = "#cf8908", 
                        "perm" = "#a368a8",
                        "temp" = "#ed6639",
                        "other" = "#b9c97f",
                        "retirement" = "#9ed9d4",
                        "care gap" = "#1952a8",
                        "dead" = "red"))
}
