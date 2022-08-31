################################################################################

# Power Simulation - Data Setup

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("readr", "tibble", "dplyr", "data.table", "tidyr", "readxl", "ggplot2", "lme4", "simr")

lapply(packages, library, character.only = TRUE)

# Data structure ---------------------------------------------------------------

group_n      <- 40

n_phases     <- 4
n_interviews <- 3
n_time       <- n_phases * n_interviews

condition    <- c("direct", "standard", "delay")

phase     <- 0:(n_phases - 1)
interview <- 0:(n_interviews - 1)
time      <- 0:(n_time - 1)

structure <- expand.grid(phase = phase, interview = interview) %>% 
  cbind(data.frame(time))

structure_full <-  structure[rep(1:nrow(structure), group_n * length(condition)), ]

id <- sort(rep(1:(group_n * length(condition)), n_time))

structure_full$id          <- id
structure_full$condition   <- sort(rep(condition, group_n * n_time))
# structure_full$information <- sample(1:5, nrow(structure_full), replace = TRUE)
