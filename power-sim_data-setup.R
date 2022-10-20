################################################################################

# Power Simulation - Data Setup

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("readr",
              "tibble",
              "dplyr",
              "data.table",
              "tidyr",
              "readxl",
              "ggplot2",
              "lme4",
              "simr")

lapply(packages, library, character.only = TRUE)

# Data structure ---------------------------------------------------------------

group_n      <- 30

n_phases     <- 5
n_interviews <- 3
n_time       <- n_phases * n_interviews

condition    <- c("direct", "standard")

phase     <- 0:(n_phases - 1)
interview <- 0:(n_interviews - 1)
time      <- 0:(n_time - 1)

structure <- expand.grid(phase = phase, interview = interview) %>% 
  cbind(data.frame(time))

structure_full <-  structure[rep(1:nrow(structure), group_n * length(condition)), ]

id <- sort(rep(1:(group_n * length(condition)), n_time))

structure_full$id          <- id
structure_full$condition   <- sort(rep(condition, group_n * n_time))
structure_full$phase_sq    <- phase^2
structure_full$condition <- factor(structure_full$condition,
                                   levels = c("direct", "standard"))


# Effect size 0.30--------------------------------------------------------------

coeff_30 <- c("(Intercept)" = 1.34,
            "phase" = -0.45,
            "phase_sq" = -0.45,
            "interview" = 0.2,
            "conditionstandard" = 0.4,
            "phase:interview" = 0,
            "phase_sq:interview" = 0,
            "phase:conditionstandard" = -0.2,
            "phase_sq:conditionstandard" = -0.2,
            "interview:conditionstandard" = 0.2,
            "phase:interview:conditionstandard" = 0.3,
            "phase_sq:interview:conditionstandard" = 0.3)

sim_lmer_30 <- makeLmer(
  formula = y ~ phase
  + phase_sq
  + interview
  + interview*condition
  + phase*interview
  + phase_sq*interview
  + phase*condition
  + phase_sq*condition
  + phase*interview*condition
  + phase_sq*interview*condition
  + (1|id), 
  fixef = coeff_30,
  VarCorr = 1.6,
  sigma = 1.2,
  data = structure_full
)

sim_30 <- powerSim(sim_lmer_30,
                   test = fixed("phase_sq:interview:conditionstandard",
                   method = "t"),
                   nsim = 1000)

save(sim_30, file = "sim_sos_30.rda")


# Effect size 0.20--------------------------------------------------------------

coeff_20 <- c("(Intercept)" = 1.34,
              "phase" = -0.45,
              "phase_sq" = -0.45,
              "interview" = 0.2,
              "conditionstandard" = 0.4,
              "phase:interview" = 0,
              "phase_sq:interview" = 0,
              "phase:conditionstandard" = -0.2,
              "phase_sq:conditionstandard" = -0.2,
              "interview:conditionstandard" = 0.2,
              "phase:interview:conditionstandard" = 0.3,
              "phase_sq:interview:conditionstandard" = 0.2)

sim_lmer_20 <- makeLmer(
  formula = y ~ phase
  + phase_sq
  + interview
  + interview*condition
  + phase*interview
  + phase_sq*interview
  + phase*condition
  + phase_sq*condition
  + phase*interview*condition
  + phase_sq*interview*condition
  + (1|id), 
  fixef = coeff_20,
  VarCorr = 1.6,
  sigma = 1.2,
  data = structure_full
)

sim_20 <- powerSim(sim_lmer_20,
                   test = fixed("phase_sq:interview:conditionstandard",
                   method = "t"),
                   nsim = 1000)

save(sim_20, file = "sim_sos_20.rda")


# Effect size 0.15--------------------------------------------------------------

coeff_15 <- c("(Intercept)" = 1.34,
              "phase" = -0.45,
              "phase_sq" = -0.45,
              "interview" = 0.2,
              "conditionstandard" = 0.4,
              "phase:interview" = 0,
              "phase_sq:interview" = 0,
              "phase:conditionstandard" = -0.2,
              "phase_sq:conditionstandard" = -0.2,
              "interview:conditionstandard" = 0.2,
              "phase:interview:conditionstandard" = 0.3,
              "phase_sq:interview:conditionstandard" = 0.15)

sim_lmer_15 <- makeLmer(
  formula = y ~ phase
  + phase_sq
  + interview
  + interview*condition
  + phase*interview
  + phase_sq*interview
  + phase*condition
  + phase_sq*condition
  + phase*interview*condition
  + phase_sq*interview*condition
  + (1|id), 
  fixef = coeff_15,
  VarCorr = 1.6,
  sigma = 1.2,
  data = structure_full
)

sim_15 <- powerSim(sim_lmer_15,
                   test = fixed("phase_sq:interview:conditionstandard",
                   method = "t"),
                   nsim = 1000)

save(sim_15, file = "sim_sos_15.rda")