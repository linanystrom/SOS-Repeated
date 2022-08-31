################################################################################

# Power Simulation - Data Setup

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("readr", "tibble", "dplyr", "data.table", "tidyr", "readxl", "ggplot2", "lme4", "simr")

lapply(packages, library, character.only = TRUE)

# Data structure ---------------------------------------------------------------

group_n      <- 60

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
structure_full$phase_sq    <- phase^2
structure_full$condition <- factor(structure_full$condition, levels = c("direct", "standard", "delay"))
# structure_full$information <- sample(1:5, nrow(structure_full), replace = TRUE)

coeff <- c( "(Intercept)" = 1.34,
            "phase" = -0.45,
            "phase_sq" = -0.45,
            "interview" = 0.2,
            "conditionstandard" = 0.4,
            "conditiondelay" = 0,
            "phase:interview" = 0,
            "phase_sq:interview" = 0,
            "phase:conditionstandard" = -0.2,
            "phase_sq:conditionstandard" = -0.2,
            "phase:conditiondelay" = 0, 
            "phase_sq:conditiondelay" = 0,
            "interview:conditionstandard" = 0.2,
            "interview:conditiondelay" = 0.1,
            "phase:interview:conditionstandard" = 0.3,
            "phase_sq:interview:conditionstandard" = 0.3,
            "phase:interview:conditiondelay" = 0.3,
            "phase_sq:interview:conditiondelay" = 0.3)


sim_lmer <- makeLmer(
  formula = y ~ phase + phase_sq + interview + interview*condition + phase*interview + phase_sq*interview + phase*condition + phase_sq*condition + phase*interview*condition + phase_sq*interview*condition + (1|id), 
  fixef = coeff,
  VarCorr = 1.2,
  sigma = 1.6,
  data = structure_full
)

sim_1 <- powerSim(sim_lmer, test = fixed("phase_sq:interview:conditionstandard", method = "t"), nsim = 1000)

save(sim_1, file = "sim_sos.rda")