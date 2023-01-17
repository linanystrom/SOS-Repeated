################################################################################

# LRT Power Simulation 

################################################################################

# Set up environment -----------------------------------------------------------

## Packages

packages <- c("gtools",
              "readr",
              "tibble",
              "dplyr",
              "data.table",
              "tidyr",
              "simr",
              "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(666)


# Set up design-----------------------------------------------------------------

nr_mcs    <- 3                   #Number of mock crimes
nr_stages <- 5                   #Number of stages per mock crime
nr_cond   <- 2                   #Number of conditions
nr_int    <- 3                   #Number of interviews
nr_intw   <- 6                   #Number of interviewers
poss_det  <- 5                   #Possible details disclosed
group_sz  <- 30                  #Group size
poss_comb <- nr_mcs*nr_cond      #Possible MC*Condition combinations
nr_time   <- nr_stages * nr_int  #Total data points per participant
sample    <- group_sz*nr_cond    #Sample size

MC          <- c("MC1","MC2","MC3")
cond        <- c("direct", "sos")
interviewer <- c("H", "K", "M", "S", "N", "F")

stage     <- 0:(nr_stages - 1)
interview <- 0:(nr_int  - 1)
time      <- 0:(nr_time - 1)
detail    <- 0:(poss_det)
id_mc     <- 1:sample


# Set up basic data structure---------------------------------------------------


df <- expand.grid(stage = stage,interview = interview) %>% 
  cbind(data.frame(time))

df_full <- df[rep(1:nrow(df), group_sz * length(cond)), ]

id <- sort(rep(1:(group_sz * length(cond)), nr_time))

df_full$id <- id

df_full$condition <- sort(rep(cond, group_sz * nr_time))

df_full <- df_full %>% 
  select(id, everything())

mc <- expand.grid(id = id_mc)

mc$MC <- sample(MC, nrow(mc), replace = TRUE)

mc$interviewer <- sample(interviewer, nrow(mc), replace = TRUE)

complete_df <- left_join(df_full, mc, by ='id')

complete_df$condition <- factor(complete_df$condition,
                                levels = c("direct", "sos"))

complete_df$stage_sq    <- stage^2


# Create start & end slope -----------------------------------------------------

complete_df <- complete_df %>% 
  mutate(start_slope = ifelse(interview == 0, case_when(
    stage == 0 ~ 0,
    stage == 1 ~ 0,
    stage == 2 ~ 0,
    stage == 3 ~ 0,
    stage == 4 ~ 0), 
    ifelse(interview == 1, case_when(
      stage == 0 ~ 0,
      stage == 1 ~ 1,
      stage == 2 ~ 1,
      stage == 3 ~ 1,
      stage == 4 ~ 1), 
      ifelse(interview == 2, case_when(
        stage == 0 ~ 0,
        stage == 1 ~ 1,
        stage == 2 ~ 2,
        stage == 3 ~ 2,
        stage == 4 ~ 2), NA
      ))))

complete_df <- complete_df %>% 
  mutate(end_slope = ifelse(interview == 0, case_when(
    stage == 0 ~ 0,
    stage == 1 ~ 1,
    stage == 2 ~ 2,
    stage == 3 ~ 3,
    stage == 4 ~ 4), 
    ifelse(interview == 1, case_when(
      stage == 0 ~ 0,
      stage == 1 ~ 0,
      stage == 2 ~ 1,
      stage == 3 ~ 2,
      stage == 4 ~ 3), 
      ifelse(interview == 2, case_when(
        stage == 0 ~ 0,
        stage == 1 ~ 0,
        stage == 2 ~ 0,
        stage == 3 ~ 1,
        stage == 4 ~ 2), NA
      ))))



# Set up coefficients ----------------------------------------------------------

coeff <- c( "(Intercept)"                         =  1.34,
            "interview"                           =  0.20,
            "start_slope"                         =  0.00,
            "end_slope"                           = -0.25,
            "conditionsos"                        =  0.85,
            "interview:start_slope"               =  0.10,
            "interview:end_slope"                 = -0.20,
            "interview:conditionsos"              =  0.20,
            "start_slope:conditionsos"            =  0.25,
            "end_slope:conditionsos"              = -0.20,
            "interview:start_slope:conditionsos"  =  0.10,
            "interview:end_slope:conditionsos"    =  0.00)

variances <- list(1.53130, 0.04062, 0.00000)


sim_detail_lmer <- makeLmer(
  formula = detail 
  ~ interview 
  + start_slope 
  + end_slope 
  + condition 
  + interview*start_slope
  + interview*end_slope
  + interview*condition
  + start_slope*condition
  + end_slope*condition
  + interview*condition*start_slope # effect of interest
  + interview*condition*end_slope   # effect of interest
  + (1|MC/id)
  + (1|interviewer), 
  fixef = coeff,
  VarCorr = variances,
  sigma = 1.2,
  data = complete_df
)

detail_data <- getData(sim_detail_lmer)

# LRT power simulation ---------------------------------------------------------

sims <- 1000

p_vec <- rep(NA, sims)

p_vec_main <- rep(NA, sims)

for (i in 1:sims) {
  
  sim_detail_lmer <- makeLmer(
    formula = detail 
    ~ interview 
    + start_slope 
    + end_slope 
    + condition 
    + interview*start_slope
    + interview*end_slope
    + interview*condition
    + start_slope*condition
    + end_slope*condition
    + interview*condition*start_slope 
    + interview*condition*end_slope   
    + (1|MC/id)
    + (1|interviewer), 
    fixef = coeff,
    VarCorr = variances,
    sigma = 1.2,
    data = complete_df
  )
  
  detail_data <- getData(sim_detail_lmer)
  
  main_eff <- lmer(
    formula = detail 
    ~ interview 
    + start_slope 
    + end_slope 
    + condition 
    + (1|MC/id)
    + (1|interviewer),
    data = detail_data,
    REML = FALSE
  )
  
  inter_eff <- lmer(
    formula = detail 
    ~ interview 
    + start_slope 
    + end_slope 
    + condition 
    + interview*start_slope
    + interview*end_slope
    + interview*condition
    + start_slope*condition
    + end_slope*condition
    + interview*condition*start_slope 
    + interview*condition*end_slope   
    + (1|MC/id)
    + (1|interviewer),
    data = detail_data,
    REML = FALSE
  )
  
  lrt <- anova(main_eff, inter_eff, test = "LRT")
  
  p_vec[i] <- lrt$`Pr(>Chisq)`[[2]]
  
  p_vec_main[i] <- summary(main_eff)$coefficients[5,5]
  
}

sum(p_vec < .05)/sims # Power

sum(p_vec_main < .05)/sims # Power condition

write.csv(
  detail_data,
  "./sim_repeated_2.csv",
  row.names = FALSE
)

