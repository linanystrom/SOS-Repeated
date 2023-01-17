################################################################################

# SoS Repeated - Analysis

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "readxl", "ggplot2", "lme4", "lmerTest")

lapply(packages, library, character.only = TRUE)

my_df <- read_csv("./sim_repeated.csv") #Load data here

# Plots ------------------------------------------------------------------------

## Descriptives

info_desc <- my_df %>% 
  group_by(condition, stage, interview) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )

# Grid with plots for information disclosure over stages of interview (one plot per interview/condition combination).

plot_1 <- ggplot(
  my_df,
  aes(
    x=stage,
    y=detail,
    group=id)
) + 
  geom_line(
    position = position_jitter()
  ) +
  geom_line(
    data = info_desc,
    aes(
      x = stage,
      y = Mean,
      group = interview
    ),
    color = "deeppink2",
    linewidth = 1.5
  ) +
  facet_wrap(~ condition + interview, labeller = label_both)

# One plot per interview, displaying mean information disclosure for SoS and Direct.

plot_2 <- ggplot(
  my_df,
  aes(
    x=stage,
    y=detail,
    group=id,
    color=condition)
) + 
  geom_line(
    position = position_jitter(),
    color = "black"
  ) +
  geom_line(
    data = info_desc,
    aes(
      x = stage,
      y = Mean,
      group = condition
    ),
    linewidth = 1.5
  ) +
  facet_wrap(~interview, labeller = label_both)

# Grid displaying information disclosure over the three interviews for each mock crime stage. 

plot_3 <- ggplot(
  my_df,
  aes(
    x=interview,
    y=detail,
    group=id,
    color=condition
  )
) + 
  geom_line(
    position = position_jitter(),
    color = "black"
  ) +
  geom_line(
    data = info_desc,
    aes(
      x = interview,
      y = Mean,
      group = condition
    ),
    linewidth = 1.5
  ) +
  scale_x_continuous(breaks = c(0,1,2)) +
  facet_wrap(~stage, labeller = label_both)


# Information disclosure models ------------------------------------------------

# ICC Model - Unconditional mean model

icc_model <- lmer(detail
            ~ 1 
            + (1 |MC/id)
            + (1 |interviewer) #Have not simulated variances for interviewer yet
            + (1 |interview) 
            + (1 |stage),
            data=my_df,
            REML=TRUE)

summary(icc_model)

performance::icc(icc_model, by_group = TRUE)

# Simple effects, splines

info_simple <- lmer(detail 
            ~ 1 
            + interview 
            + start_slope 
            + end_slope 
            + condition 
            + (1 + start_slope + end_slope + interview | MC/id)
            + (1 |interviewer)
            + (1 | stage),
            data=my_df,
            REML=FALSE)

summary(info_simple)

#Interaction effects (2-way)

info_2_way <- lmer(detail 
            ~ 1 
            + interview 
            + start_slope 
            + end_slope 
            + condition 
            + interview*start_slope
            + interview*end_slope
            + interview*condition
            + start_slope*condition
            + end_slope*condition
            + (1 + start_slope + end_slope + interview | MC/id)
            + (1 |interviewer)
            + (1 |stage),
            data=my_df,
            REML=FALSE)

summary(info_2_way)

anova(info_simple,info_2_way, refit=FALSE)

#Interaction effects (3-way)

info_3_way <- lmer(detail 
            ~ 1 
            + interview 
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
            + (1 + start_slope + end_slope + interview | MC/id)
            + (1 |interviewer)
            + (1 | stage),
            data=my_df,
            REML=FALSE)

summary(info_3_way)

anova(info_2_way, info_3_way, refit=FALSE)

# Self-assessment of performance -----------------------------------------------

# ICC Model - Unconditional mean model

SA_icc_model <- lmer(self_assessment 
                  ~ 1 
                  + (1 |MC/id)
                  + (1 |interviewer) #Have not simulated variances for interviewer yet
                  + (1 |stage)
                  + (1 |interview),
                  data=my_df,
                  REML=TRUE)

summary(SA_icc_model)

performance::icc(SA_icc_model, by_group = TRUE)

## Main effects

self_simple <- lmer(self_assessment 
                    ~ 1 
                    + interview 
                    + condition 
                    + (1 + interview | MC/id),
                    data=my_df,
                    REML=FALSE)

summary(self_simple)

## Interaction effect

self_interaction <- lmer(self_assessment  
                   ~ 1 
                   + interview 
                   + condition 
                   + interview*condition
                   + (1 + interview | MC/id),
                   data=my_df,
                   REML=FALSE)

summary(self_interaction)

anova(self_simple, self_interaction, refit=FALSE)

# Interaction quality ----------------------------------------------------------

## Interview -------------------------------------------------------------------

# ICC Model - Unconditional mean model

QIW_icc_model <- lmer(qual_interview 
                     ~ 1 
                     + (1 |MC/id)
                     + (1 |interviewer) #Have not simulated variances for interviewer yet
                     + (1 |stage)
                     + (1 |interview),
                     data=my_df,
                     REML=TRUE)

summary(QIW_icc_model)

performance::icc(QIW_icc_model, by_group = TRUE)

## Main effects

qual_interview_simple <- lmer(qual_interview 
                    ~ 1 
                    + interview
                    + interview_sq
                    + condition 
                    + (1 + interview  + interview_sq| MC/id),
                    data=my_df,
                    REML=FALSE)

summary(qual_interview_simple)

## Interaction effect

qual_interview_interaction <- lmer(qual_interview 
                         ~ 1 
                         + interview 
                         + condition 
                         + interview*condition
                         + (1 + interview | MC/id),
                         data=my_df,
                         REML=FALSE)

summary(qual_interview_interaction)

anova(qual_interview_simple, qual_interview_interaction, refit=FALSE)

## Interviewer -----------------------------------------------------------------

# ICC Model - Unconditional mean model

QIR_icc_model <- lmer(qual_interviewer 
                      ~ 1 
                      + (1 |MC/id)
                      + (1 |interviewer) #Have not simulated variances for interviewer yet
                      + (1 |stage)
                      + (1 |interview),
                      data=my_df,
                      REML=TRUE)

summary(QIR_icc_model)

performance::icc(QIR_icc_model, by_group = TRUE)

## Main effects

qual_interviewer_simple <- lmer(qual_interviewer 
                              ~ 1 
                              + interview 
                              + condition 
                              + (1 + interview | MC/id),
                              data=my_df,
                              REML=FALSE)

summary(qual_interviewer_simple)

## Interaction effects

qual_interviewer_interaction <- lmer(qual_interviewer 
                                   ~ 1 
                                   + interview 
                                   + condition 
                                   + interview*condition
                                   + (1 + interview | MC/id),
                                   data=my_df,
                                   REML=FALSE)

summary(qual_interviewer_interaction)

anova(qual_interviewer_simple, qual_interviewer_interaction, refit=FALSE)

# Exploratory analyses ---------------------------------------------------------


## Predicting disclosed details by self_assessment -----------------------------

### Main effect model

expl_model_1 <- lmer(detail
                     ~ interview
                     + condition
                     + self_assessment
                     + (1 + interview | MC/id) 
                     + (1 | stage),
                     data=my_df,
                     REML=FALSE)

summary(expl_model_1)


### Interaction effect model

expl_model_2 <- lmer(detail
                     ~ interview
                     + condition
                     + self_assessment
                     + self_assessment*condition
                     + self_assessment*interview
                     + (1 + interview | MC/id) 
                     + (1 | stage),
                     data = my_df,
                     REML = FALSE
)

summary(expl_model_2)

comp_expl_model_anova <- anova(expl_model_1, expl_model_2)

### 3-way Interaction effect model

expl_model_3 <- lmer(detail
                     ~ interview
                     + condition
                     + self_assessment
                     + self_assessment*condition
                     + self_assessment*interview
                     + self_assessment*interview*condition
                     + (1 + interview | MC/id) 
                     + (1|stage),
                     data = my_df,
                     REML = FALSE
)

summary(expl_model_3)

comp_expl_model_anova_2 <- anova(expl_model_2, expl_model_3)
