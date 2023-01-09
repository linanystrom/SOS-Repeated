################################################################################

# SoS Repeated - Analysis

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "readxl", "ggplot2", "lme4")

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

# Simple effects, splines

info_simple <- lmer(detail 
            ~ 1 
            + interview 
            + start_slope 
            + end_slope 
            + condition 
            + (1 + start_slope + end_slope + interview | MC/id) 
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
            + (1|stage),
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
            + (1 | stage),
            data=my_df,
            REML=FALSE)

summary(info_3_way)

anova(info_2_way, info_3_way, refit=FALSE)

# Self-assessment of performance -----------------------------------------------

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

qual_interview_simple <- lmer(qual_interview 
                    ~ 1 
                    + interview 
                    + condition 
                    + (1 + interview | MC/id),
                    data=my_df,
                    REML=FALSE)

summary(qual_interview_simple)

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

qual_interviewer_simple <- lmer(qual_interviewer 
                              ~ 1 
                              + interview 
                              + condition 
                              + (1 + interview | MC/id),
                              data=my_df,
                              REML=FALSE)

summary(qual_interviewer_simple)

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
                     + stage
                     + interview
                     + condition
                     + self_assessment
                     + (1 + interview | MC/id) 
                     + (1 | stage),
                     data=my_df,
                     REML=FALSE)

summary(expl_model_1)


### Interaction effect model

expl_model_2 <- lmer(detail
                     ~ stage
                     + interview
                     + condition
                     + self_assessment
                     + self_assessment*condition
                     + self_assessment*interview
                     + (1|crime_order:ID) 
                     + (1|stage),
                     data = my_df,
                     REML = FALSE
)

summary(expl_model_2)

comp_expl_model_anova <- anova(expl_model_1, expl_model_2)

### 3-way Interaction effect model

expl_model_3 <- lmer(detail
                     ~ stage
                     + interview
                     + condition
                     + self_assessment
                     + self_assessment*condition
                     + self_assessment*interview
                     + self_assessment*interview*condition
                     + (1|crime_order:ID) 
                     + (1|stage),
                     data = my_df,
                     REML = FALSE
)

summary(expl_model_3)

comp_expl_model_anova_2 <- anova(expl_model_2, expl_model_3)
