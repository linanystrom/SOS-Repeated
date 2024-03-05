################################################################################

# SoS Repeated - Analysis

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "readxl", "ggplot2", "lme4", "lmerTest")

lapply(packages, library, character.only = TRUE)

my_df <- read_csv("data/sos_rep_long.csv") #Load data here

# Plots ------------------------------------------------------------------------

plot_df <- my_df


## Factor training & details (critical, noncritical)

plot_df$style <- factor(plot_df$style)

plot_df$critical <- factor(plot_df$critical)

## Descriptives

info_desc <- plot_df %>% 
  group_by(style, interview, activity) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )

## Descriptives - critical

crit_desc <- plot_df %>% 
  group_by(style, interview, critical) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )


mean_plot <- ggplot(crit_desc, aes(
  x = interview,
  y = Mean,
  colour = style,
  group = style)
  ) + 
  geom_errorbar(aes(
    ymin = Lower,
    ymax = Upper),
    width = .1,
    position = position_dodge(width = .2)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(width = .2)
  ) +
  geom_line(
    
  ) +
  labs(
    y = "Disclosed details",
    x = "Detail type",
    color = "Condition"
  ) +
  facet_wrap(
    ~ critical
  )

# Grid with plots for information disclosure over stages of interview (one plot per interview/style combination).

plot_1_desc <- plot_df %>% 
  group_by(style, interview, activity) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )

plot_1 <- ggplot(
  plot_df,
  aes(
    x=activity,
    y=detail,
    group=id)
) + 
  geom_line(
    position = position_jitter()
  ) +
  geom_line(
    data = plot_1_desc,
    aes(
      x = activity,
      y = Mean,
      group = interview
    ),
    color = "deeppink2",
    linewidth = 1.5
  ) +
  facet_wrap(~ style + interview, labeller = label_both)

# One plot per interview, displaying mean information disclosure for SoS and Direct.

interview.labs <- c("Interview 1", "Interview 2", "Interview 3")
names(interview.labs) <- c("0", "1", "2")

plot_df$style <- factor(plot_df$style,
                               levels = c("direct", "sos"),
                               labels = c("Direct","SoS"))

plot_2 <- ggplot(
  plot_df,
  aes(
    x=activity,
    y=detail,
    color=style,
    group = id)
) + 
  geom_line(
    position = position_jitter(),
    color = "grey"
  ) +
  geom_line(
    data = plot_1_desc,
    aes(
      x = activity,
      y = Mean,
      group = style
    ),
    linewidth = 1.5
  ) +
  labs(
    y = "Information disclosure",
    x = "Activity",
    color = "Condition"
  )+  
  facet_wrap(~interview, labeller = labeller(interview = interview.labs)) +
  scale_x_continuous(
    labels = c("1", "2", "3","4","5"),
    breaks = 0:4) +
  ylim(0, 5
  ) 


# Information disclosure models ------------------------------------------------

# ICC Model - Unconditional mean model

icc_model <- lmer(detail
            ~ 1 
            + (1 |mc/id)
            + (1 |interviewer) #Have not simulated variances for interviewer yet
            + (1 |interview) 
            + (1 |activity),
            data=my_df,
            REML=TRUE)

summary(icc_model)

performance::icc(icc_model, by_group = TRUE)

# We begin by testing a model without random slopes

info_no_rslope <- lmer(detail 
                    ~ 1 
                    + interview 
                    + start_slope 
                    + end_slope 
                    + style 
                    + (1 | id)
                    + (1 |interviewer),
                    data=my_df,
                    REML=FALSE)

summary(info_no_rslope)

# Simple effects, splines

info_simple <- lmer(detail 
            ~ 1 
            + interview 
            + start_slope 
            + end_slope 
            + style 
            + (1 + start_slope + end_slope + interview | id)
            + (1 |interviewer),
            data=my_df,
            REML=FALSE)

summary(info_simple)

# Depending on the outcome of test below we will either retain or omit random slopes in the subsequent models. 

anova(info_no_rslope,info_simple, refit=FALSE)

#Interaction effects (2-way)

info_2_way_slopes <- lmer(detail 
                   ~ 1 
                   + interview 
                   + start_slope 
                   + end_slope 
                   + style 
                   + interview*start_slope
                   + interview*end_slope
                   + interview*style
                   + start_slope*style
                   + end_slope*style
                   + (1 + start_slope + end_slope + interview | id)
                   + (1 |interviewer),
                   data=my_df,
                   REML=FALSE)

summary(info_2_way_slopes)


anova(info_simple,info_2_way_slopes, refit=FALSE)

#Interaction effects (3-way)

info_3_way <- lmer(detail 
            ~ 1 
            + interview 
            + start_slope 
            + end_slope 
            + style 
            + interview*start_slope
            + interview*end_slope
            + interview*style
            + start_slope*style
            + end_slope*style
            + interview*style*start_slope 
            + interview*style*end_slope   
            + (1 + start_slope + end_slope + interview |id)
            + (1 |interviewer),
            data=my_df,
            REML=FALSE)

summary(info_3_way)

anova(info_2_way_slopes, info_3_way, refit=FALSE)

# Self-assessment of performance -----------------------------------------------

# ICC Model - Unconditional mean model

SA_icc_model <- lmer(self_assessment 
                  ~ 1 
                  + (1 |mc/id)
                  + (1 |interviewer) #Have not simulated variances for interviewer yet
                  + (1 |activity)
                  + (1 |interview),
                  data=my_df,
                  REML=TRUE)

summary(SA_icc_model)

performance::icc(SA_icc_model, by_group = TRUE)

## Main effects

self_simple <- lmer(self_assessment 
                    ~ 1 
                    + interview 
                    + style 
                    + (1 + interview | id)
                    + (1|interviewer),
                    data=my_df,
                    REML=FALSE)

summary(self_simple)

## Interaction effect

self_interaction <- lmer(self_assessment  
                   ~ 1 
                   + interview 
                   + style 
                   + interview*style
                   + (1 + interview | id)
                   + (1|interviewer),
                   data=my_df,
                   REML=FALSE)

summary(self_interaction)

anova(self_simple, self_interaction, refit=FALSE)

# Interaction quality ----------------------------------------------------------

## Interview -------------------------------------------------------------------

# ICC Model - Unconditional mean model

QIW_icc_model <- lmer(interview_qual 
                     ~ 1 
                     + (1 |mc/id)
                     + (1 |interviewer) 
                     + (1 |activity)
                     + (1 |interview),
                     data=my_df,
                     REML=TRUE)

summary(QIW_icc_model)

performance::icc(QIW_icc_model, by_group = TRUE)

## Main effects

qual_interview_simple <- lmer(interview_qual
                    ~ 1 
                    + interview
                    + style 
                    + (1 + interview| id)
                    + (1|interviewer),
                    data=my_df,
                    REML=FALSE)

summary(qual_interview_simple)

## Interaction effect

qual_interview_interaction <- lmer(interview_qual
                         ~ 1 
                         + interview 
                         + style 
                         + interview*style
                         + (1 + interview | id)
                         + (1|interviewer),
                         data=my_df,
                         REML=FALSE)

summary(qual_interview_interaction)

anova(qual_interview_simple, qual_interview_interaction, refit=FALSE)

## Interviewer -----------------------------------------------------------------

# ICC Model - Unconditional mean model

QIR_icc_model <- lmer(interviewer_qual 
                      ~ 1 
                      + (1 |id)
                      + (1 |interviewer) #Have not simulated variances for interviewer yet
                      + (1 |interview),
                      data=my_df,
                      REML=TRUE)

summary(QIR_icc_model)

performance::icc(QIR_icc_model, by_group = TRUE)

## Main effects

qual_interviewer_simple <- lmer(interviewer_qual  
                              ~ 1 
                              + interview 
                              + style 
                              + (1 + interview |id)
                              + (1|interviewer),
                              data=my_df,
                              REML=FALSE)

summary(qual_interviewer_simple)

## Interaction effects

qual_interviewer_interaction <- lmer(interviewer_qual
                                   ~ 1 
                                   + interview 
                                   + style 
                                   + interview*style
                                   + (1 + interview |id)
                                   + (1|interviewer),
                                   data=my_df,
                                   REML=FALSE)

summary(qual_interviewer_interaction)

anova(qual_interviewer_simple, qual_interviewer_interaction, refit=FALSE)

# Exploratory analyses ---------------------------------------------------------


## Predicting disclosed details by self_assessment -----------------------------

### Main effect model

expl_model_1 <- lmer(detail
                     ~ interview
                     + style
                     + self_assessment
                     + (1 + interview | id) 
                     + (1 | stage),
                     data=my_df,
                     REML=FALSE)

summary(expl_model_1)


### Interaction effect model

expl_model_2 <- lmer(detail
                     ~ interview
                     + style
                     + self_assessment
                     + self_assessment*style
                     + self_assessment*interview
                     + (1 + interview | id) 
                     + (1 | stage),
                     data = my_df,
                     REML = FALSE
)

summary(expl_model_2)

comp_expl_model_anova <- anova(expl_model_1, expl_model_2)

### 3-way Interaction effect model

expl_model_3 <- lmer(detail
                     ~ interview
                     + style
                     + self_assessment
                     + self_assessment*style
                     + self_assessment*interview
                     + self_assessment*interview*style
                     + (1 + interview | MC/id) 
                     + (1|stage),
                     data = my_df,
                     REML = FALSE
)

summary(expl_model_3)

comp_expl_model_anova_2 <- anova(expl_model_2, expl_model_3)
