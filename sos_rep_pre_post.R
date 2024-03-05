################################################################################

# Pre & Post-Questionnaire 

################################################################################

# Load data --------------------------------------------------------------------

qualtrics <- read_csv("./qualtrics_clean.csv") 

# Demographics -----------------------------------------------------------------

## Age

interviewee_age_table <- qualtrics %>% 
  summarise(
    Age_M = mean(age, na.rm = TRUE),
    Age_SD = sd(age, na.rm = TRUE),
    Age_Mdn = median(age, na.rm = TRUE)
  ) 

## Gender

### 1 = Male, 2 = Female, 3 = Non-Binary, 4 = Prefer not to say 

interviewee_gender_table <- qualtrics %>% 
  filter(interview == 0) %>% 
  group_by(gender) %>% 
  summarise(
    n = n()) %>%
  mutate(rel_freq = paste0(round(100 * n/sum(n), 0), "%"))

# Pre-interview questionnaire --------------------------------------------------

# Pre-interview questionnaire --------------------------------------------------

## Confidence

confidence_desc <- qualtrics %>%
  #group_by(style, interview) %>% 
  summarise(
    Mean = mean(confidence, na.rm = TRUE),
    SD = sd(confidence, na.rm = TRUE),
    Median = median(confidence, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )

## Motivation

motivation_desc <- qualtrics %>% 
  group_by(style, interview) %>% 
  summarise(
    Mean = mean(motivation, na.rm = TRUE),
    SD = sd(motivation, na.rm = TRUE),
    Median = median(motivation, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )

# Post-interview questionnaire -------------------------------------------------

performance_desc <- qualtrics %>%
  group_by(style) %>%
  summarise(
    Mean = mean(self_assessment, na.rm = TRUE),
    SD = sd(self_assessment, na.rm = TRUE),
    Median = median(self_assessment, na.rm = TRUE)
  )

change_strat_desc <- qualtrics %>%
  drop_na (change_strategy) %>% 
  group_by(style, change_strategy) %>%
  summarise(
    n = n()) %>%
  mutate(rel_freq = paste0(round(100 * n/sum(n), 0), "%")
  )

interview_desc <- qualtrics %>%
  group_by(style) %>%
  summarise(
    Mean = mean(interview_qual, na.rm = TRUE),
    SD = sd(interview_qual, na.rm = TRUE),
    Median = median(interview_qual, na.rm = TRUE)
  )

interviewer_desc <- qualtrics %>%
  group_by(style) %>%
  summarise(
    Mean = mean(interviewer_qual, na.rm = TRUE),
    SD = sd(interviewer_qual, na.rm = TRUE),
    Median = median(interviewer_qual, na.rm = TRUE)
  )

