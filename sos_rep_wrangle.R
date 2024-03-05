################################################################################

# Data preparation

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("readr", "dplyr", "readxl", "tidyr", "reshape2")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

qualtrics_eng <- read_csv("data/Repeated_SoS_ENG.csv") %>% 
  filter(!row_number() %in% c(1, 2)) %>%
  type_convert()

qualtrics_swe <- read_csv("data/Repeated_SoS_SWE.csv") %>% 
  filter(!row_number() %in% c(1, 2)) %>%
  type_convert()

qualtrics_raw <- rbind(qualtrics_eng, qualtrics_swe)

excell_raw <- read_xlsx("data/coding_solved.xlsx") 

condition_info <- excell_raw %>% select(c("id","style", "interviewer"))

wide_data <- qualtrics_raw %>% 
  select(c("id", "interview",
           "interview_statements_1",
           "interview_statements_2",
           "interview_statements_3",
           "interview_statements_4"))

wide_data_clean <- wide_data[rowSums(is.na(wide_data)) <= 3, ] 

wide_data_clean <- wide_data_clean %>% 
  pivot_wider(
    names_from = interview,
    values_from = c("interview_statements_1",
                    "interview_statements_2",
                    "interview_statements_3", 
                    "interview_statements_4")
  )

wide_data_clean <- wide_data_clean %>% 
  rename(confident_1 = interview_statements_1_1,
         confident_2 = interview_statements_1_2,
         confident_3 = interview_statements_1_3,
         difficult_1 = interview_statements_2_1,
         difficult_2 = interview_statements_2_2,
         difficult_3 = interview_statements_2_3,
         suspicious_1 = interview_statements_3_1,
         suspicious_2 = interview_statements_3_2,
         suspicious_3 = interview_statements_3_3,
         convincing_1 = interview_statements_4_1,
         convincing_2 = interview_statements_4_2,
         convincing_3 = interview_statements_4_3)

wide_data_clean <- merge(wide_data_clean, excell_raw, by = "id")

wide_data_clean <- wide_data_clean %>%
  select(id,
         confident_1,
         confident_2, 
         confident_3, 
         difficult_1, 
         difficult_2, 
         difficult_3, 
         suspicious_1, 
         suspicious_2,
         suspicious_3,
         convincing_1,
         convincing_2,
         convincing_3,
         everything()) %>%
  type_convert()

qualtrics_raw <- merge(x=qualtrics_raw,y=condition_info, by= "id")

# Long form detail data --------------------------------------------------------

excell_long <- excell_raw  %>% 
  pivot_longer(
    cols = c("int1_st1",
             "int1_st2",
             "int1_st3",
             "int1_st4",
             "int1_st5",
             "int2_st1",
             "int2_st2",
             "int2_st3",
             "int2_st4",
             "int2_st5",
             "int3_st1",
             "int3_st2",
             "int3_st3",
             "int3_st4",
             "int3_st5"),
    names_to = "stage",
    values_to = "detail")

# Assign values ----------------------------------------------------------------

excell_long <- excell_long %>% 
  mutate(
    interview = case_when(
      startsWith(stage,"int1") ~ 0,
      startsWith(stage,"int2") ~ 1,
      startsWith(stage,"int3") ~ 2,
      
    ),
    activity = case_when(
      endsWith(stage, "st1") ~ 0,
      endsWith(stage, "st2") ~ 1,
      endsWith(stage, "st3") ~ 2,
      endsWith(stage, "st4") ~ 3,
      endsWith(stage, "st5") ~ 4,
      
    ),
    critical = case_when(
      endsWith(stage, "st1") ~ 0,
      endsWith(stage, "st2") ~ 0,
      endsWith(stage, "st3") ~ 0,
      endsWith(stage, "st4") ~ 1,
      endsWith(stage, "st5") ~ 1
    ))

## Create composite measures ---------------------------------------------------

qualtrics_clean <- qualtrics_raw %>% 
  mutate(
    interview_adj_2_R = case_when(
      interview_adj_2 == 5 ~ 1,
      interview_adj_2 == 4 ~ 2,
      interview_adj_2 == 3 ~ 3,
      interview_adj_2 == 2 ~ 4,
      interview_adj_2 == 1 ~ 5
    ),
    interview_adj_4_R = case_when(
      interview_adj_4 == 5 ~ 1,
      interview_adj_4 == 4 ~ 2,
      interview_adj_4 == 3 ~ 3,
      interview_adj_4 == 2 ~ 4,
      interview_adj_4 == 1 ~ 5
    ),
    
    interview_qual = (
      interview_adj_1 + 
        interview_adj_2_R + 
        interview_adj_3 + 
        interview_adj_4_R + 
        interview_adj_5 + 
        interview_adj_6)/6,
    
    ###Interviewer quality
    
    interviewer_adj_2_R = case_when(
      interviewer_adj_2 == 5 ~ 1,
      interviewer_adj_2 == 4 ~ 2,
      interviewer_adj_2 == 3 ~ 3,
      interviewer_adj_2 == 2 ~ 4,
      interviewer_adj_2 == 1 ~ 5
    ),
    interviewer_adj_3_R = case_when(
      interviewer_adj_3 == 5 ~ 1,
      interviewer_adj_3 == 4 ~ 2,
      interviewer_adj_3 == 3 ~ 3,
      interviewer_adj_3 == 2 ~ 4,
      interviewer_adj_3 == 1 ~ 5
    ),
    interviewer_adj_6_R = case_when(
      interviewer_adj_6 == 5 ~ 1,
      interviewer_adj_6 == 4 ~ 2,
      interviewer_adj_6 == 3 ~ 3,
      interviewer_adj_6 == 2 ~ 4,
      interviewer_adj_6 == 1 ~ 5
    ),
    interviewer_qual = (
      interviewer_adj_1 + 
        interviewer_adj_2_R + 
        interviewer_adj_3_R + 
        interviewer_adj_4 + 
        interviewer_adj_5 + 
        interviewer_adj_6_R)/6,
    
    ### Self-assessment of performance
    
    interview_statements_2 = case_when(
      interview_statements_2 == 5 ~ 1,
      interview_statements_2 == 4 ~ 2,
      interview_statements_2 == 3 ~ 3,
      interview_statements_2 == 2 ~ 4,
      interview_statements_2 == 1 ~ 5
    ),
    interview_statements_3 = case_when(
      interview_statements_3 == 5 ~ 1,
      interview_statements_3 == 4 ~ 2,
      interview_statements_3 == 3 ~ 3,
      interview_statements_3 == 2 ~ 4,
      interview_statements_3 == 1 ~ 5
    ),
    self_assessment = (
      interview_statements_1 + 
        interview_statements_2 + 
        interview_statements_3 + 
        interview_statements_4)/4,
  )

### Organize columns -----------------------------------------------------------

qualtrics_clean <- qualtrics_clean %>%
  select(id,
         interview,
         confidence, 
         motivation, 
         interview_qual, 
         interviewer_qual, 
         self_assessment, 
         age, 
         gender, 
         everything())

### Assign interview -----------------------------------------------------------

qualtrics_clean <- qualtrics_clean %>%
  mutate(
    interview = case_when(
      interview == 1 ~ 0,
      interview == 2 ~ 1,
      interview == 3 ~ 2
    )
  )

## Define slopes ---------------------------------------------------------------

excell_long <- excell_long %>% 
  mutate(start_slope = ifelse(interview == 0, case_when(
    activity == 0 ~ 0,
    activity == 1 ~ 0,
    activity == 2 ~ 0,
    activity == 3 ~ 0,
    activity == 4 ~ 0), 
    ifelse(interview == 1, case_when(
      activity == 0 ~ 0,
      activity == 1 ~ 1,
      activity == 2 ~ 1,
      activity == 3 ~ 1,
      activity == 4 ~ 1), 
      ifelse(interview == 2, case_when(
        activity == 0 ~ 0,
        activity == 1 ~ 1,
        activity == 2 ~ 2,
        activity == 3 ~ 2,
        activity == 4 ~ 2), NA)
    )))


excell_long <- excell_long %>% 
  mutate(
    end_slope = ifelse(interview == 0, case_when(
      activity == 0 ~ 0,
      activity == 1 ~ 1,
      activity == 2 ~ 2,
      activity == 3 ~ 3,
      activity == 4 ~ 4), 
      ifelse(interview == 1, case_when(
        activity == 0 ~ 0,
        activity == 1 ~ 0,
        activity == 2 ~ 1,
        activity == 3 ~ 2,
        activity == 4 ~ 3), 
        ifelse(interview == 2, case_when(
          activity == 0 ~ 0,
          activity == 1 ~ 0,
          activity == 2 ~ 0,
          activity == 3 ~ 1,
          activity == 4 ~ 2), NA)
      )))

### Merge data -----------------------------------------------------------------

merged_long_data <- merge(x=qualtrics_clean,y=excell_long, 
                   by=c("id", "interview", "style", "interviewer"))

merged_long_data$stage_sq <- merged_long_data$activity^2

### Export data ----------------------------------------------------------------

write.csv(
  qualtrics_clean,
  "data/qualtrics_clean.csv",
  row.names = FALSE
)

write.csv(
  excell_long,
  "data/excell_clean.csv",
  row.names = FALSE
)

write.csv(
  merged_long_data,
  "data/sos_rep_long.csv",
  row.names = FALSE
)

write_csv(
  wide_data_clean,
  "data/wide_data.csv"
)

