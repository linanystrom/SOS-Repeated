################################################################################

# Data preparation

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("readr", "dplyr")

lapply(packages, library, character.only = TRUE)

qualtrics_raw <- read_csv("./qualtrics.csv") %>% 
  filter(!row_number() %in% c(1, 2)) %>%
  type_convert()

excell_raw <- read_xlsx("./coding_random.xlsx")

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

### Organize columns

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

##

qualtrics_clean <- qualtrics_clean %>%
  mutate(
    interview = case_when(
      interview == 1 ~ 0,
      interview == 2 ~ 1,
      interview == 3 ~ 2
    )
  )


### Long format excell data

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

### Add interview variable, excel data

excell_clean <- excell_long %>% 
  mutate(
    interview = case_when(
      stage == "int1_st1" ~ 0,
      stage == "int1_st2" ~ 0,
      stage == "int1_st3" ~ 0,
      stage == "int1_st4" ~ 0,
      stage == "int1_st5" ~ 0,
      stage == "int2_st1" ~ 1,
      stage == "int2_st2" ~ 1,
      stage == "int2_st3" ~ 1,
      stage == "int2_st4" ~ 1,
      stage == "int2_st5" ~ 1,
      stage == "int3_st1" ~ 2,
      stage == "int3_st2" ~ 2,
      stage == "int3_st3" ~ 2,
      stage == "int3_st4" ~ 2,
      stage == "int3_st5" ~ 2
    ),
    
    stage = case_when(
      stage == "int1_st1" ~ 0,
      stage == "int1_st2" ~ 1,
      stage == "int1_st3" ~ 2,
      stage == "int1_st4" ~ 3,
      stage == "int1_st5" ~ 4,
      stage == "int2_st1" ~ 0,
      stage == "int2_st2" ~ 1,
      stage == "int2_st3" ~ 2,
      stage == "int2_st4" ~ 3,
      stage == "int2_st5" ~ 4,
      stage == "int3_st1" ~ 0,
      stage == "int3_st2" ~ 1,
      stage == "int3_st3" ~ 2,
      stage == "int3_st4" ~ 2,
      stage == "int3_st5" ~ 3
      )
    )
    
excell_clean <- excell_clean %>% 
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
          stage == 4 ~ 2), NA)
        )))
        
      
excell_clean <- excell_clean %>% 
  mutate(
    end_slope = ifelse(interview == 0, case_when(
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
              stage == 4 ~ 2), NA)
            )))

### Merge data -----------------------------------------------------------------

merged_long_data <- merge(x=qualtrics_clean,y=excell_clean, 
                   by=c("id","interview"))

merged_long_data$stage_sq <- merged_long_data$stage^2

### Export data ----------------------------------------------------------------

write.csv(
  qualtrics_clean,
  "./qualtrics_clean.csv",
  row.names = FALSE
)

write.csv(
  excell_clean,
  "./excell_clean.csv",
  row.names = FALSE
)

write.csv(
  merged_long_data,
  "./test_data.csv",
  row.names = FALSE
)

