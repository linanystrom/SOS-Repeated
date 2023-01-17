################################################################################

# SoS Repeated - Scale Reliability

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("psych", "lavaan", "semTools")

lapply(packages, library, character.only = TRUE)

sos_reliability <- read_csv("./qualtrics_clean.csv") #Load data here

sos_reliability_T1 <- sos_reliability %>% 
  filter(interview == 0)

sos_reliability_T2 <- sos_reliability %>% 
  filter(interview == 1)

sos_reliability_T3 <- sos_reliability %>% 
  filter(interview == 2)


# Reliability analysis - Self assessment ---------------------------------------

# Interview 1

self_assessment_corr_T1 = sos_reliability_T1[c(
  "interview_statements_1",
  "interview_statements_2",
  "interview_statements_3",
  "interview_statements_4")]

describe(self_assessment_corr_T1)

lowerCor(self_assessment_corr_T1)

self_corr_T1 <- omega(self_assessment_corr_T1)

summary(self_corr_T1)

## CFA

self_assessment_uni_model_T1 <- 
  '

self =~ interview_statements_1
+ interview_statements_2
+ interview_statements_3
+ interview_statements_4

'

self_uni_fit_T1 <- cfa(self_assessment_uni_model_T1, data = sos_reliability_T1,
                    std.lv = TRUE,
                    estimator = "MLR")

summary(self_uni_fit_T1, fit.measures = TRUE)

compRelSEM(self_uni_fit_T1)

# Interview 2

self_assessment_corr_T2 = sos_reliability_T2[c(
  "interview_statements_1",
  "interview_statements_2",
  "interview_statements_3",
  "interview_statements_4")]

describe(self_assessment_corr_T2)

lowerCor(self_assessment_corr_T2)

self_corr_T2 <- omega(self_assessment_corr_T2)

summary(self_corr_T2)

## CFA

self_assessment_uni_model_T2 <- 
  '

self =~ interview_statements_1
+ interview_statements_2
+ interview_statements_3
+ interview_statements_4

'

self_uni_fit_T2 <- cfa(self_assessment_uni_model_T2, data = sos_reliability_T2,
                       std.lv = TRUE,
                       estimator = "MLR")

summary(self_uni_fit_T2, fit.measures = TRUE)

compRelSEM(self_uni_fit_T2)

# Interview 3

self_assessment_corr_T3 = sos_reliability_T3[c(
  "interview_statements_1",
  "interview_statements_2",
  "interview_statements_3",
  "interview_statements_4")]

describe(self_assessment_corr_T3)

lowerCor(self_assessment_corr_T3)

self_corr_T3 <- omega(self_assessment_corr_T3)

summary(self_corr_T3)

## CFA

self_assessment_uni_model_T3 <- 
  '

self =~ interview_statements_1
+ interview_statements_2
+ interview_statements_3
+ interview_statements_4

'

self_uni_fit_T3 <- cfa(self_assessment_uni_model_T3, data = sos_reliability_T3,
                       std.lv = TRUE,
                       estimator = "MLR")

summary(self_uni_fit_T3, fit.measures = TRUE)

compRelSEM(self_uni_fit_T3)

## Interview quality -----------------------------------------------------------

# Interview 1

interviewQ_corr_T1 = sos_reliability_T1[c(
  "interview_adj_1",
  "interview_adj_2_R",
  "interview_adj_3",
  "interview_adj_4_R",
  "interview_adj_5",
  "interview_adj_6")]

describe(interviewQ_corr_T1)

lowerCor(interviewQ_corr_T1)

IQ_corr <- omega(interviewQ_corr_T1)

summary(IQ_corr_T1)

## CFA

qual_interview_uni_model_T1 <- 
  '

qual_interview =~ interview_adj_1
+ interview_adj_2_R
+ interview_adj_3
+ interview_adj_4_R
+ interview_adj_5
+ interview_adj_6

'

qual_interview_uni_fit_T1 <- cfa(qual_interview_uni_model_T1, data = sos_reliability_T1,
                              std.lv = TRUE,
                              estimator = "MLR")

summary(qual_interview_uni_fit_T1, fit.measures = TRUE)

compRelSEM(qual_interview_uni_fit_T1)

# Interview 2

interviewQ_corr_T2 = sos_reliability_T2[c(
  "interview_adj_1",
  "interview_adj_2_R",
  "interview_adj_3",
  "interview_adj_4_R",
  "interview_adj_5",
  "interview_adj_6")]

describe(interviewQ_corr_T2)

lowerCor(interviewQ_corr_T2)

IQ_corr <- omega(interviewQ_corr_T2)

summary(IQ_corr_T2)

## CFA

qual_interview_uni_model_T2 <- 
  '

qual_interview =~ interview_adj_1
+ interview_adj_2_R
+ interview_adj_3
+ interview_adj_4_R
+ interview_adj_5
+ interview_adj_6

'

qual_interview_uni_fit_T2 <- cfa(qual_interview_uni_model_T2, data = sos_reliability_T2,
                                 std.lv = TRUE,
                                 estimator = "MLR")

summary(qual_interview_uni_fit_T2, fit.measures = TRUE)

compRelSEM(qual_interview_uni_fit_T2)

# Interview 3

interviewQ_corr_T3 = sos_reliability_T3[c(
  "interview_adj_1",
  "interview_adj_2_R",
  "interview_adj_3",
  "interview_adj_4_R",
  "interview_adj_5",
  "interview_adj_6")]

describe(interviewQ_corr_T3)

lowerCor(interviewQ_corr_T3)

IQ_corr <- omega(interviewQ_corr_T3)

summary(IQ_corr_T3)

## CFA

qual_interview_uni_model_T3 <- 
  '

qual_interview =~ interview_adj_1
+ interview_adj_2_R
+ interview_adj_3
+ interview_adj_4_R
+ interview_adj_5
+ interview_adj_6

'

qual_interview_uni_fit_T3 <- cfa(qual_interview_uni_model_T3,
                                 data = sos_reliability_T3,
                                 std.lv = TRUE,
                                 estimator = "MLR")

summary(qual_interview_uni_fit_T3, fit.measures = TRUE)

compRelSEM(qual_interview_uni_fit_T3)


## Interviewer perception ------------------------------------------------------

# Interview 1

interviewerP_corr_T1 = sos_reliability_T1[c(
  "interviewer_adj_1",
  "interviewer_adj_2_R",
  "interviewer_adj_3_R",
  "interviewer_adj_4",
  "interviewer_adj_5",
  "interviewer_adj_6_R")]


describe(interviewerP_corr_T1)

lowerCor(interviewerP_corr_T1)

IP_corr <- omega(interviewerP_corr_T1)

summary(IP_corr_T1)

## CFA

qual_interviewer_uni_model_T1 <- 
  '

qual_interviewer =~ interviewer_adj_1
+ interviewer_adj_2_R
+ interviewer_adj_3_3
+ interviewer_adj_4
+ interviewer_adj_5
+ interviewer_adj_6_R

'

qual_interviewer_uni_fit_T1 <- cfa(qual_interviewer_uni_model_T1,
                                   data = sos_reliability_T1,
                                std.lv = TRUE,
                                estimator = "MLR")

summary(qual_interviewer_uni_fit_T1, fit.measures = TRUE)

compRelSEM(qual_interviewer_uni_fit_T1)

# Interview 2

interviewerP_corr_T2 = sos_reliability_T2[c(
  "interviewer_adj_1",
  "interviewer_adj_2_R",
  "interviewer_adj_3_R",
  "interviewer_adj_4",
  "interviewer_adj_5",
  "interviewer_adj_6_R")]


describe(interviewerP_corr_T2)

lowerCor(interviewerP_corr_T2)

IP_corr <- omega(interviewerP_corr_T2)

summary(IP_corr_T2)

## CFA

qual_interviewer_uni_model_T2 <- 
  '

qual_interviewer =~ interviewer_adj_1
+ interviewer_adj_2_R
+ interviewer_adj_3_3
+ interviewer_adj_4
+ interviewer_adj_5
+ interviewer_adj_6_R

'

qual_interviewer_uni_fit_T2 <- cfa(qual_interviewer_uni_model_T2,
                                   data = sos_reliability_T2,
                                   std.lv = TRUE,
                                   estimator = "MLR")

summary(qual_interviewer_uni_fit_T2, fit.measures = TRUE)

compRelSEM(qual_interviewer_uni_fit_T2)

# Interview 3

interviewerP_corr_T3 = sos_reliability_T3[c(
  "interviewer_adj_1",
  "interviewer_adj_2_R",
  "interviewer_adj_3_R",
  "interviewer_adj_4",
  "interviewer_adj_5",
  "interviewer_adj_6_R")]


describe(interviewerP_corr_T3)

lowerCor(interviewerP_corr_T3)

IP_corr <- omega(interviewerP_corr_T3)

summary(IP_corr_T3)

## CFA

qual_interviewer_uni_model_T3 <- 
  '

qual_interviewer =~ interviewer_adj_1
+ interviewer_adj_2_R
+ interviewer_adj_3_3
+ interviewer_adj_4
+ interviewer_adj_5
+ interviewer_adj_6_R

'

qual_interviewer_uni_fit_T3 <- cfa(qual_interviewer_uni_model_T3,
                                   data = sos_reliability_T3,
                                   std.lv = TRUE,
                                   estimator = "MLR")

summary(qual_interviewer_uni_fit_T3, fit.measures = TRUE)

compRelSEM(qual_interviewer_uni_fit_T3)