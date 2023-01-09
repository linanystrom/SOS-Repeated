################################################################################

# Reliability analyses of composite measures

################################################################################

## Self-assessment--------------------------------------------------------------

self_assessment_corr = sos[c(
  "interview_statements_1",
  "interview_statements_2",
  "interview_statements_3",
  "interview_statements_4")]

describe(self_assessment_corr)

lowerCor(self_assessment_corr)

self_corr <- omega(self_assessment_corr)

summary(self_corr)

## CFA

self_assessment_uni_model <- 
  '

self =~ interview_statements_1
+ interview_statements_2
+ interview_statements_3
+ interview_statements_4

'

self_uni_fit <- cfa(self_assessment_uni_model, data = sos,
                    std.lv = TRUE,
                    estimator = "MLR")

summary(self_uni_fit, fit.measures = TRUE)

compRelSEM(self_uni_fit)


## Interview quality -----------------------------------------------------------

interviewQ_corr = sos[c(
  "interview_adj_1",
  "interview_adj_2_R",
  "interview_adj_3",
  "interview_adj_4_R",
  "interview_adj_5",
  "interview_adj_6")]

describe(interviewQ_corr)

lowerCor(interviewQ_corr)

IQ_corr <- omega(interviewQ_corr)

summary(IQ_corr)

## CFA

qual_interview_uni_model <- 
  '

qual_interview =~ interview_adj_1
+ interview_adj_2_R
+ interview_adj_3
+ interview_adj_4_R
+ interview_adj_5
+ interview_adj_6

'

qual_interview_uni_fit <- cfa(qual_interview_uni_model, data = sos,
                    std.lv = TRUE,
                    estimator = "MLR")

summary(qual_interview_uni_fit, fit.measures = TRUE)

compRelSEM(qual_interview_uni_fit)


## Interviewer perception ------------------------------------------------------

interviewerP_corr = sos[c(
  "interviewer_adj_1",
  "interviewer_adj_2_R",
  "interviewer_adj_3_R",
  "interviewer_adj_4",
  "interviewer_adj_5",
  "interviewer_adj_6_R")]


describe(interviewerP_corr)

lowerCor(interviewerP_corr)

IP_corr <- omega(interviewerP_corr)

summary(IP_corr)

## CFA

qual_interviewer_uni_model <- 
  '

qual_interviewer =~ interviewer_adj_1
+ interviewer_adj_2_R
+ interviewer_adj_3_3
+ interviewer_adj_4
+ interviewer_adj_5
+ interviewer_adj_6_R

'

qual_interviewer_uni_fit <- cfa(qual_interviewer_uni_model, data = sos,
                              std.lv = TRUE,
                              estimator = "MLR")

summary(qual_interviewer_uni_fit, fit.measures = TRUE)

compRelSEM(qual_interviewer_uni_fit)