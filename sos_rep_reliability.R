################################################################################

# SoS Repeated - Scale Reliability

################################################################################

# Basic setup ------------------------------------------------------------------

packages <- c("psych")

lapply(packages, library, character.only = TRUE)

my_df <- read_csv("./sim_repeated.csv") #Load data here

# Filter T1 

# Reliability analysis - Self assessment ---------------------------------------

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


omega(self_assessment)

omega(qual_interview)

omega(qual_interviewer)