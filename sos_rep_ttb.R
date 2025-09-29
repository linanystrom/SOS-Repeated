################################################################################

# "Take the best" analysis 

################################################################################

library(emmeans)
library(compute.es)

take_the_best <- read_xlsx("data/coding_solved.xlsx") 

take_the_best <- take_the_best %>% 
  mutate(
    activity_1 = pmax(int1_st1, int2_st1, int3_st1),
    activity_2 = pmax(int1_st2, int2_st2, int3_st2),
    activity_3 = pmax(int1_st3, int2_st3, int3_st3),
    activity_4 = pmax(int1_st4, int2_st4, int3_st4),
    activity_5 = pmax(int1_st5, int2_st5, int3_st5))

# Make long, follow by analysis?

# Long form detail data --------------------------------------------------------

take_the_best <- take_the_best  %>% 
  pivot_longer(
    cols = c("activity_1",
             "activity_2",
             "activity_3",
             "activity_4",
             "activity_5",
             ),
    names_to = "ttb_activity",
    values_to = "detail")

take_the_best <- take_the_best %>% 
  mutate(
    critical = case_when(
      ttb_activity == "activity_1" ~ 0,
      ttb_activity == "activity_2" ~ 0,
      ttb_activity == "activity_3" ~ 0,
      ttb_activity == "activity_4" ~ 1,
      ttb_activity == "activity_5" ~ 1
    ))

ttb_desc <- take_the_best %>% 
  group_by(style, critical) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )

ttb_nc_desc <- take_the_best %>% filter(critical == 0) %>% 
  group_by(style) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE),
    n = n()
  )

sum_d_ncrit <- mes(
  m.1  = ttb_nc_desc[ttb_nc_desc$style == "sos", ]$Mean,
  m.2  = ttb_nc_desc[ttb_nc_desc$style == "direct", ]$Mean,
  sd.1 = ttb_nc_desc[ttb_nc_desc$style == "sos", ]$SD,
  sd.2 = ttb_nc_desc[ttb_nc_desc$style == "direct", ]$SD,
  n.1  = ttb_nc_desc[ttb_nc_desc$style == "sos", ]$n,
  n.2  = ttb_nc_desc[ttb_nc_desc$style == "direct", ]$n
)

ttb_c_desc <- take_the_best %>% filter(critical == 1) %>% 
  group_by(style) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE),
    n = n()
  )

sum_d_ncrit <- mes(
  m.1  = ttb_c_desc[ttb_c_desc$style == "sos", ]$Mean,
  m.2  = ttb_c_desc[ttb_c_desc$style == "direct", ]$Mean,
  sd.1 = ttb_c_desc[ttb_c_desc$style == "sos", ]$SD,
  sd.2 = ttb_c_desc[ttb_c_desc$style == "direct", ]$SD,
  n.1  = ttb_c_desc[ttb_c_desc$style == "sos", ]$n,
  n.2  = ttb_c_desc[ttb_c_desc$style == "direct", ]$n
)

# TTB analysis -----------------------------------------------------------------

# Simple

ttb_simple <- lmer(detail 
                    ~ 1 
                    + style
                    + critical
                    + (1 | id)
                    + (1 | interviewer),
                    data = take_the_best,
                    REML = FALSE)

summary(ttb_simple)

# Interaction 

ttb_int <- lmer(detail 
                   ~ 1 
                   + style
                   + critical
                   + style*critical
                   + (1 | id)
                   + (1 | interviewer),
                   data = take_the_best,
                   REML = FALSE)

summary(ttb_int)

anova(ttb_simple,ttb_int, refit=FALSE)

ttb_emm <- emmeans(ttb_int, specs = ~ style + critical)

pairs(ttb_emm)

eff_size(ttb_emm, sigma = sigma(ttb_int), edf = 222)
