################################################################################

# SoS Repeated - Random assignment

################################################################################

# Set up environment -----------------------------------------------------------

## Packages

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr",
              "randomizr")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(666)

# Set up design ----------------------------------------------------------------

sample    <- 60   #Sample size
int_style     <- c("direct", "sos")
mock_crimes <- c("MC1", "MC2", "MC3")

# Set up basic data structure---------------------------------------------------

id <- 1:sample

df <- expand.grid(ID = id)

df$mc <- sample(mock_crimes, sample, replace=TRUE)

df$style <- complete_ra(N = sample, conditions = c("direct", "sos"))

overall_count <- count(df, mc, style)


# Export permutations ----------------------------------------------------------

write.csv(
  df,
  "./random_assignment.csv",
  row.names = FALSE)

