library(tidyverse)
library(readr)
library(arrow)     # for reading paquet files
# install.packages("psych")
library(psych)

# reading dataset
c_df <- read_csv("~/Documents/Davis/DDSC/Analyzing Unemployment Project/cps_monthly_hazards.csv")
p_df <- read_parquet("~/Documents/Davis/DDSC/Analyzing Unemployment Project/cps_master_panel_2005_2026.parquet", 
                     Wcol_select = c(RACE, EMPSTAT, WHYUNEMP))

# peeking df
glimpse(c_df)
glimpse(p_df)

# Univariate analysis
# choose variables from csv file
c_df <- c_df |>
  select(U_short , f_shimer, lambda_t)

# Race, EMPSTAT, WHYUNEMP are all categorical variables.
# U_short, f_shimer, lambda_t are all numerical variables.

# c_df: summary statistics and visualization
describe(c_df)

# histograms
c_long <- c_df |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot(c_long, aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distributions", x = NULL, y = "Count") +
  theme_minimal()

# boxplots
ggplot(c_long, aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal()



# p_df: proportions and visualization


## convert to factors with labels
p_df <- p_df |>
  mutate(
    EMPSTAT = factor(EMPSTAT,
                     levels = c(0, 01, 10, 12, 20, 21, 22, 30, 31, 32, 33, 34, 35, 36),
                     labels = c("NIU", "Armed Forces", "At work", "Has job, not at work", "Unemployed",
                                "Unemployed, experienced", "Unemployed, new worker",
                                "NILF", "NILF, housework", "NILF, unable to work",
                                "NILF, school", "NILF, other", "NILF, unpaid work",
                                "NILF, retired")),
    WHYUNEMP = factor(WHYUNEMP,
                      levels = 0:6,
                      labels = c("NIU", "Job loser/layoff", "Other job loser",
                                 "Temp job ended", "Job leaver", "Re-entrant", "New entrant")),
    RACE = factor(RACE
                  # ,levels = c(100, 200, 300, 650, 651, 652, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815),
                  # labels = c("White", "Black", "American Indian/Aleut/Eskimo", "Asian or Pacific Islander",
                  #           "Asian only", "Hawaiian/Pacific Islander only", "White-Black", "White-American Indian",
                  #           "White-Asian", "White-Hawaiian/Pacific Islander", "Black-American Indian", "Black-Asian",
                  #           "Black-Hawaiian/Pacific Islander", "American Indian-Asian", "Asian-Hawaiian/Pacific Islander",
                  #           "White-Black-American Indian", "White-Black-Asian", "White-American Indian-Asian", "White-Asian-Hawaiian/Pacific Islander",
                  #           "White-Black-American Indian-Asian", "American Indian-Hawaiian/Pacific Islander")
                             # many codes (max 830), so label after checking IPUMS
  ))

## create table for proportions
prop_tbl <- function(data, var) {
  data |>
    count({{ var }}) |>
    mutate(prop = n / sum(n),
           pct = round(100 * prop, 2))
}

prop_tbl(p_df, EMPSTAT)
prop_tbl(p_df, RACE)

# WHYUNEMP: "NIU" is everyone who isn't unemployed, so drop it
p_df |> filter(WHYUNEMP != "NIU") |> prop_tbl(WHYUNEMP)

## create bar charts for categorical variables.
bar_plot <- function(data, var, title) {
  data |>
    count({{ var }}) |>
    mutate(prop = n / sum(n)) |>
    ggplot(aes(x = {{ var }}, y = prop)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = title, x = NULL, y = "Share of observations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

bar_plot(p_df, EMPSTAT, "Employment status")
bar_plot(p_df, RACE, "Race")
p_df |> filter(WHYUNEMP != "NIU") |>
  bar_plot(WHYUNEMP, "Reason for unemployment")