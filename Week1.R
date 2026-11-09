library(tidyverse)
library(readr)
library(arrow)     # for reading paquet files
# install.packages("psych")
library(psych)

# reading dataset
c_df <- read_csv("~/Downloads/cps_monthly_hazards.csv")
p_df <- read_parquet("~/Downloads/cps_master_panel_2005_2026.parquet", col_select = c(RACE, EMPSTAT, WHYUNEMP))

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
c_df |> gg

# p_df: proportions and visualization
## create bar charts for categorical variables.
## create table for proportions

