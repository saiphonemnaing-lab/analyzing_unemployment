# =============================================================================
# Unemployment by Education Level & Unemployment Duration (CPS, 2005-2026)
# Assignment: line plots (unemployment rate by education) + bar plots (DURUNEMP)
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------
# Run once if needed:
# install.packages(c("arrow", "dplyr", "tidyr", "ggplot2", "scales", "lubridate"))
library(arrow)      # read parquet WITHOUT loading the whole file into memory
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

# ---- 1. Load ONLY the columns we need (so your computer doesn't crash) --------
# Change this path to wherever the file lives on your computer.
# Tip: in RStudio, Session > Set Working Directory > To Source File Location
parquet_path <- file.choose()

cps <- open_dataset(parquet_path) |>
  select(YEAR, MONTH, WTFINL, EMPSTAT, EDUC, DURUNEMP) |>   # <- column selection
  collect()                                                 # <- only now pulled into RAM

glimpse(cps)

# ---- 2. Clean / recode --------------------------------------------------------
# EMPSTAT: 10,12 = employed | 21,22 = unemployed | 32,34,36 = not in labor force
# EDUC (IPUMS codes): <73 = less than HS | 73 = HS diploma/GED
#                     81 = some college, no degree | 91,92 = associate degree
#                     111 = bachelor's | 123,124,125 = master's/professional/doctorate
# DURUNEMP: weeks unemployed; 999 = "not in universe" (i.e., not unemployed)

cps <- cps |>
  mutate(
    date = make_date(YEAR, MONTH, 1),
    in_lf = EMPSTAT %in% c(10, 12, 21, 22),   # in the labor force
    unemp = EMPSTAT %in% c(21, 22),           # unemployed
    edu_group = case_when(
      EDUC < 73              ~ "Less than HS",
      EDUC == 73             ~ "HS graduate",
      EDUC %in% c(81, 91, 92) ~ "Some college / Associate",
      EDUC >= 111            ~ "Bachelor's or higher",
      TRUE                   ~ NA_character_
    ),
    edu_group = factor(edu_group,
                       levels = c("Less than HS", "HS graduate",
                                  "Some college / Associate", "Bachelor's or higher"))
  )

# ---- 3. Recession periods (edit end dates here if the team decides otherwise) -
recessions <- tibble::tribble(
  ~name,              ~start,              ~end,
  "Great Recession",  as.Date("2007-12-01"), as.Date("2009-06-01"),
  "COVID-19",         as.Date("2020-02-01"), as.Date("2022-12-01")  # "2022/2023-ish"
)

# Reusable pieces so every plot looks consistent
recession_shading <- geom_rect(
  data = recessions, inherit.aes = FALSE,
  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
  fill = "grey60", alpha = 0.25
)

theme_set(theme_minimal(base_size = 13) +
            theme(plot.title = element_text(face = "bold"),
                  legend.position = "bottom"))

edu_colors <- c("Less than HS"             = "#d95f02",
                "HS graduate"              = "#1b9e77",
                "Some college / Associate" = "#7570b3",
                "Bachelor's or higher"     = "#386cb0")

dir.create("plots", showWarnings = FALSE)

# ---- 4. Monthly weighted unemployment rate by education ------------------------
# Rate = weighted # unemployed / weighted # in labor force (WTFINL = person weight)
ur_edu <- cps |>
  filter(!is.na(edu_group)) |>
  group_by(date, edu_group) |>
  summarise(
    unemp_rate = sum(WTFINL[unemp]) / sum(WTFINL[in_lf]) * 100,
    .groups = "drop"
  ) |>
  # Some months are missing from the data (e.g., Sep-Oct 2025). complete() inserts
  # NA rows so the lines show a gap instead of drawing straight through.
  complete(date = seq(min(date), max(date), by = "month"), edu_group)

# =============================== LINE PLOTS ===================================

# ---- Plot 1: HS grads vs. College grads (the main comparison) ------------------
p1 <- ur_edu |>
  filter(edu_group %in% c("HS graduate", "Bachelor's or higher")) |>
  ggplot(aes(date, unemp_rate, color = edu_group)) +
  recession_shading +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(values = edu_colors, name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(title = "Unemployment Rate: High School Graduates vs. College Graduates",
       subtitle = "Monthly, 2005-2026 | Shaded = Great Recession & COVID-19",
       x = NULL, y = "Unemployment rate",
       caption = "Source: CPS, ages 25-54, weighted by WTFINL")
p1
ggsave("plots/plot1_hs_vs_college_unemployment.png", p1, width = 10, height = 6, dpi = 300)

# ---- Plot 2: All four education levels -----------------------------------------
p2 <- ur_edu |>
  ggplot(aes(date, unemp_rate, color = edu_group)) +
  recession_shading +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(values = edu_colors, name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(title = "Unemployment Rate by Education Level",
       subtitle = "Monthly, 2005-2026 | Shaded = Great Recession & COVID-19",
       x = NULL, y = "Unemployment rate",
       caption = "Source: CPS, ages 25-54, weighted by WTFINL")
p2
ggsave("plots/plot2_all_education_levels_unemployment.png", p2, width = 10, height = 6, dpi = 300)

# ---- Plot 3: Same as Plot 1 but 12-month rolling average (smoother trend) -------
roll_mean <- function(x, k = 12) {
  as.numeric(stats::filter(x, rep(1 / k, k), sides = 1))  # trailing average
}

p3 <- ur_edu |>
  filter(edu_group %in% c("HS graduate", "Bachelor's or higher")) |>
  group_by(edu_group) |>
  arrange(date) |>
  mutate(unemp_smooth = roll_mean(unemp_rate)) |>
  ungroup() |>
  ggplot(aes(date, unemp_smooth, color = edu_group)) +
  recession_shading +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = edu_colors, name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(title = "Smoothed Unemployment Rate: HS Graduates vs. College Graduates",
       subtitle = "12-month trailing average | Shaded = Great Recession & COVID-19",
       x = NULL, y = "Unemployment rate (12-mo avg)",
       caption = "Source: CPS, ages 25-54, weighted by WTFINL")
p3
ggsave("plots/plot3_smoothed_hs_vs_college.png", p3, width = 10, height = 6, dpi = 300)

# =============================== BAR PLOTS (DURUNEMP) =========================

# Keep only unemployed people who actually have a duration (drops 999 = NIU)
dur <- cps |>
  filter(unemp, DURUNEMP < 999, !is.na(edu_group)) |>
  mutate(
    dur_bucket = cut(DURUNEMP,
                     breaks = c(-Inf, 4, 14, 26, Inf),
                     labels = c("< 5 weeks", "5-14 weeks", "15-26 weeks", "27+ weeks"),
                     right = TRUE),
    period = case_when(
      date <  as.Date("2007-12-01")                              ~ "Pre-recession\n(2005-Nov 2007)",
      date <= as.Date("2009-06-01")                              ~ "Great Recession\n(Dec 2007-Jun 2009)",
      date <  as.Date("2020-02-01")                              ~ "Recovery\n(Jul 2009-Jan 2020)",
      date <= as.Date("2022-12-01")                              ~ "COVID-19\n(Feb 2020-Dec 2022)",
      TRUE                                                       ~ "Post-COVID\n(2023-2026)"
    ),
    period = factor(period, levels = c("Pre-recession\n(2005-Nov 2007)",
                                       "Great Recession\n(Dec 2007-Jun 2009)",
                                       "Recovery\n(Jul 2009-Jan 2020)",
                                       "COVID-19\n(Feb 2020-Dec 2022)",
                                       "Post-COVID\n(2023-2026)"))
  )

bucket_colors <- c("< 5 weeks" = "#9ecae1", "5-14 weeks" = "#6baed6",
                   "15-26 weeks" = "#3182bd", "27+ weeks" = "#08306b")

# ---- Plot 4: Average weeks unemployed, by year ---------------------------------
dur_year <- dur |>
  group_by(YEAR) |>
  summarise(mean_weeks = weighted.mean(DURUNEMP, WTFINL), .groups = "drop")

p4 <- ggplot(dur_year, aes(factor(YEAR), mean_weeks)) +
  geom_col(fill = "#386cb0") +
  geom_text(aes(label = round(mean_weeks, 1)), vjust = -0.4, size = 3) +
  labs(title = "Average Unemployment Duration by Year",
       subtitle = "Mean weeks unemployed among the unemployed (2026 = partial year)",
       x = NULL, y = "Mean weeks unemployed",
       caption = "Source: CPS, ages 25-54, DURUNEMP, weighted by WTFINL") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p4
ggsave("plots/plot4_mean_duration_by_year.png", p4, width = 11, height = 6, dpi = 300)

# ---- Plot 5: Duration distribution by economic period --------------------------
dur_period <- dur |>
  group_by(period, dur_bucket) |>
  summarise(w = sum(WTFINL), .groups = "drop") |>
  group_by(period) |>
  mutate(share = w / sum(w)) |>
  ungroup()

p5 <- ggplot(dur_period, aes(period, share, fill = dur_bucket)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = bucket_colors, name = "Time unemployed") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Unemployment Duration Across Economic Periods",
       subtitle = "Share of unemployed workers by length of unemployment spell",
       x = NULL, y = "Share of unemployed",
       caption = "Source: CPS, ages 25-54, DURUNEMP, weighted by WTFINL")
p5
ggsave("plots/plot5_duration_by_period.png", p5, width = 11, height = 6, dpi = 300)

# ---- Plot 6: Average duration by education level (overall + recession periods) --
dur_edu_period <- dur |>
  filter(period %in% levels(period)[c(1, 2, 4)]) |>   # pre-recession, Great Rec., COVID
  group_by(edu_group, period) |>
  summarise(mean_weeks = weighted.mean(DURUNEMP, WTFINL), .groups = "drop")

p6 <- ggplot(dur_edu_period, aes(edu_group, mean_weeks, fill = period)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(title = "Average Unemployment Duration by Education Level",
       subtitle = "Pre-recession vs. Great Recession vs. COVID-19",
       x = NULL, y = "Mean weeks unemployed",
       caption = "Source: CPS, ages 25-54, DURUNEMP, weighted by WTFINL") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
p6
ggsave("plots/plot6_duration_by_education.png", p6, width = 11, height = 6, dpi = 300)

# ---- Done ------------------------------------------------------------------------
message("All plots saved to the 'plots/' folder.")

