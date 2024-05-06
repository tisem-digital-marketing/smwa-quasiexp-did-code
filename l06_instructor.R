#' l06_instructor.R
#'
#' Code from the slide of Lecture 6:
#'    Difference in Differences
#'    
#' from Social Media and Web Analytics, Spring 2024

library(readr)
library(dplyr)
library(infer)
library(ggplot2)
library(tidyr)
library(broom)
library(estimatr)
library(modelsummary)

# --- Load data --- #
df <- read_csv("data/diff_in_diff.csv")
head(df)

# --- Cross Section Design --- #
# after period
cross_section <-
    df %>%
    filter(after == 1)

cross_section_est <- cross_section %>%
    # group means
    group_by(treatment_group) %>%
    summarize(
        mean_sales = mean(sales)
    ) %>%
    # difference between groups
    ungroup() %>%
    mutate(estimate = mean_sales - lag(mean_sales)) %>%
    na.omit() %>%
    purrr::pluck("estimate")

message("Cross Section Estimate: ", cross_section_est)

tidy(lm(sales ~ treatment_group, 
        data = cross_section))

## --- Before - After Design --- #
# treatment group, period immediately before and after
before_after <-
    df %>%
    filter(time_period %in% c(10,11),
           treatment_group == "treatment")

before_after_est <- before_after %>%
    group_by(after) %>%
    dplyr::summarize(
        mean_sales = mean(sales)
    ) %>%
    ungroup() %>%
    mutate(estimate = mean_sales - lag(mean_sales)) %>%
    na.omit() %>%
    purrr::pluck("estimate")

message("Before After Estimate: ", before_after_est)

tidy(lm(sales ~ after, 
        data = before_after))


# --- Difference in Differences Design --- # 
# did data -- treat and control, before and after
did_data <- 
    df %>% 
    filter(time_period %in% c(10,11))

did_est <- did_data %>%
    group_by(treatment_group, after) %>%
    summarise(mean_sales = mean(sales)) %>%
    ungroup() %>%
    pivot_wider(names_from = after, 
                values_from = mean_sales) %>%
    mutate(across_rows_diff = `1` - `0`) %>%
    mutate(estimate = across_rows_diff - lag(across_rows_diff)) %>%
    na.omit() %>%
    purrr::pluck("estimate")

print(paste0("Diff in Diff Estimate: ", did_est))

tidy(lm(sales ~ treatment_group + after + treatment_group:after, 
        data = did_data
        ))

# --- Parallel Trends --- #
df %>%
    filter(after == 0) %>%
    group_by(treatment_group, time_period) %>%
    dplyr::summarise(mean_sales = mean(sales)) %>%
    ggplot() +
    geom_line(aes(x = time_period, y = mean_sales, color = treatment_group)) +
    theme_bw()


# --- CUPED Comparison --- #
cuped_df <- 
    did_data %>%
    tidyr::pivot_wider(id_cols = customer_id,
                       names_from = after,
                       names_prefix = "time_",
                       values_from = c(sales, treatment_group)
    )

theta <- 
    tidy(lm(sales_time_1 ~ sales_time_0, data = cuped_df)) %>%
    filter(term=="sales_time_0") %>%
    select(estimate) %>%
    purrr::pluck('estimate')

#print(theta)

cuped_df <- 
    cuped_df %>%
    mutate(cuped_spend = sales_time_1 - 
               theta*(sales_time_0 - mean(cuped_df$sales_time_0)
               )
    )

mod_cuped <- tidy(lm(cuped_spend ~ treatment_group_time_0, 
                data = cuped_df))

#print(mod_cuped)

cuped_est <-
    mod_cuped %>%
    filter(term == "treatment_group_time_0treatment") %>%
    purrr::pluck("estimate")

message("CUPED estimate: ", round(cuped_est,4))


