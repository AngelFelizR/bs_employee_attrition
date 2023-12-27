# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl


# Transformed data -----

cost_summary_tbl <-
  train_raw_tbl |>
  count(Department, JobRole, Attrition) |>
  left_join(productivity_cost_by_role_tbl,
            by = c("Department", "JobRole")) |>
  filter(Attrition == "Yes") |>
  mutate(
    Total_Cost = calculate_attrition_cost(
      n = n,
      salary = Salary_Average,
      net_revenue_per_employee = Revenue_Average
    )
  )

# Summary by role -----

cost_summary_tbl |>
  arrange(desc(Total_Cost)) |>
  transmute(
    Department,
    JobRole,
    Total_Cost,
    cum_pct = cumsum(Total_Cost)/sum(Total_Cost)
  )


# Summary by department ----

cost_summary_tbl |>
  group_by(Department) |>
  summarize(Total_Cost = sum(Total_Cost)) |>
  arrange(desc(Total_Cost)) |>
  transmute(
    Department,
    Total_Cost,
    cum_pct = cumsum(Total_Cost)/sum(Total_Cost)
  )

# Q1: Which Job Role has the highest total cost of attrition? ----
# Sales Executive

# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----
# $2,276,492

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
# 86.1%

# Q4. Which Department has the highest total cost of attrition? ----
# Sales

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----
# 50.1%

