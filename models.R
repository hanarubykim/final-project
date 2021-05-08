library(tidyverse)
library(openxlsx)
library(ggdist)
library(broom.mixed)
library(gt)
library(gtsummary)
library(webshot)
library(rstanarm)

# Need to access cleaned data from data.R

source(file = "data.R")

# Creating a predictive model for ELA test results while considering grade as a
# factor

fit_3 <- stan_glm(pred_test,
                  formula = ela_proficient ~ category*grade,
                  family = gaussian,
                  seed = 3,
                  refresh = 0)

category <- c("Asian", "Black", "Hispanic", "White")
grade <- c(3, 4, 5, 6, 7, 8)

# Creating a regression table to see the parameter values and observe the model

table_1 <- tbl_regression(fit_3,
                          intercept = TRUE,
                          estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  
  as_gt() %>%
  
  # Formatting the table neatly and intuitively
  
  tab_header(title = md("**Predicting ELA Proficiency Percentage of Racial Groups on State Exams**"),
             subtitle = "How Racial Subgroup Affects Predicted Proficiency for Each Grade") %>%
  tab_source_note(md("Source: NYCED (2019)")) %>%
  cols_label(estimate = md("**Parameter**"))

# While saving my fit as an RDS file and creating the table would be preferable,
# doing so causes a deployment error. Using gtsave as an alternative.

gtsave(table_1, "table_1.png")

newobs_3 <- expand_grid(category, grade) %>%
  mutate(names = paste(category, grade, sep = "_"))

# This posterior will show us the distributions for the likelihood of a racial
# group achieving certain percentages of proficiency relative to their grade

pe_3 <- posterior_epred(fit_3,
                        newdata = newobs_3) %>%
  as_tibble() %>%
  set_names(newobs_3$names)

# Making the posterior distributions more easily accessible when plotting

plot3_data <- pe_3 %>%
  pivot_longer(names_to = c("Category", "Grade"),
               names_sep = "_",
               values_to = "Proficiency",
               cols = everything())

# Plotting the posteriors

plot_3 <- plot3_data %>%
  ggplot(aes(x = Proficiency, y = fct_reorder(Category, Proficiency),
             fill = Grade)) +
  stat_slab() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Expected Peformance on ELA State Test Results Over Time by Racial Subgroup",
       x = "Proficiency Rates",
       y = "",
       caption = "Source: NYCED (2019)") +
  theme_light()

ggsave("overall_ela.png", plot_3, width = 8, height = 5)

# Creating a predictive model for math test results while considering grade as a
# factor

fit_4 <- stan_glm(pred_test,
                  formula = math_proficient ~ category*grade,
                  family = gaussian,
                  seed = 3,
                  refresh = 0)

# Creating a regression table to see the parameter values and observe the model

table_2 <- tbl_regression(fit_4,
                          intercept = TRUE,
                          estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  
  as_gt() %>%
  
  # Formatting the table neatly and intuitively
  
  tab_header(title = md("**Predicting Math Proficiency Percentage of Racial Groups on State Exams**"),
             subtitle = "How Racial Subgroup Affects Predicted Proficiency for Each Grade") %>%
  tab_source_note(md("Source: NYCED (2019)")) %>%
  cols_label(estimate = md("**Parameter**"))

# While saving my fit as an RDS file and creating the table would be preferable,
# doing so causes a deployment error. Using gtsave as an alternative.

gtsave(table_2, "table_2.png")

pe_4 <- posterior_epred(fit_4,
                        
                        # Using the same tibble because we are looking at the
                        # same interaction of types of racial subgroups and
                        # grades as for the ELA test predictive model
                        
                        newdata = newobs_3) %>%
  as_tibble() %>%
  set_names(newobs_3$names)

# Making the posterior distributions more easily accessible when plotting

plot4_data <- pe_4 %>%
  pivot_longer(names_to = c("Category", "Grade"),
               names_sep = "_",
               values_to = "Proficiency",
               cols = everything())

# Plotting the posteriors

plot_4 <- plot4_data %>%
  ggplot(aes(x = Proficiency, y = fct_reorder(Category, Proficiency),
             fill = Grade)) +
  stat_slab() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Expected Peformance on Math State Test Results Over Time by Racial Subgroup",
       x = "Proficiency Rates",
       y = "",
       caption = "Source: NYCED (2019)") +
  theme_light()

ggsave("overall_math.png", plot_4, width = 8, height = 5)
