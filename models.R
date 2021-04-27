library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(ggdist)
library(broom.mixed)
library(ggthemes)
library(gt)
library(gtsummary)
library(webshot)
library(rstanarm)

# Need to access cleaned data from data.R

source(file = "data.R")

# fit_1 will model (racial subgroup) as a factor of proficiency

fit_1 <- stan_glm(data = test_data,
                      formula = proficiency ~ subgroup,
                      family = gaussian,
                      seed = 3,
                      refresh = 0)

# Creating a regression table to see the parameter values and observe the model

table_1 <- tbl_regression(fit_1,
                          intercept = TRUE,
                          estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  
  as_gt() %>%
  
  # Formatting the table neatly and intuitively
  
  tab_header(title = md("**Predicting Proficiency Percentage of Racial Groups on State Exams**"),
             subtitle = "How Racial Subgroup Affects Predicted Academic Proficiency") %>%
  tab_source_note(md("Source: NYSED (2018)")) %>%
  cols_label(estimate = md("**Parameter**"))

# While saving my fit as an RDS file and creating the table would be preferable,
# doing so causes a deployment error. Using gtsave as an alternative.

gtsave(table_1, "shiny_app/table_1.png")

newobs = tibble(subgroup = c("All Students", "American Indian or Alaska Native","Asian or Native Hawaiian/Other Pacific Islander", "Black or African
American", "Hispanic or Latino", "White", "All Students"))

# Creating a posterior using the model to look at how race is a predictive
# factor of performance on state test results

pe <- posterior_epred(fit_1, newdata = newobs) %>% 
  as_tibble() %>%
  set_names(newobs$subgroup)

plot_data <- pe %>% 
  pivot_longer(names_to = c("Subgroup"), 
               values_to = "Proficiency", 
               cols = everything()) %>%
  filter(!Subgroup == "All Students")

racial_prediction <- plot_data %>% 
  ggplot(aes(x = Proficiency, y = fct_reorder(Subgroup, Proficiency), fill = "black")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  stat_slab() + theme_light() + theme(legend.position = "none") + 
  labs(title = "Looking at Race as an Indication of Performance on State Test Results", 
       x = "Proficiency Rates", 
       y = "", 
       caption = "Source: NYSED (2018)", 
       fill ="Subgroup")

# Saving plot of fitted model as image

ggsave("racial_group_pred.png", racial_prediction, width = 10, height = 7)

grade_data <- data %>% 
  select(-district) %>% 
  pivot_longer(cols = g3_ela:g8_math,

# Manipulating data to be able to be able to consider grade as a # factor

names_to = c("grade", "test"), names_sep = "_", values_to = "proficiency")

# fit_2 will allow us to look at proficiency as a factor of both grade and 
# racial subgroup

fit_2 <- stan_glm(data = grade_data, 
                  formula = proficiency ~ grade*subgroup,
                  family = gaussian, 
                  seed = 3, 
                  refresh = 0)

grade <- c(3, 4, 5, 6, 7, 8)
subgroup <- c("All Students", "American Indian or Alaska Native","Asian or Native Hawaiian/Other Pacific Islander", "Black or African American", "Hispanic or Latino", "White", "All Students")

newobs_3 <- expand_grid(grade, subgroup) %>%
  mutate(names = paste(grade, subgroup, sep = "_"))

# This posterior will show us the distributions for the likelihood of a racial
# group achieving certain percentages of proficiency relative to their grade

pe_3 <- posterior_epred(fit_2,
                        newdata = newobs_3) %>%
  as_tibble() %>%
  set_names(newobs_3$names)

plot_data_3 <- pe_3 %>%
  pivot_longer(names_to = c("Grade", "Subgroup"),
               names_sep = "_",
               values_to = "Proficiency",
               cols = everything()) %>%
  filter(!Subgroup == "All Students")

grade_pred <- plot_data_3 %>%
  ggplot(aes(x = Proficiency, y = fct_reorder(Subgroup, Proficiency),
             fill = Grade)) +
  stat_slab() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Expected Peformance on State Test Results Over Time",
       x = "Proficiency Rates",
       y = "",
       caption = "Source: NYSED (2018)") +
  theme_light()

# Saving plot of model as image

ggsave("grade_prediction.png", grade_pred, width = 10, height = 7)