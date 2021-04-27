library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
# library(rstanarm)
library(ggdist)
library(broom.mixed)
library(ggthemes)
library(gt)
library(gtsummary)


# table will be the starting framework containing the relevant subgroups so that
# I can join data across grades from their respective sheets from the Excel

table <- read_excel(path = "raw_data/NYC_GEOG_DIST_1.xlsx",
                    sheet = 3,
                    skip = 1) %>%
  clean_names() %>%
  select(subgroup)

# filepaths contains the list of paths needed to access the data of all 32
# districts

filepaths = list("raw_data/NYC_GEOG_DIST_1.xlsx",
                 "raw_data/NYC_GEOG_DIST_2.xlsx",
                 "raw_data/NYC_GEOG_DIST_3.xlsx",
                 "raw_data/NYC_GEOG_DIST_4.xlsx",
                 "raw_data/NYC_GEOG_DIST_5.xlsx",
                 "raw_data/NYC_GEOG_DIST_6.xlsx",
                 "raw_data/NYC_GEOG_DIST_7.xlsx",
                 "raw_data/NYC_GEOG_DIST_8.xlsx",
                 "raw_data/NYC_GEOG_DIST_9.xlsx",
                 "raw_data/NYC_GEOG_DIST_10.xlsx",
                 "raw_data/NYC_GEOG_DIST_11.xlsx",
                 "raw_data/NYC_GEOG_DIST_12.xlsx",
                 "raw_data/NYC_GEOG_DIST_13.xlsx",
                 "raw_data/NYC_GEOG_DIST_14.xlsx",
                 "raw_data/NYC_GEOG_DIST_15.xlsx",
                 "raw_data/NYC_GEOG_DIST_16.xlsx",
                 "raw_data/NYC_GEOG_DIST_17.xlsx",
                 "raw_data/NYC_GEOG_DIST_18.xlsx",
                 "raw_data/NYC_GEOG_DIST_19.xlsx",
                 "raw_data/NYC_GEOG_DIST_20.xlsx",
                 "raw_data/NYC_GEOG_DIST_21.xlsx",
                 "raw_data/NYC_GEOG_DIST_22.xlsx",
                 "raw_data/NYC_GEOG_DIST_23.xlsx",
                 "raw_data/NYC_GEOG_DIST_24.xlsx",
                 "raw_data/NYC_GEOG_DIST_25.xlsx",
                 "raw_data/NYC_GEOG_DIST_26.xlsx",
                 "raw_data/NYC_GEOG_DIST_27.xlsx",
                 "raw_data/NYC_GEOG_DIST_28.xlsx",
                 "raw_data/NYC_GEOG_DIST_29.xlsx",
                 "raw_data/NYC_GEOG_DIST_30.xlsx",
                 "raw_data/NYC_GEOG_DIST_31.xlsx",
                 "raw_data/NYC_GEOG_DIST_32.xlsx")

grade_proficiency <- function(filepaths, table){

  # Accessing data from all 32 NYC Districts

  for(i in 1:length(filepaths)){

    # Accessing data for the performance of grades 3-8 for every district grade
    # 3 starts on the second sheet of every Excel

    # leaving out sheets 1 and 8 for irrelevant data

    for(grade in c(2,3,4,5,6,7,9,10,11,12,13,14)){

      # Will use grade_num to label the columns by respective grade and allow
      # for cleaner merging later with respective district data
      # Separating data on ELA tests and math tests with suffix in column name

      if(grade < 8){
        grade_num = paste("g", grade + 1, "_ela", sep = "")
      } else {
        grade_num = ifelse(grade == 8, "", paste("g", grade - 6, "_math", sep = ""))
      }

      # Accessing data from specific grade

      information <- read_excel(path = paste(filepaths[i]),
                                sheet = grade,
                                skip = 1) %>%
        clean_names() %>%

        # percent_13 contains the data of a grade for the percentage of each
        # racial group that scored "proficient" on the state exam

        select(subgroup, percent_13) %>%
        rename(!!grade_num := percent_13)

      if(grade == 2){
        current_table <- table %>%
          left_join(information, by = "subgroup")
      } else {
        current_table <- current_table %>%
          left_join(information, by = "subgroup")
      }

    }
    current_table <- current_table %>%
      mutate(district = paste(i)) %>%
      filter(subgroup %in% c("All Students", "American Indian or Alaska Native", "Asian or Native Hawaiian/Other Pacific Islander", "Black or African American",
                             "Hispanic or Latino", "White", "Multiracial"))

    if(i == 1){
      new_table <- current_table
    } else {
      new_table <- new_table %>%
        full_join(current_table)
    }
  }
  return(new_table)
}

data <- grade_proficiency(filepaths, table) %>%
  filter(across(everything(), ~ !grepl("—", .))) %>%
  filter(!subgroup == "Multiracial") %>%
  
  # Dropping NA's now to avoid calculation errors later on
  
  drop_na() %>%
  
  # Mutating the character percentages to be recognizes as doubles for
  # calculating purposes
  
  mutate_each(funs(as.numeric(gsub("%", "", ., fixed = TRUE))/100), -c(subgroup, district))


# Gathering racial makeup data for later analysis

race_data <- read_excel(path = "raw_data/demographic-snapshot.xlsx",
                        sheet = 4) %>%
  clean_names() %>%
  
  # Looking at the relevant 2017-2018 demographic data, which represents the
  # same students who took the 2018 test
  
  filter(year == "2017-18") %>%
  
  # Only need to see the district and percentages for each racial group
  
  select(administrative_district, percent_asian:percent_white) %>%
  
  # Removing the preceding "0" from districts 1-9 for easier joining of data
  
  mutate(administrative_district = ifelse(administrative_district < 10, sub('.', '', administrative_district), administrative_district))


# Finding how the average proficiency on tests by race group change over time
# and where it divulges from the average and from other race groups

ela_test_plot <- data %>%
  
  # Grouping so that I can calculate average proficiencies based on racial group
  
  group_by(subgroup) %>%
  summarize_all(mean) %>%
  select(subgroup:g8_ela) %>%
  ungroup() %>%
  mutate(subgroup = as.factor(subgroup)) %>%
  
  # Trying to pivot wider to have subgroups and consequent grade scores
  
  pivot_longer(names_to = "grade_test",
               values_to = "proficiency",
               cols = g3_ela:g8_ela) %>%
  ggplot(mapping = aes(x = grade_test,
                       y = proficiency,
                       group = subgroup,
                       color = subgroup)) +
  geom_line() +
  scale_x_discrete(labels = c("3", "4", "5", "6", "7", "8")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Average Proficiencies on 2018 ELA Tests by Grade",
       subtitle = "",
       x = "Grade",
       y = "Proficiency",
       caption = "Source: NYSED (2018)",
       fill = "Racial Subgroup")

math_test_plot <- data %>%
  
  # Grouping so that I can calculate average proficiencies based on racial group
  
  group_by(subgroup) %>%
  summarize_all(mean) %>%
  select(subgroup, g3_math:g8_math) %>%
  ungroup() %>%
  mutate(subgroup = as.factor(subgroup)) %>%
  
  # Trying to pivot wider to have subgroups and consequent grade scores
  
  pivot_longer(names_to = "grade_test",
               values_to = "proficiency",
               cols = g3_math:g8_math) %>%
  ggplot(mapping = aes(x = grade_test,
                       y = proficiency,
                       group = subgroup,
                       color = subgroup)) +
  geom_line() +
  scale_x_discrete(labels = c("3", "4", "5", "6", "7", "8")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Average Proficiencies on 2018 Math Tests by Grade",
       subtitle = "",
       x = "Grade",
       y = "Proficiency",
       caption = "Source: NYSED (2018)",
       fill = "Racial Subgroup")

# test_data <- data %>%
#   select(-district) %>%
#   pivot_longer(cols = g3_ela:g8_math,
#                names_to = "test",
#                values_to = "proficiency")
#
# fit_1 will model (racial subgroup) as a factor of proficiency
# 
# fit_1 <- stan_glm(data = test_data,
#                       formula = proficiency ~ subgroup,
#                       family = gaussian,
#                       seed = 3,
#                       refresh = 0)

# Saving fitted model to use in my regression table in shiny app without
# refitting it live
#
# saveRDS(fit_1, "shiny_app/fit_1.rds")


saved_fit_1 <- readRDS("shiny_app/fit_1.rds")

table_1 <- tbl_regression(saved_fit_1, 
               intercept = TRUE,
               estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  
  as_gt() %>%
  
  # Formatting the table neatly and intuitively
  
  tab_header(title = md("**Predicting Proficiency on State Exams**"),
             subtitle = "How Racial Subgroup Affects Predicted Academic Proficiency") %>%
  tab_source_note(md("Source: NYSED (2018)")) %>% 
  cols_label(estimate = md("**Parameter**"))

# newobs = tibble(subgroup = c("All Students", "American Indian or Alaska
# Native","Asian or Native Hawaiian/Other Pacific Islander", "Black or African
# American", "Hispanic or Latino", "White", "All Students"))
#
# pe <- posterior_epred(fit_1, newdata = newobs) %>% as_tibble() %>%
# set_names(newobs$subgroup)
#
# plot_data <- pe %>% pivot_longer(names_to = c("Subgroup"), values_to =
# "Proficiency", cols = everything()) %>% filter(!Subgroup == "All Students")
#
# racial_prediction <- plot_data %>% ggplot(aes(x = Proficiency, y =
# fct_reorder(Subgroup, Proficiency), fill = "black")) +
# scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
# stat_slab() + theme_light() + theme(legend.position = "none") + labs(title =
# "Looking at Race as an Indication of Performance on State Test Results", x =
# "Proficiency Rates", y = "", caption = "Source: NYSED (2018)", fill =
# "Subgroup")
#
# ggsave("racial_group_pred.png", racial_prediction, width = 10, height = 7)
#
# grade_data <- data %>% select(-district) %>% pivot_longer(cols =
# g3_ela:g8_math,
#
# # Manipulating data to be able to be able to consider grade as a # factor
#
# names_to = c("grade", "test"), names_sep = "_", values_to = "proficiency")
#
#
# # fit_2 will allow us to look at proficiency as a factor of both grade and #
# racial subgroup
#
# fit_2 <- stan_glm(data = grade_data, formula = proficiency ~ grade*subgroup,
# family = gaussian, seed = 3, refresh = 0)

# grade <- c(3, 4, 5, 6, 7, 8)
# subgroup <- c("All Students", "American Indian or Alaska Native","Asian or Native Hawaiian/Other Pacific Islander", "Black or African American", "Hispanic or Latino", "White", "All Students")
# 
# newobs_3 <- expand_grid(grade, subgroup) %>%
#   mutate(names = paste(grade, subgroup, sep = "_"))
# 
# pe_3 <- posterior_epred(fit_2,
#                         newdata = newobs_3) %>%
#   as_tibble() %>%
#   set_names(newobs_3$names)
# 
# plot_data_3 <- pe_3 %>%
#   pivot_longer(names_to = c("Grade", "Subgroup"),
#                names_sep = "_",
#                values_to = "Proficiency",
#                cols = everything()) %>%
#   filter(!Subgroup == "All Students")
# 
# 
# grade_pred <- plot_data_3 %>%
#   ggplot(aes(x = Proficiency, y = fct_reorder(Subgroup, Proficiency),
#              fill = Grade)) +
#   stat_slab() +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(title = "Expected Peformance on State Test Results Over Time",
#        x = "Proficiency Rates",
#        y = "",
#        caption = "Source: NYSED (2018)") +
#   theme_light()
# 
# ggsave("grade_prediction.png", grade_pred, width = 10, height = 7)

combined_data <- full_join(data, race_data, by = c("district" = "administrative_district")) %>%
  drop_na()

# From a scatterplot, we may be able to assume a relationship with academic
# proficiency performance by students across all racial groups in relation to
# the racial makeup of their schools

combined_data %>%
  ggplot(mapping = aes(x = percent_black, y = g8_math, 
                       color = subgroup)) +
  geom_point()


# With this model, we can see whether or not the predominant racial makeup of a
# district actually does affect how students of a racial group perform within
# that district
# 
# fit_3 <- stan_glm(data = combined_data,
#                   formula = g8_math ~ percent_black*subgroup,
#                   family = gaussian,
#                   seed = 3,
#                   refresh = 0)


