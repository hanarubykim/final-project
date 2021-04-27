library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(ggdist)
library(broom.mixed)
library(ggthemes)
library(gt)
library(gtsummary)
library(nycgeo)
library(sf)
library(patchwork)
library(webshot)


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

test_data <- data %>%
  select(-district) %>%
  pivot_longer(cols = g3_ela:g8_math,
               names_to = "test",
               values_to = "proficiency")

# Combining the two datasets of demographic data and academic data for
# manipulation

combined_data <- full_join(data, race_data, by = c("district" = "administrative_district")) %>%
  drop_na()

# From a scatterplot, we may be able to assume a relationship with academic
# proficiency performance by students across all racial groups in relation to
# the racial makeup of their schools

combined_data %>%
  ggplot(mapping = aes(x = percent_black, y = g8_math, 
                       color = subgroup)) +
  geom_point()

# With these plots, we can see whether or not the racial makeup of a district
# affects how students of different racial groups perform within that district

district_asian <- combined_data %>%
  filter(!subgroup == "All Students") %>%
  ggplot(mapping = aes(x = percent_asian, y = g8_math, 
                       color = subgroup)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Asian Students",
       x = "Percentage of Asian Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = subgroup), method = "lm", formula =  'y ~ x', se = FALSE)

district_black <- combined_data %>%
  filter(!subgroup == "All Students") %>%
  ggplot(mapping = aes(x = percent_black, y = g8_math, 
                       color = subgroup)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Black Students",
       x = "Percentage of Black Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = subgroup), method = "lm", formula =  'y ~ x', se = FALSE)

district_white <- combined_data %>%
  filter(!subgroup == "All Students") %>%
  ggplot(mapping = aes(x = percent_white, y = g8_math, 
                       color = subgroup)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of White Students",
       x = "Percentage of White Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = subgroup), method = "lm", formula =  'y ~ x', se = FALSE)

district_hispanic <- combined_data %>%
  filter(!subgroup == "All Students") %>%
  ggplot(mapping = aes(x = percent_asian, y = g8_math, 
                       color = subgroup)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Hispanic Students",
       x = "Percentage of Hispanic Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = subgroup), method = "lm", formula =  'y ~ x', se = FALSE)

district_na <- combined_data %>%
  filter(!subgroup == "All Students") %>%
  ggplot(mapping = aes(x = percent_asian, y = g8_math, 
                       color = subgroup)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Native American Students",
       x = "Percentage of Native American Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = subgroup), method = "lm", formula =  'y ~ x', se = FALSE)

# Looking at overall proficiencies for districts when mapping

map_data <- data %>%
  filter(subgroup == "All Students")

ela_map <- nyc_boundaries(geography = "school") %>%
  
  # Joining based on district with provided geography
  
  left_join(map_data, by = c("school_dist_id" = "district")) %>%
  ggplot() +
  geom_sf(aes(fill = g8_ela * 100)) +
  scale_fill_viridis_c() +
  labs(title = "English Language Arts Exam",
       caption = "Source: NYSED (2018) and NYC DOE (2018)",
       fill = "Percent Proficient") +
  theme_void()

math_map <- nyc_boundaries(geography = "school") %>%
  
  # Joining based on district with provided geography
  
  left_join(map_data, by = c("school_dist_id" = "district")) %>%
  ggplot() +
  geom_sf(aes(fill = g8_math * 100)) +
  scale_fill_viridis_c() +
  labs(title = "Math Exam",
       fill = "Percent Proficient") +
  theme_void()

# Combine the two plots and add titles for the combined plots

final_map <- ela_map + math_map +
  plot_annotation(title = "Average District Proficiency For 8th Graders on State Examinations",
                  caption = "Source: NYSED (2018) and NYC DOE (2018)")
