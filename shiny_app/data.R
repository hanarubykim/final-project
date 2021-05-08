library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(ggdist)
library(broom.mixed)


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

# Creating a function to easily compile the data from 32 different files during
# 2018 to see overall proficincies for each district

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

# Cleaning the data

map_data <- grade_proficiency(filepaths, table) %>%
  filter(across(everything(), ~ !grepl("—", .))) %>%
  filter(!subgroup == "Multiracial") %>%
  
  # Dropping NA's now to avoid calculation errors later on
  
  drop_na() %>%
  
  # Mutating the character percentages to be recognizes as doubles for
  # calculating purposes
  
  mutate_each(funs(as.numeric(gsub("%", "", ., fixed = TRUE))/100), -c(subgroup, district)) %>%
  
  # Looking at overall proficiencies for districts when mapping
  
  filter(subgroup == "All Students")


# Cleaning data of ELA test score results

ela_data <- read_excel(path = "raw_data/district-ela-results-2013-2019-(public).xlsx",
                       
                       # Sheet 4 accesses data by district
                       
                       sheet = 4) %>%
  
  clean_names() %>%
  
  # Dropping NA's now to avoid calculation errors later on
  
  drop_na() %>%
  
  # Mutating the percentages for easier axes scale manipulation later and
  # changing column names to distinguish tests when joining data sets of
  # different tests
  
  mutate(ela_proficient = percent_level_3_4 / 100) %>%
  
  # Selecting relevant data columns
  
  select(!number_tested:percent_level_3_4)


# Cleaning data of math test score results

math_data <- read_excel(path = "raw_data/district-math-results-2013-2019-(public).xlsx",
                        
                        # Sheet 4 accesses data by district
                        
                        sheet = 4) %>%
  
  clean_names() %>%
  
  # Dropping NA's now to avoid calculation errors later on
  
  drop_na() %>%
  
  # Mutating the percentages for easier axes scale manipulation later and
  # changing column names to distinguish tests when joining data sets of
  # different tests
  
  mutate(math_proficient = percent_level_3_4 / 100) %>%
  
  # Selecting relevant data columns
  
  select(!number_tested:percent_level_3_4)


# Combining test data frames

test_data <- full_join(ela_data, math_data) %>%
  drop_na()


# Cleaning demographic data of districts

demographic_data <- read_excel(path = "raw_data/demographic-snapshot-2015-16-to-2019-20-(public).xlsx",
                               
                               # Sheet 4 accesses data by district
                               
                               sheet = 4) %>%
  clean_names() %>%
  
  # Dropping NA's now to avoid calculation errors later on
  
  drop_na() %>%
  mutate(
    
    # Removing the preceding "0" from districts 1-9 for easier joining of data
    
    administrative_district = as.numeric(ifelse(administrative_district < 10, sub('.', '', administrative_district), administrative_district)),
    
    # Benchmark exams are taken in the latter half of the academic year, so I
    # will mutate the school years to reflect the proper testing year to
    # correspond with test data.
    
    year = as.numeric(paste("20", substr(year, 6,7), sep = ""))) %>%
  
  # Grades here are not relevant for analysis because the demographics are for
  # the overall school, not by grade
  
  select(!grade_3k_pk_half_day_full_day:grade_12) %>%
  
  # Matching column name of year to test_data
  
  rename(district = administrative_district)

# Combining full test data with demographic data
data <- full_join(test_data, demographic_data, by = c("district", "year")) %>%
  
  # Only have the relevant demographic data beginning from 2016, so need to drop
  # rows with NA's (i.e. test data for years before 2016)
  
  drop_na() %>%
  
  # Will be considering grade as a factor later, will convert to numeric now
  
  filter(!grade == "All Grades") %>%
  mutate(grade = as.numeric(grade))

# Condensing the data for animation, which will visualize the overall citywide
# subgroup proficiency rates by year

animation_data <- test_data %>%
  group_by(year, category) %>%
  
  # Using 'all grades' to see overall proficiency of racial subgroup within a
  # district
  
  filter(grade == "All Grades") %>%
  
  # Using the mean of proficiency by racial subgroup across districts
  
  summarize(ela_proficient = mean(ela_proficient),
            math_proficient = mean(math_proficient),
            .groups = "drop")

# test_data has relevant information from 2013, as opposed to data (which
# combines demographic and test data) which begins from 2016; using test_data
# when demographic data is not relevant for fitting models

pred_test <- test_data %>%
  
  # Recognizing grade as a number, but must filter All Grades first because it
  # will be converted to NA because of the type conversion
  
  filter(!grade == "All Grades") %>%
  mutate(grade = as.numeric(grade))


