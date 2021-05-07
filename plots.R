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
library(gganimate)
library(gifski)

source(file = "shiny_app/data.R")

# Creating maps to visualize overall proficiencies for districts

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

# Visualizing the overall citywide subgroup proficiency rates by year

ela_animation_plot <- animation_data %>%
  ggplot(mapping = aes(x = ela_proficient,
                       y = fct_reorder(category, ela_proficient),
                       fill = as.factor(category))) +
  geom_col() +
  theme_light() +
  transition_states(states = year) +
  ease_aes("cubic-in-out") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ELA Benchmark Exam Year: {closest_state}",
       x = "Proficiency",
       y = "",
       fill = "Racial Subgroup")

# Saving as gif file because Shiny cannot render gif objects
# anim_save("ela_animation.gif", animate(ela_animation_plot, width = 950, height = 750, renderer = gifski_renderer()))

math_animation_plot <- animation_data %>%
  ggplot(mapping = aes(x = math_proficient,
                       y = fct_reorder(category, math_proficient),
                       fill = as.factor(category))) +
  geom_col() +
  theme_light() +
  transition_states(states = year) +
  ease_aes("cubic-in-out") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Math Benchmark Exam Year: {closest_state}",
       x = "Proficiency",
       y = "",
       fill = "Racial Subgroup")

# Saving as gif file because Shiny cannot render gif objects
# anim_save("math_animation.gif", animate(math_animation_plot, width = 950, height = 750, renderer = gifski_renderer()))


district_asian <- data %>%
  filter(!grade == "All Students") %>%
  ggplot(mapping = aes(x = percent_asian, y = math_proficient, 
                       color = category)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Asian Students",
       x = "Percentage of Asian Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = category), method = "lm", formula =  'y ~ x', se = FALSE)

district_black <- data %>%
  filter(!grade == "All Students") %>%
  ggplot(mapping = aes(x = percent_black, y = math_proficient, 
                       color = category)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Black Students",
       x = "Percentage of Black Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = category), method = "lm", formula =  'y ~ x', se = FALSE)

district_white <- data %>%
  filter(!grade == "All Students") %>%
  ggplot(mapping = aes(x = percent_white, y = math_proficient, 
                       color = category)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of White Students",
       x = "Percentage of White Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = category), method = "lm", formula =  'y ~ x', se = FALSE)

district_hispanic <- data %>%
  filter(!grade == "All Students") %>%
  ggplot(mapping = aes(x = percent_hispanic, y = math_proficient, 
                       color = category)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proficiency Percentages of Racial Subgroups Relative to Percentage of Hispanic Students",
       x = "Percentage of Hispanic Students in District",
       y = "Proficiency Rate of Subgroup",
       caption = "Sources: NYSED (2018) and NYC DOE (2018)",
       color = "Racial Subgroup") +
  theme_light() +
  geom_smooth(mapping = aes(group = category), method = "lm", formula =  'y ~ x', se = FALSE)