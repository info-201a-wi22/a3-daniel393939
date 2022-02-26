# Initially source and filter original dataset

require(scales)
source("helper_functions.R")

website <- paste("https://raw.githubusercontent.com/vera-institute/",
                 "incarceration-trends/master/incarceration_trends.csv",
                 sep = "")
incarceration <- read.csv(website, stringsAsFactors = F)
incarceration_ny <- incarceration %>%
  filter(state == "NY")
states <- read.csv("states.csv")

#-------------------------------------------------------------------------------

# 1) Summary Variables

# Vector of races focused on in analysis
races <- c("black", "white", "latinx", "aapi")

# a) What is the average annual number of prison inmates across all races?

all_races <- sapply(c(races, "native", "other_race"),
                    paste, "_prison_pop", sep = "")

avg_prison_pop <- incarceration %>%
  compile_by_race(all_races) %>%
  summarise(average = mean(average)) %>%
  pull() %>%
  round(0)

# b) Which race has the highest / lowest average annual number of
#    prison inmates?

prison_pop_cols <- sapply(races, paste, "_prison_pop", sep = "")

prison_pop_by_race <- compile_by_race(incarceration_ny, prison_pop_cols) %>%
  arrange(desc(average))
  
highest_avg_prison_by_race <- prison_pop_by_race %>%
  filter(average == max(average)) %>%
  race_and_value(0)

lowest_avg_prison_by_race <- prison_pop_by_race %>%
  filter(average == min(average)) %>%
  race_and_value(0)

# c) Which race has the highest / lowest average annual proportion of prison
#    inmates to race population 15 ~ 64?

races_pop_15_64 <- sapply(races, paste, "_pop_15to64", sep = "")
pop_15_64_by_race <- compile_by_race(incarceration_ny, races_pop_15_64) %>%
  rename(avg_pop = average)

prison_to_pop_prop_by_race <- prison_pop_by_race %>%
  rename(avg_inmate = average) %>%
  inner_join(pop_15_64_by_race, by = c("race")) %>%
  mutate(prop = avg_inmate / avg_pop) %>%
  select(race, prop) %>%
  arrange(desc(race))

highest_prison_to_pop_prop <- prison_to_pop_prop_by_race %>%
  filter(prop == max(prop)) %>%
  race_and_value(2)

lowest_prison_to_pop_prop <- prison_to_pop_prop_by_race %>%
  filter(prop == min(prop)) %>%
  race_and_value(2)


# d) Which race has the highest / lowest average annual prison influx rate?

adm_races <- paste(races, "_prison_adm_rate", sep = "")

prison_adm_by_race <- compile_by_race(incarceration_ny, adm_races) %>%
  arrange(desc(average))

highest_influx_by_race <- prison_adm_by_race %>%
  filter(average == max(average)) %>%
  race_and_value(0)
  
lowest_influx_by_race <- prison_adm_by_race %>%
  filter(average == min(average)) %>%
  race_and_value(0)

# e) What proportion of counties have an average annual proportion of prison
#    black inmates to white inmates of less than 1 ?

total_counties <- incarceration_ny %>%
  summarize(n_distinct(county_name)) %>%
  pull() %>%
  as.numeric()

more_black_counties_prison <- incarceration_ny %>%
  group_by(county_name) %>%
  summarise(prop = mean(black_prison_pop, na.rm = T) /
              mean(white_prison_pop, na.rm = T)) %>%
  filter(prop < 1) %>%
  summarise(count = n()) %>%
  pull() %>%
  as.numeric()

black_white_prison_above_1 <- more_black_counties_prison / total_counties

# f) What race has the most / least unfilled values from the prison_pop by
#    race columns?

missing_values <- incarceration_ny %>%
  select(all_of(prison_pop_cols)) %>%
  gather(key = race, value = prison_pop) %>%
  filter(is.na(prison_pop)) %>%
  group_by(race) %>%
  summarise(missing_values = n()) %>%
  arrange(desc(missing_values))

most_missing_values <- missing_values %>%
  filter(missing_values == max(missing_values)) %>%
  race_and_value(0)

least_missing_values <- missing_values %>%
  filter(missing_values == min(missing_values)) %>%
  race_and_value(0)

#-------------------------------------------------------------------------------

# 2) Time Series: Average prison inmates each year (by race)

time_series_data <- incarceration_ny %>%
  select(year, all_of(prison_pop_cols)) %>%
  group_by(year) %>%
  summarize_all(mean, na.rm = T) %>%
  filter_at(vars(-year), all_vars(. > 0)) %>%
  gather(key = race, value = count, -year)

time_series_data$race <- factor(time_series_data$race,
                                levels = c("black", "white", "latinx", "aapi"))

prison_by_race_over_time <- ggplot(data = time_series_data) +
  similar_plot_structure() +
  geom_line(aes(x = year, y = count, group = race, color = race)) +
  labs(title = "How has the mean prison\npopulation changed over time?",
       x = "Year",
       y = "Mean Prison Population") +
  scale_x_continuous(breaks = seq(1995, 2015, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 500), labels = comma) +
  scale_color_hue(labels = c("Black", "White", "Latino", "Asian")) +
  labs(color = "Race") +
  theme(plot.title = element_text(size = 19))

#-------------------------------------------------------------------------------

# 3) Scatter Plot: Relationship between jail inmates and prison inmates
#                  (by race)?

jail_pop_cols <- sapply(races, paste, "_jail_pop", sep = "") %>%
  unname()

prison_data <- incarceration_ny %>%
  select(year, total_pop, all_of(c(prison_pop_cols))) %>%
  group_by(year) %>%
  summarise_all(mean, na.rm = T) %>%
  rename_at(vars(-year, -total_pop), paste_helper) %>%
  gather(key = "race", value = "prison_pop", -year, -total_pop)

jail_data <- incarceration_ny %>%
  select(year, total_pop, all_of(c(jail_pop_cols))) %>%
  group_by(year) %>%
  summarise_all(mean, na.rm = T) %>%
  gather(key = "race", value = "jail_pop", -year, -total_pop)

scatter_data <- prison_data %>%
  inner_join(jail_data, by = c("total_pop", "race")) %>%
  filter_at(vars(jail_pop, prison_pop), all_vars(!is.na(.)))

scatter_data$race <- factor(scatter_data$race,
                            levels = c("black_jail_pop",
                                       "white_jail_pop",
                                       "latinx_jail_pop",
                                       "aapi_jail_pop"))

jail_and_prison_correlation <- ggplot(scatter_data) +
  geom_point(aes(x = prison_pop, y = jail_pop, color = race)) +
  similar_plot_structure() +
  scale_color_hue(labels = c("Black", "White", "Latino", "Asian")) +
  labs(title =
         "How is prison population correlated\nwith jail population by race?",
       x = "Mean Prison Population (by Year)",
       y = "Mean Jail Population (by Year)",
       color = "Race") +
  scale_x_continuous(breaks = seq(0, 1000, by = 500), labels = comma) +
  scale_y_continuous(breaks = seq(0, 300, by = 100), labels = comma) +
  theme(plot.title = element_text(size = 19))

#-------------------------------------------------------------------------------

# 4) Map: Proportion of black prison inmates to white inmates in all states

prop_data <- incarceration %>%
  select(state, black_prison_pop, white_prison_pop) %>%
  group_by(state) %>%
  summarise_all(mean, na.rm = T) %>%
  mutate(black_white_prop = black_prison_pop / white_prison_pop) %>%
  na.omit()

state_shape <- map_data("state")

map_data <- prop_data %>%
  inner_join(states, by = c("state" = "Code")) %>%
  mutate(State = tolower(State)) %>%
  right_join(state_shape, by = c("State" = "region"))

map <- ggplot(map_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = black_white_prop),
               color = "white", size = 0.2) +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(title = paste("Mean Proportion of Black to White\n",
                     "Prison Inmates by State (1970 ~ 2018)", sep = ""),
       fill = "Ratio") +
  coord_quickmap() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                                  margin = margin(0, 0, 10, 0))
  )
