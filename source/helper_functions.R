require(scales)

# Calculates the mean value by race for particular columns in 'race_columns'
compile_by_race <- function(data, race_columns) {
  data %>%
    select(all_of(race_columns)) %>%
    gather(key = race, value = count) %>%
    group_by(race) %>%
    summarise(average = mean(count, na.rm = T))
}

# Pastes "jail_pop" to given string 'string' (used for joining tables)
paste_helper <- function(string) {
  paste(string, "_jail_pop", sep = "")
}

# Gets the 'race' and 'value' from a given dataframe 'data' consisting of one
# row, returns the two elements in a vector. Value is rounded to 'digit' digits.
race_and_value <- function(data, digit) {
  c(pull(data[, 1]), round(pull(data[, 2]), digit))
}

# Returns given value 'x' by multiplied by 365
# (used to account for daily average populations)
annualise <- function(x) {
  x * 365
}

# Takes given string 'race' and returns a formalised version
race_nameify <- function(race) {
  if (str_detect(race, "aapi")) {
    return("Asian")
  } else if (str_detect(race, "black")) {
    return("Black")
  } else if (str_detect(race, "white")) {
    return("White")
  } else {
    return("Latino")
  }
}

# Returns a ggplot theme value shared by multiple plots
similar_plot_structure <- function() {
  theme(plot.title = element_text(hjust = 0.5, face = "bold",
                                  margin = margin(0, 0, 10, 0)),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x =
          element_text(size = 15, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y =
          element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        panel.border =
          element_rect(colour = "black", fill = NA, size = 1.5),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"))
}
