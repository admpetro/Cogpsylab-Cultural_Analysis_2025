library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)

# Import csv files from github
codes <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/codes.csv")
data <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/data.csv")
societies <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/societies.csv")
variables <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/variables.csv")

# Create the dataset
df = data

# Pivot the dataset so that each society has a single line, and only keep the relevant columns
df <- df %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  select(soc_id, EA034, EA066, EA067)

# Count the number of occurrences and the percentage of each value in EA067 
EA067_counts <- df %>%
  group_by(EA067) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
View(EA067_counts)

# Delete the column of EA067 as it is not informative
df <- df %>%
  select(-EA067)

# Omit rows where either value is NA
df <- na.omit(df)

# Lookup table for EA034
lookup_EA034 <- codes %>% filter(var_id == "EA034") %>% select(code, name)
colnames(lookup_EA034) <- c("EA034", "High god")

# Lookup table for EA066
lookup_EA066 <- codes %>% filter(var_id == "EA066") %>% select(code, name)
colnames(lookup_EA066) <- c("EA066", "Class differentiation")

# Replace the values in EA034 and EA066 with the corresponding names
df2 <- df %>%
  left_join(lookup_EA034, by = "EA034") %>%
  left_join(lookup_EA066, by = "EA066") %>%
  select(soc_id, `High god`, `Class differentiation`)

# Count the number of occurrences of each value in Class differentiation
class_counts <- df2 %>%
  group_by(`Class differentiation`) %>%
  summarise(count = n())
View(class_counts)

# Relevant codes
codes_filtered <- codes %>% filter(var_id == "EA034" | var_id == "EA066")
View(codes_filtered)

# Set the ordering of the categories according to the code order
class_counts$`Class differentiation` <- factor(class_counts$`Class differentiation`, levels = c(
  "Absence of distinctions",
  "Wealth distinctions",
  "Elite stratification",
  "Dual stratification",
  "Complex stratification"
))

# Create a bar chart of the number of societies in each category of Class differentiation
bar_plot_class <- ggplot(class_counts, aes(x = `Class differentiation`, y = count, fill = `Class differentiation`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of examined societies among categories of Class differentiation",
       x = "Class differentiation",
       y = "Number of societies") +
  theme(plot.title = element_text(size = 12, hjust = 0.5),     # Centered,
        axis.text.x = element_text(angle = 40, hjust = 0.9)
        ) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  theme(legend.position = "none")

print(bar_plot_class)

# Save the plot as a PNG file
ggsave("bar_plot_class.png", plot = bar_plot_class, width = 6, height = 2.8, units = "in")


# Set the order of categories and groups in reverse order, because the coord_flip will inverse them
df2$`High god` <- factor(df2$`High god`, levels = c(
  "Active, supporting morality",
  "Active, but not supporting morality",
  "Otiose",
  "Absent"
))

df2$`Class differentiation` <- factor(df2$`Class differentiation`, levels = c(
  "Complex stratification",
  "Dual stratification",
  "Elite stratification",
  "Wealth distinctions",
  "Absence of distinctions"
))

# Horizontal bar chart of df2, displaying the percentage of different High god beliefs for each category of Class differentiation
horizontal_plot_god <- ggplot(df2, aes(x = `Class differentiation`, fill = `High god`)) +
  geom_bar(
    position = "fill",
    width = 0.7
    ) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = c("0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%")
    ) +    
  theme(
    panel.grid.major.x = element_line(color = "grey80"), # Major grid lines
    panel.grid.minor.x = element_blank()                 # No minor grid lines
    ) +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order back to original
  labs(title = "Conception of High god in relation to Class differentiation in society",
       x = "Class differentiation",
       y = "Percentage of societies") +
  scale_fill_brewer(palette = "Set1") +
  #display horizontal labels in percentage
  theme(plot.title = element_text(size = 12, hjust = 0.5),     # Centered,
    axis.text.x = element_text(angle = 0)
    )

print(horizontal_plot_god)

# Save the plot as a PNG file
ggsave("horizontal_plot_god.png", plot = horizontal_plot_god, width = 6, height = 2.5, units = "in")


# Create new dataset
df3 = data

# Pivot the dataset so that each society has a single line, and only keep the relevant columns
df3 <- df3 %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  select(soc_id, EA034, EA066, EA068, EA070)

# Keep only the rows where the value of EA066, EA068, and EA070 is 1 (no class/caste/slavery)
df3 <- df3 %>%
  filter(EA066 == 1 & EA068 == 1 & EA070 == 1) %>% 
  na.omit() %>% 
  select(-EA068, -EA070)

# Prepare for display
df3 <- df3 %>%
  left_join(lookup_EA034, by = "EA034") %>%
  mutate(EA066 = "No class/caste/slavery") %>%
  # replace header
  rename("Class differentiation" = EA066) %>% 
  select(soc_id, `High god`, `Class differentiation`)

df3$`High god` <- factor(df3$`High god`, levels = c(
  "Active, supporting morality",
  "Active, but not supporting morality",
  "Otiose",
  "Absent"
  ))

# Horizontal bar chart of df3, displaying the percentage of different High god beliefs
horizontal_plot_limited <- ggplot(df3, aes(x = `Class differentiation`, fill = `High god`)) +
  geom_bar(
    position = "fill",
    width = 0.23
  ) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = c("0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%")
  ) +    
  theme(
    panel.grid.major.x = element_line(color = "grey80"), # Major grid lines
    panel.grid.minor.x = element_blank()                 # No minor grid lines
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order back to original
  labs(title = "Conception of High god in societies without social classes, caste system, slavery",
       x = "",
       y = "Percentage of societies") +
  scale_fill_brewer(palette = "Set1") +
  #display horizontal labels in percentage
  theme(plot.title = element_text(size = 12, hjust = 0.38),     # Centered,
        axis.text.x = element_text(angle = 0)
  )

print(horizontal_plot_limited)

# Save the plot as a PNG file
ggsave("horizontal_plot_limited.png", plot = horizontal_plot_limited, width = 6, height = 2, units = "in")


# Appendix
# From the societies dataset, match the id to the soc_id in df2, and get the Latitude and Longitude of the societies in df2
df2 <- df2 %>%
  left_join(societies %>% select(id, Lat, Long), by = c("soc_id" = "id"))

# From the societies dataset, match the id to the soc_id in df3, and get the Latitude and Longitude of the societies in df3
df3 <- df3 %>%
  left_join(societies %>% select(id, Lat, Long), by = c("soc_id" = "id"))

# Map creator function
create_map_plot <- function(data, 
                            variable, 
                            palette, 
                            title,
                            hjust,
                            reverse_legend = TRUE,
                            point_size = 0.2) {
  # Get world map data
  world_map <- map_data("world")
  
  # Create plot
  ggplot() +
    # World map background
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "gray95", color = "gray60", linewidth = 0.2
    ) +
    # Societies as colored dots
    geom_point(
      data = data,
      aes(x = Long, y = Lat, color = {{ variable }}),
      size = point_size
    ) +
    scale_color_brewer(palette = palette) +
    theme_minimal() +
    labs(
      title = title,
      color = deparse(substitute(variable))
    ) +
    coord_fixed(1.3) +
    theme(plot.title = element_text(size = 12, hjust = hjust),
          axis.title = element_blank(),
          axis.text = element_blank()
    ) +
    guides(color = guide_legend(
      reverse = reverse_legend,
      override.aes = list(size = point_size*4)
    ))
}

# Create the maps
map_plot_class <- create_map_plot(df2, `Class differentiation`, "Paired", "Class differentiation in examined societies", 0.25)
map_plot_god <- create_map_plot(df2, `High god`, "Set1", "Conception of High god in examined societies", 0.25)
map_plot_limited <- create_map_plot(df3, `High god`, "Set1", "Conception of High god in societies without social classes, caste system, slavery", 0)

# Print the maps
print(map_plot_class)
print(map_plot_god)
print(map_plot_limited)

# Save the maps as PNG files
ggsave("map_plot_class.png", plot = map_plot_class, width = 6.5, height = 2.7, units = "in")
ggsave("map_plot_god.png", plot = map_plot_god, width = 6.5, height = 3, units = "in")
ggsave("map_plot_limited.png", plot = map_plot_limited, width = 6.5, height = 3, units = "in")

# Save the used dataframes to csv files
write.csv(df2, "examined_societies.csv", row.names = FALSE)
write.csv(df3, "examined_societies_limited.csv", row.names = FALSE)
