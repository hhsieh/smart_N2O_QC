library(readxl)
all_gases_data <- read_excel("S:/UTK_project/Database management/Data_reformatting/Aug12/Data folder/data-smart_n2o_project_fluxes.xlsx")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
library(htmlwidgets)


# Ensure 'gas' and 'treatment' columns are treated as factors for proper grouping in ggplot
all_gases_data$gas <- as.factor(all_gases_data$gas)
all_gases_data$treatment_name <- as.factor(all_gases_data$treatment_name)
all_gases_data$sample_date <- as.Date(all_gases_data$sample_date)


# Explore unique site names
unique_sites <- unique(all_gases_data$site)
print(unique_sites)

# Empty list to store the data frames
site_data_list <- list()

# Loop through each unique site and filter the data
for (site_name in unique_sites) {
  # Filter the data for the current site
  site_data <- all_gases_data %>%
    filter(site == site_name)
  
# Store the filtered data in the list with the site name as the key
  site_data_list[[site_name]] <- site_data
}
# Now, each site's data is stored separately in site_data_list
# Loop through the list and save each data frame to a file
for (site_name in names(site_data_list)) {
  write.csv(site_data_list[[site_name]], paste0(site_name, "_data.csv"), row.names = FALSE)
}

# Loop through each site's data and create plots
#for (site_name in names(site_data_list)) {
  # Extract the data for the current site
  site_data <- site_data_list[[site_name]]
  
  # Spread the data to have separate columns for each gas
  site_data_spread <- site_data %>%
    spread(key = gas, value = flux)
#separating various sites
site1_data <- all_gases_data %>% filter(site == "Hermitage Research Station")
site2_data <- all_gases_data %>% filter(site == "Tamworth Agricultural Institute")
site3_data <- all_gases_data %>% filter(site == "Cunderdin Agriculture College, Cunderdin")
site4_data <- all_gases_data %>% filter(site == "Shenyang Experimental Station of the Institute of Applied Ecology, Chinese Academy of Sciences")
site5_data <- all_gases_data %>% filter(site == "Wagga Wagga Agricultural Institute, Wagga Wagga, NSW 2650 Australia")
site6_data <- all_gases_data %>% filter(site == "Ingham in north Queensland, Australia")
site7_data <- all_gases_data %>% filter(site == "KBS GLBRC, Hickory Corners, MI, USA")
site8_data <- all_gases_data %>% filter(site == "Biomass & Environment experiment, EstrÃ©es-Mons, France")
site9_data <- all_gases_data %>% filter(site == "Kelly North, Iowa State Research Farm, Ames Iowa")


#1## Plot for Hermitage Research Station
# Example for site 1, repeat similarly for other sites
# plot type: time series vs flux 
site1_name <- "Hermitage Research Station"
site1_data_spread <- site1_data %>%
  spread(key = gas, value = flux)

plot1 <- ggplot(site1_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Hermitage Research Station",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(plot1)
#plot type 2: CH4, CO2, and N2O separately
# Plot for CH4
plot_CH4 <- ggplot(site1_data_spread, aes(x = sample_date, y = `CH4`)) + 
  geom_line(color = "blue", group = 1) +
  labs(title = paste("CH4 Fluxes at", site1_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)") +
  theme_minimal()

# Plot for CO2
plot_CO2 <- ggplot(site1_data_spread, aes(x = sample_date, y = `CO2`)) + 
  geom_line(color = "green", group = 1) +
  labs(title = paste("CO2 Fluxes at", site1_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)") +
  theme_minimal()

# Plot for N2O
plot_N2O <- ggplot(site1_data_spread, aes(x = sample_date, y = `N2O`)) + 
  geom_line(color = "red", group = 1) +
  labs(title = paste("N2O Fluxes at", site1_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)") +
  theme_minimal()

# Display each plot
print(plot_CH4)
print(plot_CO2)
print(plot_N2O)

# plot type3: time series vs flux affected by treatments
#plot for CH4 vs treatment
plot_CH4 <- ggplot(site1_data_spread, aes(x = sample_date, y = `CH4`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CH4 Fluxes at", site1_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_CH4)
#plot for CO2 vs treatment
plot_CO2 <- ggplot(site1_data_spread, aes(x = sample_date, y = `CO2`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CO2 Fluxes at", site1_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_CO2)
#plot for N2O vs treatmements
plot_N2O <- ggplot(site1_data_spread, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site1_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 4: Box plots vs treatment

# Box plot for CH4
plot_CH4_box <- ggplot(site1_data_spread, aes(x = treatment_name, y = `CH4`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("CH4 Fluxes at", site1_name),
       x = "Treatment",
       y = "CH4 Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for CO2
plot_CO2_box <- ggplot(site1_data_spread, aes(x = treatment_name, y = `CO2`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("CO2 Fluxes at", site1_name),
       x = "Treatment",
       y = "CO2 Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for N2O
plot_N2O_box <- ggplot(site1_data_spread, aes(x = treatment_name, y = `N2O`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("N2O Fluxes at", site1_name),
       x = "Treatment",
       y = "N2O Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots
print(plot_CH4_box)
print(plot_CO2_box)
print(plot_N2O_box)

#plot type 5: interactive plots

# plot for CH4 vs treatment
plot_CH4 <- ggplot(site1_data_spread, aes(x = sample_date, y = `CH4`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CH4 Fluxes at", site1_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_CH4_interactive <- ggplotly(plot_CH4)

# Display the interactive plot
plot_CH4_interactive

# plot for CO2 vs treatment
plot_CO2 <- ggplot(site1_data_spread, aes(x = sample_date, y = `CO2`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CO2 Fluxes at", site1_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_CO2_interactive <- ggplotly(plot_CO2)

# Display the interactive plot
plot_CO2_interactive

# plot for N2O vs treatments
plot_N2O <- ggplot(site1_data_spread, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site1_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_CH4_interactive, "C:/Users/shiba/Downloads/CH4_interactice_emissions.html")
saveWidget(plot_CO2_interactive, "C:/Users/shiba/Downloads/CO2_interactive_emissions.html")
saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")

#plot type 6: Crop wise daily cumulative

# Calculate cumulative sums for each crop
# Calculate cumulative sums for each crop and date
# Ensure that flux values are not missing or zero
site1_data_spread <- site1_data_spread %>%
  filter(!is.na(CH4) & !is.na(CO2) & !is.na(N2O)) %>%
  filter(CH4 != 0 | CO2 != 0 | N2O != 0)

# Calculate cumulative emissions over time for each gas
site1_data_cumulative <- site1_data_spread %>%
  arrange(sample_date) %>%
  mutate(
    cumulative_CH4 = cumsum(CH4),
    cumulative_CO2 = cumsum(CO2),
    cumulative_N2O = cumsum(N2O)
  )
# Plot cumulative CH4 over time
plot_CH4_cumulative <- ggplot(site1_data_cumulative, aes(x = sample_date, y = cumulative_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Daily Cumulative CH4 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Cumulative CH4 Emissions (g/ha)") +
  theme_minimal()

# Plot cumulative CO2 over time
plot_CO2_cumulative <- ggplot(site1_data_cumulative, aes(x = sample_date, y = cumulative_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Daily Cumulative CO2 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Cumulative CO2 Emissions (g/ha)") +
  theme_minimal()

# Plot cumulative N2O over time
plot_N2O_cumulative <- ggplot(site1_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Cumulative N2O Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

# Display the plots
print(plot_CH4_cumulative)
print(plot_CO2_cumulative)
print(plot_N2O_cumulative)
#plot type 7: Crop wise weekly sum trend
# Create a new column for the week number
site1_data_spread <- site1_data_spread %>%
  mutate(week = floor_date(sample_date, unit = "week"))
# Calculate the weekly sum for each gas
site1_data_weekly <- site1_data_spread %>%
  group_by(week) %>%
  summarise(
    weekly_sum_CH4 = sum(CH4, na.rm = TRUE),
    weekly_sum_CO2 = sum(CO2, na.rm = TRUE),
    weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )
# Plot weekly sum for CH4 over time
plot_CH4_weekly <- ggplot(site1_data_weekly, aes(x = week, y = weekly_sum_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Weekly CH4 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Weekly CH4 Emissions (g/ha)") +
  theme_minimal()

# Plot weekly sum for CO2 over time
plot_CO2_weekly <- ggplot(site1_data_weekly, aes(x = week, y = weekly_sum_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Weekly CO2 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Weekly CO2 Emissions (g/ha)") +
  theme_minimal()

# Plot weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site1_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Display the plots
print(plot_CH4_weekly)
print(plot_CO2_weekly)
print(plot_N2O_weekly)

#plot type 7: interactive weekly sum trend
# Ensure the sample_date is in Date format
#site1_data_spread$sample_date <- as.Date(site1_data_spread$sample_date)

# Create a new column for the week number
site1_data_spread <- site1_data_spread %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site1_data_weekly <- site1_data_spread %>%
  group_by(week) %>%
  summarise(
    weekly_sum_CH4 = sum(CH4, na.rm = TRUE),
    weekly_sum_CO2 = sum(CO2, na.rm = TRUE),
    weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for CH4 over time
plot_CH4_weekly <- ggplot(site1_data_weekly, aes(x = week, y = weekly_sum_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Weekly CH4 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Weekly CH4 Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_CH4_weekly_interactive <- ggplotly(plot_CH4_weekly)

# Plot interactive weekly sum for CO2 over time
plot_CO2_weekly <- ggplot(site1_data_weekly, aes(x = week, y = weekly_sum_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Weekly CO2 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Weekly CO2 Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_CO2_weekly_interactive <- ggplotly(plot_CO2_weekly)

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site1_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_CH4_weekly_interactive
plot_CO2_weekly_interactive
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_CH4_weekly_interactive, "C:/Users/shiba/Downloads/CH4_weekly_emissions.html")
saveWidget(plot_CO2_weekly_interactive, "C:/Users/shiba/Downloads/CO2_weekly_emissions.html")
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")

#plot type 8: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site1_data_spread <- site1_data_spread %>%
  filter(!is.na(CH4) & !is.na(CO2) & !is.na(N2O)) %>%
  filter(CH4 != 0 | CO2 != 0 | N2O != 0)

# Calculate cumulative emissions over time for each gas
site1_data_cumulative <- site1_data_spread %>%
  arrange(sample_date) %>%
  mutate(
    cumulative_CH4 = cumsum(CH4),
    cumulative_CO2 = cumsum(CO2),
    cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative CH4 over time and make it interactive
plot_CH4_cumulative <- ggplot(site1_data_cumulative, aes(x = sample_date, y = cumulative_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Daily Cumulative CH4 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Cumulative CH4 Emissions (g/ha)") +
  theme_minimal()

interactive_plot_CH4 <- ggplotly(plot_CH4_cumulative)

# Plot cumulative CO2 over time and make it interactive
plot_CO2_cumulative <- ggplot(site1_data_cumulative, aes(x = sample_date, y = cumulative_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Daily Cumulative CO2 Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Cumulative CO2 Emissions (g/ha)") +
  theme_minimal()

interactive_plot_CO2 <- ggplotly(plot_CO2_cumulative)

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site1_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site1_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_CH4
interactive_plot_CO2
interactive_plot_N2O
#ggsave(filename = paste0("Fluxes_at_", site1_name, ".png"), plot = plot1)
# Save the interactive plot
saveWidget(interactive_plot_CH4, "C:/Users/shiba/Downloads/CH4_daily_cumulative_emissions.html")
saveWidget(interactive_plot_CO2, "C:/Users/shiba/Downloads/CO2_daily_cumulative_emissions.html")
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")


#2## Plot for Tamworth Agricultural Institute
site2_name <- "Tamworth Agricultural Institute"
site2_data_wide <- site2_data %>%
  pivot_wider(names_from = gas, values_from = flux)

plot2 <- ggplot(site2_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Tamworth Agricultural Institute",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(plot2)
#Warning message:
#Values from flux are not uniquely identified; output will contain list-cols.
#ggsave(filename = paste0("Fluxes_at_", site2_name, ".png"), plot = plot2)

duplicates <- site2_data %>%
  dplyr::group_by(site, longitude, latitude, sample_date, treatment_name,
                  replicate_name, crop, fertilization, tillage, nitrogen_inhibitor, irrigation,
                  datetime, chamber, chamber_temperature_C, gas) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

# View the duplicates
print(duplicates)
#solution 1 (taking mean of the duplicate)
site2_name <- "Tamworth"
site2_data_wide <- site2_data %>%
  pivot_wider(names_from = gas, values_from = flux, 
              values_fn = list(flux = mean))
plot2 <- ggplot(site2_data_wide, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Tamworth Agricultural Institute",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

#ggsave(filename = paste0("Fluxes_at_", site2_name, ".png"), plot = plot2)
print(plot2)

#plot type 2: CH4, CO2, and N2O separately
# Plot for CH4
plot_CH4 <- ggplot(site2_data_wide, aes(x = sample_date, y = `CH4`)) + 
  geom_line(color = "blue", group = 1) +
  labs(title = paste("CH4 Fluxes at", site2_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)") +
  theme_minimal()

# Plot for CO2
plot_CO2 <- ggplot(site2_data_wide, aes(x = sample_date, y = `CO2`)) + 
  geom_line(color = "green", group = 1) +
  labs(title = paste("CO2 Fluxes at", site2_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)") +
  theme_minimal()

# Plot for N2O
plot_N2O <- ggplot(site2_data_wide, aes(x = sample_date, y = `N2O`)) + 
  geom_line(color = "red", group = 1) +
  labs(title = paste("N2O Fluxes at", site2_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)") +
  theme_minimal()

# Display each plot
print(plot_CH4)
print(plot_CO2)
print(plot_N2O)

# plot type3: time series vs flux affected by treatments
#plot for CH4 vs treatment
plot_CH4 <- ggplot(site2_data_wide, aes(x = sample_date, y = `CH4`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CH4 Fluxes at", site2_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_CH4)
#plot for CO2 vs treatment
plot_CO2 <- ggplot(site2_data_wide, aes(x = sample_date, y = `CO2`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CO2 Fluxes at", site2_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_CO2)
#plot for N2O vs treatmements
plot_N2O <- ggplot(site2_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site2_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 4: interactive plots

# plot for CH4 vs treatment
site2_name <- "Tamworth Agricultural Institute"
plot_CH4 <- ggplot(site2_data_wide, aes(x = sample_date, y = `CH4`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CH4 Fluxes at", site2_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_CH4_interactive <- ggplotly(plot_CH4)

# Display the interactive plot
plot_CH4_interactive
saveWidget(plot_CH4_interactive, "C:/Users/shiba/Downloads/CH4_interactice_emissions.html")

# plot for CO2 vs treatment
site2_name <- "Tamworth Agricultural Institute"
plot_CO2 <- ggplot(site2_data_wide, aes(x = sample_date, y = `CO2`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CO2 Fluxes at", site2_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_CO2_interactive <- ggplotly(plot_CO2)

# Display the interactive plot
plot_CO2_interactive
saveWidget(plot_CO2_interactive, "C:/Users/shiba/Downloads/CO2_interactive_emissions.html")

# plot for N2O vs treatments
plot_N2O <- ggplot(site2_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site2_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")

#plot type 4: Box plots vs treatment

# Box plot for CH4
plot_CH4_box <- ggplot(site2_data_wide, aes(x = treatment_name, y = `CH4`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("CH4 Fluxes at", site2_name),
       x = "Treatment",
       y = "CH4 Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for CO2
plot_CO2_box <- ggplot(site2_data_wide, aes(x = treatment_name, y = `CO2`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("CO2 Fluxes at", site2_name),
       x = "Treatment",
       y = "CO2 Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for N2O
plot_N2O_box <- ggplot(site2_data_wide, aes(x = treatment_name, y = `N2O`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("N2O Fluxes at", site2_name),
       x = "Treatment",
       y = "N2O Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots
print(plot_CH4_box)
print(plot_CO2_box)
print(plot_N2O_box)

#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site2_data_wide <- site2_data_wide %>%
  filter(!is.na(CH4) & !is.na(CO2) & !is.na(N2O)) %>%
  filter(CH4 != 0 | CO2 != 0 | N2O != 0)

# Calculate cumulative emissions over time for each gas
site2_data_cumulative <- site2_data_wide %>%
  arrange(sample_date) %>%
  mutate(
    cumulative_CH4 = cumsum(CH4),
    cumulative_CO2 = cumsum(CO2),
    cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative CH4 over time and make it interactive
plot_CH4_cumulative <- ggplot(site2_data_cumulative, aes(x = sample_date, y = cumulative_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Daily Cumulative CH4 Emissions Over Time at", site2_name),
       x = "Time (year)",
       y = "Cumulative CH4 Emissions (g/ha)") +
  theme_minimal()

interactive_plot_CH4 <- ggplotly(plot_CH4_cumulative)

# Plot cumulative CO2 over time and make it interactive
plot_CO2_cumulative <- ggplot(site2_data_cumulative, aes(x = sample_date, y = cumulative_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Daily Cumulative CO2 Emissions Over Time at", site2_name),
       x = "Time (year)",
       y = "Cumulative CO2 Emissions (g/ha)") +
  theme_minimal()

interactive_plot_CO2 <- ggplotly(plot_CO2_cumulative)

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site2_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site2_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_CH4
interactive_plot_CO2
interactive_plot_N2O
saveWidget(interactive_plot_CH4, "C:/Users/shiba/Downloads/CH4_daily_cumulative_emissions.html")
saveWidget(interactive_plot_CO2, "C:/Users/shiba/Downloads/CO2_daily_cumulative_emissions.html")
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site2_data_wide <- site2_data_wide %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site2_data_weekly <- site2_data_wide %>%
  group_by(week) %>%
  summarise(
    weekly_sum_CH4 = sum(CH4, na.rm = TRUE),
    weekly_sum_CO2 = sum(CO2, na.rm = TRUE),
    weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for CH4 over time
plot_CH4_weekly <- ggplot(site2_data_weekly, aes(x = week, y = weekly_sum_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Weekly CH4 Emissions Over Time at", site2_name),
       x = "Time (year)",
       y = "Weekly CH4 Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_CH4_weekly_interactive <- ggplotly(plot_CH4_weekly)

# Plot interactive weekly sum for CO2 over time
plot_CO2_weekly <- ggplot(site2_data_weekly, aes(x = week, y = weekly_sum_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Weekly CO2 Emissions Over Time at", site2_name),
       x = "Time (year)",
       y = "Weekly CO2 Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_CO2_weekly_interactive <- ggplotly(plot_CO2_weekly)

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site2_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site2_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_CH4_weekly_interactive
plot_CO2_weekly_interactive
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_CH4_weekly_interactive, "C:/Users/shiba/Downloads/CH4_weekly_emissions.html")
saveWidget(plot_CO2_weekly_interactive, "C:/Users/shiba/Downloads/CO2_weekly_emissions.html")
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")

#ggsave(filename = paste0("Fluxes_at_", site1_name, ".png"), plot = plot1)
# Save the interactive plot

#3###. # Plot for Cunderdin Agriculture College, Cunderdin
site3_name <- "Cunderdin Agriculture College, Cunderdin"
site3_data_spread <- site3_data %>%
  spread(key = gas, value = flux)

plot3 <- ggplot(site3_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Cunderdin Agriculture College",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(plot3)
#ggsave(filename = paste0("Fluxes_at_", site3_name, ".png"), plot = plot3)
#solution 1
site3_data_unique <- site3_data %>%
  distinct(site, longitude, latitude, sample_date, treatment_name,
           replicate_name, crop, fertilization, tillage, nitrogen_inhibitor, irrigation,
           datetime, chamber, chamber_temperature_C, gas, .keep_all = TRUE)
site3_data_wide <- site3_data_unique %>%
  pivot_wider(names_from = gas, values_from = flux)

plot3 <- ggplot(site3_data_wide, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Cunderdin Agriculture College",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

#ggsave(filename = paste0("Fluxes_at_", site3_name, ".png"), plot = plot3)
print(plot3)
#error
#Solution to missing column
#CH4 and CO2 columns are not present
plot3 <- ggplot(site3_data_wide, aes(x = sample_date))

# Add lines for each available gas
available_gases <- names(site3_data_wide)[names(site3_data_wide) %in% c("CH4", "CO2", "N2O")]
print(available_gases)

if ("CH4" %in% available_gases) {
  plot3 <- plot3 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot3 <- plot3 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot3 <- plot3 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Complete the plot with labels and theme
plot3 <- plot3 +
  labs(title = "Fluxes at Cunderdin Agriculture College",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save and display the plot
#ggsave(filename = paste0("Fluxes_at_", site3_name, ".png"), plot = plot3)
print(plot3)

#plot type 2: N2O vs Treatments
#plot for N2O vs treatmements
plot_N2O <- ggplot(site3_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site3_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 2: interactive plots

# plot for N2O vs treatment
site3_name <- "Cunderdin Agriculture College"
# plot for N2O vs treatments
plot_N2O <- ggplot(site3_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site3_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site3_data_wide <- site3_data_wide %>%
  filter(!is.na(N2O)) %>%
  filter(N2O != 0)

# Calculate cumulative emissions over time for each gas
site3_data_cumulative <- site3_data_wide %>%
  arrange(sample_date) %>%
  mutate(cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site3_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site3_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_N2O
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site3_data_wide <- site3_data_wide %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site3_data_weekly <- site3_data_wide %>%
  group_by(week) %>%
  summarise(weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site3_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site3_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")

#4.# Plot for Shenyang Experimental Station of the Institute of Applied Ecology, Chinese Academy of Sciences
site4_name <- "Shenyang Experimental Station of the Institute of Applied Ecology, Chinese Academy of Sciences"
site4_data_spread <- site4_data %>%
  spread(key = gas, value = flux)

plot4 <- ggplot(site4_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Shenyang Experimental Station",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print((plot4))
#ggsave(filename = paste0("Fluxes_at_", site4_name, ".png"), plot = plot4)
#missing CH4 column again
#solution 1
site4_name <- "Shenyang Experimental Station of the Institute of Applied Ecology, Chinese Academy of Sciences"
site4_data_spread <- site4_data %>%
  spread(key = gas, value = flux)

# Identify which gases are available in the data
available_gases <- names(site4_data_spread)[names(site4_data_spread) %in% c("CH4", "CO2", "N2O")]

# Start the ggplot object
plot4 <- ggplot(site4_data_spread, aes(x = sample_date))

# Add lines for each available gas
if ("CH4" %in% available_gases) {
  plot4 <- plot4 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot4 <- plot4 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot4 <- plot4 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Finalize the plot with labels and themes
plot4 <- plot4 +
  labs(title = "Fluxes at Shenyang Experimental Station",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(plot4)
# Convert the ggplot object to an interactive plotly object
interactive_plot4 <- ggplotly(plot4)

# Display the interactive plot
interactive_plot4
saveWidget(interactive_plot4, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")

#plot type 2: N2O vs Treatments
#plot for N2O vs treatments
plot_N2O <- ggplot(site4_data_spread, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site4_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)


#plot type 2: interactive plots

# plot for N2O vs treatment
site4_name <- "Shenyang Experimental Station"
# plot for N2O vs treatments
plot_N2O <- ggplot(site4_data_spread, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site4_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site4_data_spread <- site4_data_spread %>%
  filter(!is.na(N2O)) %>%
  filter(N2O != 0)

# Calculate cumulative emissions over time for each gas
site4_data_cumulative <- site4_data_spread %>%
  arrange(sample_date) %>%
  mutate(cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site4_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site4_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_N2O
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site4_data_spread <- site4_data_spread %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site4_data_weekly <- site4_data_spread %>%
  group_by(week) %>%
  summarise(weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site4_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site4_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")

#5.# Plot for Wagga Wagga Agricultural Institute, Wagga Wagga, NSW 2650 Australia
site5_name <- "Wagga Wagga Agricultural Institute, Wagga Wagga, NSW 2650 Australia"
site5_data_spread <- site5_data %>%
  spread(key = gas, value = flux)

plot5 <- ggplot(site5_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Wagga Wagga Agricultural Institute",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

#ggsave(filename = paste0("Fluxes_at_", site5_name, ".png"), plot = plot5)
#error missing values, incorrect data types, or duplicates spread()
#solution 1

site5_data_wide <- site5_data %>%
  pivot_wider(names_from = gas, values_from = flux)
site5_data_unique <- site5_data %>%
  distinct(site, longitude, latitude, sample_date, treatment_name,
           replicate_name, crop, fertilization, tillage, nitrogen_inhibitor, irrigation,
           datetime, chamber, chamber_temperature_C, gas, .keep_all = TRUE)
site5_data_wide <- site5_data_unique %>%
  pivot_wider(names_from = gas, values_from = flux)
# Identify available gases
available_gases <- names(site5_data_wide)[names(site5_data_wide) %in% c("CH4", "CO2", "N2O")]

# Initialize the plot
plot5 <- ggplot(site5_data_wide, aes(x = sample_date))

# Add lines for each available gas
if ("CH4" %in% available_gases) {
  plot5 <- plot5 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot5 <- plot5 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot5 <- plot5 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Finalize and display the plot
plot5 <- plot5 +
  labs(title = "Fluxes at Wagga Wagga Agricultural Institute",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot5)
#plot type 2: CH4, CO2, and N2O separately
# Plot for CH4
plot_CH4 <- ggplot(site5_data_wide, aes(x = sample_date, y = `CH4`)) + 
  geom_line(color = "blue", group = 1) +
  labs(title = paste("CH4 Fluxes at", site5_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)") +
  theme_minimal()

# Plot for CO2
plot_CO2 <- ggplot(site5_data_wide, aes(x = sample_date, y = `CO2`)) + 
  geom_line(color = "green", group = 1) +
  labs(title = paste("CO2 Fluxes at", site5_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)") +
  theme_minimal()

# Plot for N2O
plot_N2O <- ggplot(site5_data_wide, aes(x = sample_date, y = `N2O`)) + 
  geom_line(color = "red", group = 1) +
  labs(title = paste("N2O Fluxes at", site5_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)") +
  theme_minimal()

# Display each plot
print(plot_CH4)
print(plot_CO2)
print(plot_N2O)
# plot type3: time series vs flux affected by treatments
#plot for CH4 vs treatment
plot_CH4 <- ggplot(site5_data_wide, aes(x = sample_date, y = `CH4`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CH4 Fluxes at", site5_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_CH4)
#plot for CO2 vs treatment
plot_CO2 <- ggplot(site5_data_wide, aes(x = sample_date, y = `CO2`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CO2 Fluxes at", site5_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_CO2)
#plot for N2O vs treatments
plot_N2O <- ggplot(site5_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site5_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 4: Box plots vs treatment

# Box plot for CH4
plot_CH4_box <- ggplot(site5_data_wide, aes(x = treatment_name, y = `CH4`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("CH4 Fluxes at", site5_name),
       x = "Treatment",
       y = "CH4 Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for CO2
plot_CO2_box <- ggplot(site5_data_wide, aes(x = treatment_name, y = `CO2`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("CO2 Fluxes at", site5_name),
       x = "Treatment",
       y = "CO2 Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for N2O
plot_N2O_box <- ggplot(site5_data_wide, aes(x = treatment_name, y = `N2O`, fill = treatment_name)) + 
  geom_boxplot() +
  labs(title = paste("N2O Fluxes at", site5_name),
       x = "Treatment",
       y = "N2O Flux (g/ha/day)",
       fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots
print(plot_CH4_box)
print(plot_CO2_box)
print(plot_N2O_box)

#plot type 2: interactive plots

# plot for CH4 vs treatment
site5_name <- "Wagga Wagga Agricultural Institute"
plot_CH4 <- ggplot(site5_data_wide, aes(x = sample_date, y = `CH4`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CH4 Fluxes at", site5_name),
       x = "Time (year)",
       y = "CH4 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_CH4_interactive <- ggplotly(plot_CH4)

# Display the interactive plot
plot_CH4_interactive
saveWidget(plot_CH4_interactive, "C:/Users/shiba/Downloads/CH4_interactice_emissions.html")

# plot for CO2 vs treatment
site5_name <- "Wagga Wagga Agricultural Institute"
plot_CO2 <- ggplot(site5_data_wide, aes(x = sample_date, y = `CO2`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on CO2 Fluxes at", site5_name),
       x = "Time (year)",
       y = "CO2 Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_CO2_interactive <- ggplotly(plot_CO2)

# Display the interactive plot
plot_CO2_interactive
saveWidget(plot_CO2_interactive, "C:/Users/shiba/Downloads/CO2_interactive_emissions.html")

# plot for N2O vs treatments
plot_N2O <- ggplot(site5_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site5_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site5_data_wide <- site5_data_wide %>%
  filter(!is.na(CH4) & !is.na(CO2) & !is.na(N2O)) %>%
  filter(CH4 != 0 | CO2 != 0 | N2O != 0)

# Calculate cumulative emissions over time for each gas
site5_data_cumulative <- site5_data_wide %>%
  arrange(sample_date) %>%
  mutate(
    cumulative_CH4 = cumsum(CH4),
    cumulative_CO2 = cumsum(CO2),
    cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative CH4 over time and make it interactive
plot_CH4_cumulative <- ggplot(site5_data_cumulative, aes(x = sample_date, y = cumulative_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Daily Cumulative CH4 Emissions Over Time at", site5_name),
       x = "Time (year)",
       y = "Cumulative CH4 Emissions (g/ha)") +
  theme_minimal()

interactive_plot_CH4 <- ggplotly(plot_CH4_cumulative)

# Plot cumulative CO2 over time and make it interactive
plot_CO2_cumulative <- ggplot(site5_data_cumulative, aes(x = sample_date, y = cumulative_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Daily Cumulative CO2 Emissions Over Time at", site5_name),
       x = "Time (year)",
       y = "Cumulative CO2 Emissions (g/ha)") +
  theme_minimal()

interactive_plot_CO2 <- ggplotly(plot_CO2_cumulative)

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site5_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site5_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_CH4
interactive_plot_CO2
interactive_plot_N2O
saveWidget(interactive_plot_CH4, "C:/Users/shiba/Downloads/CH4_daily_cumulative_emissions.html")
saveWidget(interactive_plot_CO2, "C:/Users/shiba/Downloads/CO2_daily_cumulative_emissions.html")
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site5_data_wide <- site5_data_wide %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site5_data_weekly <- site5_data_wide %>%
  group_by(week) %>%
  summarise(
    weekly_sum_CH4 = sum(CH4, na.rm = TRUE),
    weekly_sum_CO2 = sum(CO2, na.rm = TRUE),
    weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for CH4 over time
plot_CH4_weekly <- ggplot(site5_data_weekly, aes(x = week, y = weekly_sum_CH4)) + 
  geom_line(color = "blue") +
  labs(title = paste("Weekly CH4 Emissions Over Time at", site5_name),
       x = "Time (year)",
       y = "Weekly CH4 Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_CH4_weekly_interactive <- ggplotly(plot_CH4_weekly)

# Plot interactive weekly sum for CO2 over time
plot_CO2_weekly <- ggplot(site5_data_weekly, aes(x = week, y = weekly_sum_CO2)) + 
  geom_line(color = "green") +
  labs(title = paste("Weekly CO2 Emissions Over Time at", site5_name),
       x = "Time (year)",
       y = "Weekly CO2 Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_CO2_weekly_interactive <- ggplotly(plot_CO2_weekly)

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site5_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site5_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_CH4_weekly_interactive
plot_CO2_weekly_interactive
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_CH4_weekly_interactive, "C:/Users/shiba/Downloads/CH4_weekly_emissions.html")
saveWidget(plot_CO2_weekly_interactive, "C:/Users/shiba/Downloads/CO2_weekly_emissions.html")
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")

#6.# Plot for Ingham in north Queensland, Australia
site6_name <- "Ingham in north Queensland, Australia"
site6_data_spread <- site6_data %>%
  spread(key = gas, value = flux)

plot6 <- ggplot(site6_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Ingham",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

#ggsave(filename = paste0("Fluxes_at_", site6_name, ".png"), plot = plot6)
#error for non-unique entries, missing data, or incorrect data types.
#solution 1
site6_data_wide <- site6_data %>%
  pivot_wider(names_from = gas, values_from = flux)
site6_data_unique <- site6_data %>%
  distinct(site, longitude, latitude, sample_date, treatment_name,
           replicate_name, crop, fertilization, tillage, nitrogen_inhibitor, irrigation,
           datetime, chamber, chamber_temperature_C, gas, .keep_all = TRUE)
site6_data_wide <- site6_data_unique %>%
  pivot_wider(names_from = gas, values_from = flux)
# Identify available gases
available_gases <- names(site6_data_wide)[names(site6_data_wide) %in% c("CH4", "CO2", "N2O")]

# Initialize the plot
plot6 <- ggplot(site6_data_wide, aes(x = sample_date))

# Add lines for each available gas
if ("CH4" %in% available_gases) {
  plot6 <- plot6 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot6 <- plot6 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot6 <- plot6 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Finalize and display the plot
plot6 <- plot6 +
  labs(title = "Fluxes at Ingham in north Queensland, Australia",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot6)
# Convert to an interactive plot
plot6_interactive <- ggplotly(plot6)

# Display the interactive plot
plot6_interactive
saveWidget(plot6_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 2: N2O vs Treatments
#plot for N2O vs treatments
plot_N2O <- ggplot(site6_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site6_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 2: interactive plots

# plot for N2O vs treatment
site6_name <- "Ingham in north Queensland, Australia"
# plot for N2O vs treatments
plot_N2O <- ggplot(site6_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site6_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site6_data_wide <- site6_data_wide %>%
  filter(!is.na(N2O)) %>%
  filter(N2O != 0)

# Calculate cumulative emissions over time for each gas
site6_data_cumulative <- site6_data_wide %>%
  arrange(sample_date) %>%
  mutate(cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site6_data_cumulative, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site6_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_N2O
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site6_data_wide <- site6_data_wide %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site6_data_weekly <- site6_data_wide %>%
  group_by(week) %>%
  summarise(weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site6_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site6_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")


#7.# Plot for KBS GLBRC, Hickory Corners, MI, USA
site7_name <- "KBS GLBRC, Hickory Corners, MI, USA"
site7_data_spread <- site7_data %>%
  spread(key = gas, value = flux)

plot7 <- ggplot(site7_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at KBS GLBRC",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(plot7)
#ggsave(filename = paste0("Fluxes_at_", site7_name, ".png"), plot = plot7)
#error at site7_spread for missing column
# Handle duplicates if necessary
site7_data_unique <- site7_data %>%
  group_by(site, longitude, latitude, sample_date, treatment_name,
           replicate_name, crop, fertilization, tillage, nitrogen_inhibitor, irrigation,
           datetime, chamber, chamber_temperature_C, gas) %>%
  summarise(flux = mean(flux), .groups = "drop")

# Transform the data into a wide format
site7_data_wide <- site7_data_unique %>%
  pivot_wider(names_from = gas, values_from = flux)

# Check the structure of site7_data_wide
str(site7_data_wide)

# Identify available gases in the data
available_gases <- names(site7_data_wide)[names(site7_data_wide) %in% c("CH4", "CO2", "N2O")]

# Initialize the ggplot object
plot7 <- ggplot(site7_data_wide, aes(x = sample_date))

# Add lines for each available gas
if ("CH4" %in% available_gases) {
  plot7 <- plot7 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot7 <- plot7 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot7 <- plot7 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Finalize the plot with labels and themes
plot7 <- plot7 +
  labs(title = "Fluxes at KBS GLBRC, Hickory Corners, MI, USA",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(plot7)

# Convert to an interactive plot
plot7_interactive <- ggplotly(plot7)

# Display the interactive plot
plot7_interactive
saveWidget(plot7_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")

#plot type 2: N2O vs Treatments
#plot for N2O vs treatments
plot_N2O <- ggplot(site7_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site7_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 2: interactive plots

# plot for N2O vs treatment
site7_name <- "KBS GLBRC, Hickory Corners, MI, USA"
# plot for N2O vs treatments
plot_N2O <- ggplot(site7_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site7_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site7_data_wide <- site7_data_wide %>%
  filter(!is.na(N2O)) %>%
  filter(N2O != 0)

# Calculate cumulative emissions over time for each gas
site7_data_wide <- site7_data_wide %>%
  arrange(sample_date) %>%
  mutate(cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site7_data_wide, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site7_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_N2O
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site7_data_wide <- site7_data_wide %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site7_data_weekly <- site7_data_wide %>%
  group_by(week) %>%
  summarise(weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site7_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site7_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")


##8.# Plot for Biomass & Environment experiment, Estrées-Mons, France
site8_name <- "Biomass & Environment experiment, Estrées-Mons, France"
site8_data_spread <- site8_data %>%
  spread(key = gas, value = flux)

plot8 <- ggplot(site8_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Biomass & Environment Experiment",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot8)
#ggsave(filename = paste0("Fluxes_at_", site8_name, ".png"), plot = plot8)
#errro for missing Ch4 column
site8_data_unique <- site8_data %>%
  distinct(site, longitude, latitude, sample_date, treatment_name,
           replicate_name, crop, fertilization, tillage, nitrogen_inhibitor, irrigation,
           datetime, chamber, chamber_temperature_C, gas, .keep_all = TRUE)
site8_data_wide <- site8_data_unique %>%
  pivot_wider(names_from = gas, values_from = flux)

# Check the structure of site8_data_wide to confirm that it was created correctly
str(site8_data_wide)
# Identify available gases
available_gases <- names(site8_data_wide)[names(site8_data_wide) %in% c("CH4", "CO2", "N2O")]

# Initialize the plot
plot8 <- ggplot(site8_data_wide, aes(x = sample_date))

# Add lines for each available gas
if ("CH4" %in% available_gases) {
  plot8 <- plot8 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot8 <- plot8 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot8 <- plot8 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Finalize the plot with labels and themes
plot8 <- plot8 +
  labs(title = "Fluxes at Biomass & Environment experiment, Estrées-Mons, France",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(plot8)
# Convert to an interactive plot
plot8_interactive <- ggplotly(plot8)

# Display the interactive plot
plot8_interactive

saveWidget(plot8_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")

#plot type 2: N2O vs Treatments
#plot for N2O vs treatments
plot_N2O <- ggplot(site8_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site8_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 2: interactive plots

# plot for N2O vs treatment
site8_name <- "Biomass & Environment experiment, Estrées-Mons, France"
# plot for N2O vs treatments
plot_N2O <- ggplot(site8_data_wide, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site8_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site8_data_wide <- site8_data_wide %>%
  filter(!is.na(N2O)) %>%
  filter(N2O != 0)

# Calculate cumulative emissions over time for each gas
site8_data_wide <- site8_data_wide %>%
  arrange(sample_date) %>%
  mutate(cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site8_data_wide, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site8_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_N2O
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site8_data_wide <- site8_data_wide %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site8_data_weekly <- site8_data_wide %>%
  group_by(week) %>%
  summarise(weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site8_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site8_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")

#9.# Plot for Kelly North, Iowa State Research Farm, Ames Iowa
site9_name <- "Kelly North, Iowa State Research Farm, Ames Iowa"
site9_data_spread <- site9_data %>%
  spread(key = gas, value = flux)

plot9 <- ggplot(site9_data_spread, aes(x = sample_date)) + 
  geom_line(aes(y = `CH4`, color = "CH4", group = 1)) +
  geom_line(aes(y = `CO2`, color = "CO2", group = 1)) +
  geom_line(aes(y = `N2O`, color = "N2O", group = 1)) +
  labs(title = "Fluxes at Kelly North",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(plot9)
#error as CH4 is not present

#ggsave(filename = paste0("Fluxes_at_", site9_name, ".png"), plot = plot9)
# Identify available gases
available_gases <- names(site9_data_spread)[names(site9_data_spread) %in% c("CH4", "CO2", "N2O")]

# Initialize the plot
plot9 <- ggplot(site9_data_spread, aes(x = sample_date))

# Add lines for each available gas
if ("CH4" %in% available_gases) {
  plot9 <- plot9 + geom_line(aes(y = `CH4`, color = "CH4", group = 1))
}
if ("CO2" %in% available_gases) {
  plot9 <- plot9 + geom_line(aes(y = `CO2`, color = "CO2", group = 1))
}
if ("N2O" %in% available_gases) {
  plot9 <- plot9 + geom_line(aes(y = `N2O`, color = "N2O", group = 1))
}

# Finalize the plot with labels and themes
plot9 <- plot9 +
  labs(title = "Kelly North, Iowa State Research Farm, Ames Iowa",
       x = "Time (year)",
       y = "Flux (g/ha/day)",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(plot9)

#plot type 2: N2O vs Treatments
#plot for N2O vs treatmements
plot_N2O <- ggplot(site9_data_spread, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site9_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 3: N2O vs irrigation
#plot for N2O vs irrigation
plot_N2O <- ggplot(site9_data_spread, aes(x = sample_date, y = `N2O`, color = irrigation, group = irrigation)) + 
  geom_line() +
  labs(title = paste("Effect of Tillage on N2O Fluxes at", site9_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Irrigation") +
  theme_minimal()

# Display the plot
print(plot_N2O)
#plot type 4: N2O vs tillage
#plot for N2O vs tillage
plot_N2O <- ggplot(site9_data_spread, aes(x = sample_date, y = `N2O`, color = tillage, group = tillage)) + 
  geom_line() +
  labs(title = paste("Effect of Irrigation on N2O Fluxes at", site9_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Tillage") +
  theme_minimal()

# Display the plot
print(plot_N2O)
#plot type 5: N2O vs Crop
#plot for N2O vs crop
plot_N2O <- ggplot(site9_data_spread, aes(x = sample_date, y = `N2O`, color = crop, group = crop)) + 
  geom_line() +
  labs(title = paste("Effect of Crop on N2O Fluxes at", site9_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Crop") +
  theme_minimal()

# Display the plot
print(plot_N2O)

#plot type 2: interactive plots

# plot for N2O vs treatment
site9_name <- "Kelly North, Iowa State Research Farm, Ames Iowa"
# plot for N2O vs treatments
plot_N2O <- ggplot(site9_data_spread, aes(x = sample_date, y = `N2O`, color = treatment_name, group = treatment_name)) + 
  geom_line() +
  labs(title = paste("Effect of Treatments on N2O Fluxes at", site9_name),
       x = "Time (year)",
       y = "N2O Flux (g/ha/day)",
       color = "Treatment") +
  theme_minimal()

# Convert to an interactive plot
plot_N2O_interactive <- ggplotly(plot_N2O)

# Display the interactive plot
plot_N2O_interactive

saveWidget(plot_N2O_interactive, "C:/Users/shiba/Downloads/N2O_interactive_emissions.html")


#plot type 3: interactive daily cumulative 
# Ensure the sample_date is in Date format
# Ensure that flux values are not missing or zero
site9_data_spread <- site9_data_spread %>%
  filter(!is.na(N2O)) %>%
  filter(N2O != 0)

# Calculate cumulative emissions over time for each gas
site9_data_spread <- site9_data_spread %>%
  arrange(sample_date) %>%
  mutate(cumulative_N2O = cumsum(N2O)
  )

# Plot cumulative N2O over time and make it interactive
plot_N2O_cumulative <- ggplot(site9_data_spread, aes(x = sample_date, y = cumulative_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Daily Cumulative N2O Emissions Over Time at", site9_name),
       x = "Time (year)",
       y = "Daily Cumulative N2O Emissions (g/ha)") +
  theme_minimal()

interactive_plot_N2O <- ggplotly(plot_N2O_cumulative)

# Display the interactive plots
interactive_plot_N2O
saveWidget(interactive_plot_N2O, "C:/Users/shiba/Downloads/N2O_daily_cumulative_emissions.html")

#plot type 4: interactive weekly sum trend
# Ensure the sample_date is in Date format
# Create a new column for the week number
site9_data_spread <- site9_data_spread %>%
  mutate(week = floor_date(sample_date, unit = "week"))

# Calculate the weekly sum for each gas
site9_data_weekly <- site9_data_spread %>%
  group_by(week) %>%
  summarise(weekly_sum_N2O = sum(N2O, na.rm = TRUE)
  )

# Plot interactive weekly sum for N2O over time
plot_N2O_weekly <- ggplot(site9_data_weekly, aes(x = week, y = weekly_sum_N2O)) + 
  geom_line(color = "red") +
  labs(title = paste("Weekly N2O Emissions Over Time at", site9_name),
       x = "Time (year)",
       y = "Weekly N2O Emissions (g/ha)") +
  theme_minimal()

# Convert to interactive plot
plot_N2O_weekly_interactive <- ggplotly(plot_N2O_weekly)

# Display the interactive plots
plot_N2O_weekly_interactive

# Save the interactive plot
saveWidget(plot_N2O_weekly_interactive, "C:/Users/shiba/Downloads/N2O_weekly_emissions.html")
