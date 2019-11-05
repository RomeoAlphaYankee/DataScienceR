# Exploring, visualizing, and modeling USDA crop yield data
# The goal is to write functions which will do the heavy lifting, convert units, 
# join census region data, plot yields by region, model yields,
# and finally predict future crop yields.

# Download data from https://quickstats.nass.usda.gov/
# Read into current environment

library(readr)
corn <- read_rds('../data/nass.corn.rds')
wheat <- read_rds('../data/nass.wheat.rds')
barley <- read_rds('../data/nass.barley.rds')

# Write a function to convert acres to sq. yards
acres_to_sq_yards <- function(x) {
  x * 4840
}

# Write a function to convert yards to meters
yards_to_meters <- function(x){
  x * 0.9144
}

# Write a function to convert sq. meters to hectares
sq_meters_to_hectares <- function(x){
  x / 10000
}

# Write a reciprocal function
get_reciprocal <- function(x){
  1 / x
}

library(dplyr)
library(magrittr)

# Write a function to convert sq. yards to sq. meters
sq_yards_to_sq_meters <- function(sq_yards) {
  sq_yards %>%
    sqrt() %>%
    yards_to_meters() %>%
    raise_to_power(2)
}

# Write a function to convert acres to hectares
acres_to_hectares <- function(acres) {
  acres %>%
    acres_to_sq_yards() %>%
    sq_yards_to_sq_meters() %>%
    sq_meters_to_hectares()
}

# Define a harmonic acres to hectares function
harmonic_acres_to_hectares <- function(acres) {
  acres %>% 
    get_reciprocal() %>%
    acres_to_hectares() %>% 
    get_reciprocal()
}

# Write a function to convert lb to kg
lbs_to_kgs <- function(x){
  x * 0.45359237
}

# Write a function to convert bushels to lbs
bushels_to_lbs <- function(bushels, crop) {
  c(barley = 48, corn = 56, wheat = 60) %>%
    extract(crop) %>%
    multiply_by(bushels)
}

# Write a function to convert bushels to kg
bushels_to_kgs <- function(bushels, crop) {
  bushels %>%
    bushels_to_lbs(crop) %>%
    lbs_to_kgs()
}

# Write a function to convert bushels/acre to kg/ha
bushels_per_acre_to_kgs_per_hectare <- function(bushels_per_acre, crop = c('barley', 'corn', 'wheat')){
  # Match the crop argument
  crop <- match.arg(crop)
  bushels_per_acre %>%
    bushels_to_kgs(crop) %>%
    harmonic_acres_to_hectares()
}

glimpse(corn)

# Write a function that converts grain data using custom functions above
# and adds the data as two new columns in the dataframe.
fortify_with_metric_units <- function(data, crop){
  data %>%
    mutate(
      farmed_area_ha = acres_to_hectares(farmed_area_acres),
      yield_kg_per_ha = bushels_per_acre_to_kgs_per_hectare(
        yield_bushels_per_acre,
        crop = crop
      )
    )
}

# Run conversion on our commodity datasets
wheat <- fortify_with_metric_units(wheat, crop = "wheat")
corn <- fortify_with_metric_units(corn, crop = "corn")
barley <- fortify_with_metric_units(barley, crop = "barley")

# Check the results
head(wheat)
glimpse(corn)
ls.str(barley)

# Plot changes in yields over time
library(ggplot2)

plot_yield_vs_year <- function(data){
  ggplot(data, aes(year, yield_kg_per_ha)) +
    geom_line(aes(group = state)) +
    geom_smooth()
}

# Test the plotting function on the wheat dataset
plot_yield_vs_year(wheat)

# Add US regions to data for further analysis
# First download the data
usa_census_regions <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
head(usa_census_regions)

# Clean and rename column to match the grain data
usa_census_regions <- usa_census_regions[ , -c(2)]
colnames(usa_census_regions) <- tolower(colnames(usa_census_regions))
tail(usa_census_regions)

# Join the grain data to the census regions
fortify_with_census_region <- function(data){
  data %>% 
    inner_join(usa_census_regions, by = 'state')
}

# Fortify the data sets
corn <- fortify_with_census_region(corn)
wheat <- fortify_with_census_region(wheat)
barley <- fortify_with_census_region(barley)

# Plot crop yields over time by region
plot_yield_vs_year_by_region <- function(data){
  plot_yield_vs_year(data) +
    facet_wrap(vars(division))
}

plot_yield_vs_year_by_region(wheat)

# Model grain yields
# Because yields have a 'hockey stick' look, try gereralized additive model instead of linear model
library(mgcv)

run_gam_yield_vs_year_by_region <- function(data){
  gam(yield_kg_per_ha ~ s(year) + division, data = data)
}

# Create the models
corn_model <- run_gam_yield_vs_year_by_region(corn)
wheat_model <- run_gam_yield_vs_year_by_region(wheat)
barley_model <- run_gam_yield_vs_year_by_region(barley)

# Forecast predictions for future periods
census_regions <- usa_census_regions %>%
  distinct(division)

# Predict the yield
predict_yields <- function(model, year){
  predict_this <- data.frame(
    year = year,
    census_region = census_regions
  )
  pred_yield_kg_per_ha <- predict(model, predict_this, type = "response")
  
  predict_this %>%
    mutate(pred_yield_kg_per_ha = pred_yield_kg_per_ha)
}

# Try the crop yield prediction function
predict_yields(corn_model, 2050)

# Visualize and predict in one simple step
plot_yield_vs_year_by_region(corn)
predict_yields(corn_model, 2021)

plot_yield_vs_year_by_region(barley)
predict_yields(barley_model, 2025)

plot_yield_vs_year_by_region(wheat)
predict_yields(wheat_model, 2020)

###