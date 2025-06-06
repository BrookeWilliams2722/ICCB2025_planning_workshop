# Required packages
library(terra)
library(viridisLite)
library(prioritizr)

#Set the working directory to the folder you just created (prioritizr_workshop)
setwd("C:/Users/uqbwil14/OneDrive - Queensland University of Technology/Desktop/ICCB_workshop/prioritizr_workshop")

# Load the Planning unit
PU <- rast("data/otherdata/PlanningUnits.tif")
plot(PU, col = viridisLite::mako(n = 1))

# Get the file names of the testing data
spp.list <- list.files(path = "data/SpeciesDistributions/", full.names = T, recursive = T, pattern = ".tif$")
                       
# Load all files and rename them
spp <- rast(spp.list[grep("current", spp.list)])
# Get just the filenames (without full paths and extensions)
new_names <- tools::file_path_sans_ext(basename(spp.list[grep("current", spp.list)]))
# Load and assign names
spp <- rast(spp.list[grep("current", spp.list)])
names(spp) <- new_names
# Plot species distributions
plot(spp, axes = F,col = viridisLite::mako(n = 100, direction = -1), main = c(names(spp)))

# Do the same for "future" rasters
spp <- rast(spp.list[grep("future", spp.list)])
# Get just the filenames (without full paths and extensions)
new_names <- tools::file_path_sans_ext(basename(spp.list[grep("future", spp.list)]))
# Load and assign names
spp <- rast(spp.list[grep("future", spp.list)])
names(spp) <- new_names
# Plot first four species distributions
plot(spp, axes = F,col = viridisLite::mako(n = 100, direction = -1), main = c(names(spp)))

# load protected areas data
PA <- rast("data/otherdata/protected_areas.tif")
# plot them
plot(c(PA), 
     axes = FALSE, 
     col = viridisLite::mako(n = 100, direction = -1), 
     main = c("Protected Areas (I & II)"))

# load urban centers data
urban <- rast("data/otherdata/urban_centers.tif")
# plot them
plot(c(urban), 
     axes = FALSE, 
     col = viridisLite::mako(n = 100, direction = -1), 
     main = c("Urban Centers"))

# load cost layer
hfp <- rast("data/otherdata/cost_hfp2013.tif")
# plot the result
plot(hfp, 
     axes = FALSE, 
     col = viridisLite::mako(n = 10, direction = -1), 
     main = "Global human footprint")


budget.area <- round(0.3 * length(cells(PU)))

# Scenario 1
p <- problem(PU, spp) %>% 
  add_min_shortfall_objective(budget = budget.area) %>% # Budget in # of cells or area units
  add_relative_targets(targets = 1) %>% 
  add_default_solver() %>% 
  add_proportion_decisions()

s1 <- solve(p)
plot(s1)

# Scenario 2 - lock out urban areas
p <- problem(PU, spp) %>% 
  add_min_shortfall_objective(budget = budget.area) %>% # Budget in # of cells or area units
  add_relative_targets(targets = 1) %>% 
  add_proportion_decisions() %>% 
  add_locked_out_constraints(urban) %>%  # <- Lock out unsuitable areas
  add_default_solver()

s2 <- solve(p)
plot(s2)

# Scenario 3 - lock in protected areas
p <- problem(PU, spp) %>% 
  add_min_shortfall_objective(budget = budget.area) %>% # Budget in # of cells or area units
  add_relative_targets(targets = 1) %>% 
  add_proportion_decisions() %>% 
  add_locked_in_constraints(PA) %>%  # <- <- Lock in protected areas
  add_locked_out_constraints(urban) %>%  # <- Lock out unsuitable areas
  add_default_solver()

s3 <- solve(p)
plot(s3)

# Scenario 4 - hfp is a cost/penalty
# Build the problem
p <- problem(PU, spp) %>% 
  add_min_shortfall_objective(budget = budget.area) %>%  # Budget in # of cells or area units
  add_relative_targets(targets = 1) %>% 
  add_linear_penalties(penalty = 1, data = hfp) %>%  # Penalize high HFP values
  add_proportion_decisions() %>% 
  add_locked_in_constraints(PA) %>%  # <- Lock in protected areas
  add_locked_out_constraints(urban) %>%  # <- Lock out unsuitable areas
  add_default_solver()

s4 <- solve(p)
plot(s4)

# Plot all scenarios side by side
# Set plotting area to 1 row, 4 columns
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))  # 2 rows, 2 columns

plot(s1, main = "Scenario 1")
plot(s2, main = "Scenario 2 - lock out")
plot(s3, main = "Scenario 3 - lock in")
plot(s4, main = "Scenario 4 - apply penalty")

# Reset plotting layout to default (optional)
par(mfrow = c(1,1))

#Calculate metrics
#Scenario 1
rpz_target_spp_s1 <- eval_target_coverage_summary(p, s1) 
mean(rpz_target_spp_s1$relative_held)
mean(rpz_target_spp_s1$relative_shortfall) 
#Scenario 2
rpz_target_spp_s2 <- eval_target_coverage_summary(p, s2) 
mean(rpz_target_spp_s2$relative_held)
mean(rpz_target_spp_s2$relative_shortfall) 
#Scenario 3
rpz_target_spp_s3 <- eval_target_coverage_summary(p, s3) 
mean(rpz_target_spp_s3$relative_held)
mean(rpz_target_spp_s3$relative_shortfall)
#Scenario 4
rpz_target_spp_s4 <- eval_target_coverage_summary(p, s4) 
mean(rpz_target_spp_s4$relative_held)
mean(rpz_target_spp_s4$relative_shortfall)

