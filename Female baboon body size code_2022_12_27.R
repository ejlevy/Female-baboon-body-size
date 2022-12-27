# Set up ----------------------------------------------------------------

## This script recreates the analyses for 
  # Early life drought predicts components of adult body size in wild female baboons
  # Emily J. Levy, Anna Lee, I. Long’ida Siodi, Emma C. Helmich, Emily M. McLean, Elise J. Malone, Maggie J. Pickard, Riddhi Ranjithkumar, Jenny Tung, Elizabeth A. Archie, Susan C. Alberts 
# To access the data: https://doi.org/10.7924/r43r11g2m

## Load packages
library(performance) # for R2
library(forcats)
library(dplyr)
library(ggplot2)
library(readxl)
library(asreml) # for animal model; need a license for it


## Set working directory
  # I'm setting my working directory as downloads for the purposes of this script
setwd("~/Downloads")

## Load data (please see readme in the first tab of each excel file)
# Body size & demographic data
sr <- read_xlsx("Body size and demographic data.xlsx", sheet = 2)
leg <- read_xlsx("Body size and demographic data.xlsx", sheet = 3)
forearm <- read_xlsx("Body size and demographic data.xlsx", sheet = 4)
  # Make sure ID is a categorical variable
sr$ID <- as.factor(sr$ID)
leg$ID <- as.factor(leg$ID)
forearm$ID <- as.factor(forearm$ID)

# Pedigree
pedigree <- read_xlsx("Pedigree.xlsx", sheet = 2)
  # Make sure ID is a categorical variable
pedigree$ID <- as.factor(pedigree$ID)

# Inanimate object validation data
validation <- read_xlsx("Inanimate object validation data.xlsx", sheet = 2)


# Inanimate object validation (Table S1) ---------------------------------------------
# This dataset is very close to what's reported in the supplement...
  # All that's left is to calculate a few more columns and take the mean across distance_meters
validation2 <- validation %>% 
  group_by(object, measurement, manual_length_cm) %>% 
  mutate(difference = manual_length_cm - photogrammetry_length_cm, prct_difference = difference/manual_length_cm*100) %>% # Create difference and percent difference variables
  summarise(mean_manual = mean(manual_length_cm), mean_parallel_laser = mean(photogrammetry_length_cm), # create new columns that contense across distance_meters
            mean_difference = mean(difference), mean_prct_difference = mean(prct_difference), parallel_laser_sd = sd(photogrammetry_length_cm))
# This is Table S1


# Descriptive body size data (Table S3) ----------------------------------------------

### Making Table S3
  # This table has 3 sections - 
    # one that includes all the data in the 3 datasets (full dataset), 
    # one that only includes images in which rain, drought, and maternal loss data are not NA (slightly smaller dataset), 
    # and one in which ELA score is not NA (slightly smaller than the other two datasets)

## Full dataset
descriptives_sr1 <- sr %>% group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(sr_width_diff), mean_prct_diff = mean(sr_width_diff/sr_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(sr_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(sr_cm)*100) %>%
  mutate(body_part = "Shoulder-rump")
descriptives_leg1 <- leg %>% group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(leg_diff), mean_prct_diff = mean(leg_diff/leg_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(leg_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(leg_cm)*100) %>%
  mutate(body_part = "Leg")
descriptives_forearm1 <- forearm %>% group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(forearm_diff), mean_prct_diff = mean(forearm_diff/forearm_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(forearm_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(forearm_cm)*100) %>%
  mutate(body_part = "Forearm")
# Putting those three all together into one large table
descriptives_1 <- rbind(descriptives_sr1, descriptives_leg1, descriptives_forearm1) %>% arrange(ID, body_part)

# Table with one row for each body part
# Note that SD, SE, and CV exclude the baboons for which there was only 1 image.
descriptives_2 <- descriptives_1 %>%
  group_by(body_part) %>%
  summarise(n_subjects = length(unique(ID)), n_total_images = sum(n_images), n_images_subject = n_total_images/n_subjects,
            mean_age_range = mean(age_range),
            mean_sd = mean(sd, na.rm=T), mean_se = mean(se, na.rm=T), mean_prct_cv = mean(prct_cv, na.rm=T))
# And, for aggregating across subjects, we want the diff and percent diff values to be out of the TOTAL dataset, not the means of baboon means.
# First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
meandiff_sr <- sr %>% 
  summarise(n_images = n(), mean_diff = mean(sr_width_diff, na.rm=T), mean_prct_diff = mean(sr_width_diff/sr_cm*100, na.rm=T)) %>%
  mutate(body_part = "Shoulder-rump")
meandiff_leg <- leg %>% 
  summarise(n_images = n(), mean_diff = mean(leg_diff, na.rm=T), mean_prct_diff = mean(leg_diff/leg_cm*100, na.rm=T)) %>%
  mutate(body_part = "Leg")
meandiff_forearm <- forearm %>% 
  summarise(n_images = n(), mean_diff = mean(forearm_diff, na.rm=T), mean_prct_diff = mean(forearm_diff/forearm_cm*100, na.rm=T)) %>%
  mutate(body_part = "Forearm")
meandiff <- rbind(meandiff_sr, meandiff_leg, meandiff_forearm) # This along with descriptives_2 give you the first section of Table S3


## Rain, drought, maternal loss dataset
descriptives_sr1b <- sr %>% filter(!is.na(maternal_loss_0isNo)) %>%
  group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(sr_width_diff), mean_prct_diff = mean(sr_width_diff/sr_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(sr_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(sr_cm)*100) %>%
  mutate(body_part = "Shoulder-rump")
descriptives_leg1b <- leg %>% filter(!is.na(maternal_loss_0isNo)) %>%
  group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(leg_diff), mean_prct_diff = mean(leg_diff/leg_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(leg_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(leg_cm)*100) %>%
  mutate(body_part = "Leg")
descriptives_forearm1b <- forearm %>% filter(!is.na(maternal_loss_0isNo)) %>%
  group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(forearm_diff), mean_prct_diff = mean(forearm_diff/forearm_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(forearm_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(forearm_cm)*100) %>%
  mutate(body_part = "Forearm")
# Putting those three all together into one large table
descriptives_1b <- rbind(descriptives_sr1b, descriptives_leg1b, descriptives_forearm1b) %>% arrange(ID, body_part)

# Table with one row for each body part
# Note that SD, SE, and CV exclude the baboons for which there was only 1 image.
descriptives_2b <- descriptives_1b %>%
  group_by(body_part) %>%
  summarise(n_subjects = length(unique(ID)), n_total_images = sum(n_images), n_images_subject = n_total_images/n_subjects,
            mean_age_range = mean(age_range),
            mean_sd = mean(sd, na.rm=T), mean_se = mean(se, na.rm=T), mean_prct_cv = mean(prct_cv, na.rm=T))
# And, for aggregating across subjects, we want the diff and percent diff values to be out of the TOTAL dataset, not the means of baboon means.
# First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
meandiff_srb <- sr %>% filter(!is.na(maternal_loss_0isNo)) %>%
  summarise(n_images = n(), mean_diff = mean(sr_width_diff, na.rm=T), mean_prct_diff = mean(sr_width_diff/sr_cm*100, na.rm=T)) %>%
  mutate(body_part = "Shoulder-rump")
meandiff_legb <- leg %>% filter(!is.na(maternal_loss_0isNo)) %>%
  summarise(n_images = n(), mean_diff = mean(leg_diff, na.rm=T), mean_prct_diff = mean(leg_diff/leg_cm*100, na.rm=T)) %>%
  mutate(body_part = "Leg")
meandiff_forearmb <- forearm %>% filter(!is.na(maternal_loss_0isNo)) %>%
  summarise(n_images = n(), mean_diff = mean(forearm_diff, na.rm=T), mean_prct_diff = mean(forearm_diff/forearm_cm*100, na.rm=T)) %>%
  mutate(body_part = "Forearm")
meandiffb <- rbind(meandiff_srb, meandiff_legb, meandiff_forearmb) # This along with descriptives_2b give you the second section of Table S3


## ELA score dataset
descriptives_sr1c <- sr %>% filter(!is.na(early_adversity_score)) %>%
  group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(sr_width_diff), mean_prct_diff = mean(sr_width_diff/sr_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(sr_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(sr_cm)*100) %>%
  mutate(body_part = "Shoulder-rump")
descriptives_leg1c <- leg %>% filter(!is.na(early_adversity_score)) %>%
  group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(leg_diff), mean_prct_diff = mean(leg_diff/leg_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(leg_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(leg_cm)*100) %>%
  mutate(body_part = "Leg")
descriptives_forearm1c <- forearm %>% filter(!is.na(early_adversity_score)) %>%
  group_by(ID) %>%
  summarise(n_images = n(), age_range = max(age)-min(age),
            mean_diff = mean(forearm_diff), mean_prct_diff = mean(forearm_diff/forearm_cm*100), # First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
            sd = sd(forearm_cm), se = sd/sqrt(n_images), prct_cv = sd/mean(forearm_cm)*100) %>%
  mutate(body_part = "Forearm")
# Putting those three all together into one large table
descriptives_1c <- rbind(descriptives_sr1c, descriptives_leg1c, descriptives_forearm1c) %>% arrange(ID, body_part)

# Table with one row for each body part
# Note that SD, SE, and CV exclude the baboons for which there was only 1 image.
descriptives_2c <- descriptives_1c %>%
  group_by(body_part) %>%
  summarise(n_subjects = length(unique(ID)), n_total_images = sum(n_images), n_images_subject = n_total_images/n_subjects,
            mean_age_range = mean(age_range),
            mean_sd = mean(sd, na.rm=T), mean_se = mean(se, na.rm=T), mean_prct_cv = mean(prct_cv, na.rm=T))
# And, for aggregating across subjects, we want the diff and percent diff values to be out of the TOTAL dataset, not the means of baboon means.
# First is absolute difference in cm, second calculates that diff as a proportion of the body size measure, *100 to make it a percent
meandiff_src <- sr %>% filter(!is.na(early_adversity_score)) %>%
  summarise(n_images = n(), mean_diff = mean(sr_width_diff, na.rm=T), mean_prct_diff = mean(sr_width_diff/sr_cm*100, na.rm=T)) %>%
  mutate(body_part = "Shoulder-rump")
meandiff_legc <- leg %>% filter(!is.na(early_adversity_score)) %>%
  summarise(n_images = n(), mean_diff = mean(leg_diff, na.rm=T), mean_prct_diff = mean(leg_diff/leg_cm*100, na.rm=T)) %>%
  mutate(body_part = "Leg")
meandiff_forearmc <- forearm %>% filter(!is.na(early_adversity_score)) %>%
  summarise(n_images = n(), mean_diff = mean(forearm_diff, na.rm=T), mean_prct_diff = mean(forearm_diff/forearm_cm*100, na.rm=T)) %>%
  mutate(body_part = "Forearm")
meandiffc <- rbind(meandiff_src, meandiff_legc, meandiff_forearmc) # This along with descriptives_2c give you the third section of Table S3

# Put these all together for Table S3



# Animal model Set up ---------------------------
inverse_pedigree <- ainverse(pedigree)            # Fixes the pedigree so it's ready for the model

# Piecewise linear-linear with asreml -------------------------------

### Shoulder-rump (Annotating more for this section, less for leg and forearm because it's basically the same code)
## Finding the best knot
find_knot_linear_sr <- function(knot_input){
  sr$timespline_var <- ifelse(sr$age >= knot_input, sr$age - knot_input, 0) # Create timespline variable that shifts everything after the knot back by the value of the knot
  fit <- asreml(fixed = sr_cm ~ age + timespline_var,                       # Run the animal model at that knot location
                random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = sr) 
  save <- c(knot_input, summary(fit)$aic[1])                                # Save the knot_input and the AIC value of the model
}
knots_lin_sr <- seq(3.2,22.8, by=0.1)                     # Define the knots it should rotate through (this is just below the minimum and just past the maximum)
df_linear_sr <- sapply(knots_lin_sr, find_knot_linear_sr) # Run the function for each knot defined in 'knots'
df_linear_sr <- as.data.frame(t(df_linear_sr))            # Transpose and make into a data frame
colnames(df_linear_sr) <- c("knot_location", "AIC")       # Add column names
ggplot(df_linear_sr, aes(x=knot_location, y=AIC)) + geom_point() + geom_line() + theme_minimal()  # Plot the AIC results
min_lin_sr <- min(df_linear_sr$AIC)                        # Identifies the minimum AIC value
df_linear_sr$knot_location[df_linear_sr$AIC == min_lin_sr] # Finds the knot location at the min AIC.
# Knot is at 6.4 - It's a VERY clear minimum


## Visualizing it (Figure S3)
knot_lin_sr <- 6.4  # Set knot location
sr$timespline <- ifelse(sr$age >= knot_lin_sr, sr$age - knot_lin_sr, 0) # Create timespline variable that shifts everything after the knot back by the value of the knot
linear_sr_ped <- asreml(fixed = sr_cm ~ age + timespline,               # Run animal model at that knot location 
                        random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = sr)
intercept_lin_sr <- coefficients(linear_sr_ped)$fixed[3] # Extract coefficients from the model
age_slope_lin_sr <- coefficients(linear_sr_ped)$fixed[2]
spline_slope_lin_sr <- coefficients(linear_sr_ped)$fixed[1]
plotlinear_sr <- function(x){                            # Function that creates an equation of the model results (to plot in next line)
  return(ifelse(x>1 & x < knot_lin_sr, intercept_lin_sr + age_slope_lin_sr*x,
                ifelse(x>=knot_lin_sr & x <=25, intercept_lin_sr + age_slope_lin_sr*x + spline_slope_lin_sr*(x-knot_lin_sr), 0
                )))}
ggplot(sr, aes(x=age, y=sr_cm)) + geom_point(alpha = 0.2) +   # Plot model results
  geom_path(stat="function", fun=plotlinear_sr) + 
  theme_minimal() + labs(x="Age", y="Shoulder-Rump (cm)", title = 'Linear-Linear, Knot at 6.4')
ggsave("Linear piecewise sr_knot at 6.4.jpg", width = 6, height = 4, units = "in")


### Leg 
## Finding the best knot
find_knot_linear_leg <- function(knot_input){
  leg$timespline_var <- ifelse(leg$age >= knot_input, leg$age - knot_input, 0) # Create timespline variable that shifts everything after the knot back by the value of the knot
  fit <- asreml(fixed = mean_leg ~ age + timespline_var + leg_rating, 
                random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = leg) 
  save <- c(knot_input, summary(fit)$aic[1]) # Save the knot_input and the AIC value of the model
}
knots_lin_leg <- seq(3.2,22.8, by=0.1) # Define the knots it should rotate through (this is just below the minimum and just past the maximum)
df_linear_leg <- sapply(knots_lin_leg, find_knot_linear_leg) # Run the function for each knot defined in 'knots'
df_linear_leg <- as.data.frame(t(df_linear_leg)) # Transpose and make into a data frame
colnames(df_linear_leg) <- c("knot_location", "AIC") # Add column names
ggplot(df_linear_leg, aes(x=knot_location, y=AIC)) + geom_point() + geom_line() + theme_minimal()
min_lin_leg <- min(df_linear_leg$AIC) 
df_linear_leg$knot_location[df_linear_leg$AIC == min_lin_leg] # Finds the knot location at the min AIC.
# Knot is at 5.8 


## Visualizing it (Figure S3)
knot_lin_leg <- 5.8
leg$timespline <- ifelse(leg$age >= knot_lin_leg, leg$age - knot_lin_leg, 0) # Create timespline variable that shifts everything after the knot back by the value of the knot
linear_leg_ped <- asreml(fixed = mean_leg ~ age + timespline + leg_rating, 
                         random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = leg)
intercept_lin_leg <- coefficients(linear_leg_ped)$fixed[4]
age_slope_lin_leg <- coefficients(linear_leg_ped)$fixed[3]
spline_slope_lin_leg <- coefficients(linear_leg_ped)$fixed[2]
leg_rating_lin <- coefficients(linear_leg_ped)$fixed[1]
plotlinear_leg <- function(x){
  return(ifelse(x>1 & x < knot_lin_leg, intercept_lin_leg + age_slope_lin_leg*x + 1.5*leg_rating_lin,
                ifelse(x>=knot_lin_leg & x <=25, intercept_lin_leg + age_slope_lin_leg*x + spline_slope_lin_leg*(x-knot_lin_leg) + 1.5*leg_rating_lin, 0
                )))}
ggplot(leg, aes(x=age, y=mean_leg)) + geom_point(alpha = 0.2) + 
  geom_path(stat="function", fun=plotlinear_leg) + 
  theme_minimal() + labs(x="Age", y="Leg (cm)", title = 'Linear-Linear, Knot at 5.8')
ggsave("Linear piecewise leg_knot at 5.8.jpg", width = 6, height = 4, units = "in")

### Forearm 
## Finding the best knot
find_knot_linear_forearm <- function(knot_input){
  forearm$timespline_var <- ifelse(forearm$age >= knot_input, forearm$age - knot_input, 0) # Create timespline variable that shifts everything after the knot back by the value of the knot
  fit <- asreml(fixed = mean_forearm ~ age + timespline_var, 
                random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = forearm) 
  save <- c(knot_input, summary(fit)$aic[1]) # Save the knot_input and the AIC value of the model
}
knots_lin_forearm <- seq(3.2,22.8, by=0.1) # Define the knots it should rotate through (this is just below the minimum and just past the maximum)
df_linear_forearm <- sapply(knots_lin_forearm, find_knot_linear_forearm) # Run the function for each knot defined in 'knots'
df_linear_forearm <- as.data.frame(t(df_linear_forearm)) # Transpose and make into a data frame
colnames(df_linear_forearm) <- c("knot_location", "AIC") # Add column names
ggplot(df_linear_forearm, aes(x=knot_location, y=AIC)) + geom_point() + geom_line() + theme_minimal()
min_lin_forearm <- min(df_linear_forearm$AIC) 
df_linear_forearm$knot_location[df_linear_forearm$AIC == min_lin_forearm] # Finds the knot location at the min AIC.
# Knot is at 5.5


## Visualizing it (Figure S3)
knot_lin_forearm <- 5.5
forearm$timespline <- ifelse(forearm$age >= knot_lin_forearm, forearm$age - knot_lin_forearm, 0) # Create timespline variable that shifts everything after the knot back by the value of the knot
linear_forearm_ped <- asreml(fixed = mean_forearm ~ age + timespline, 
                             random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = forearm)
intercept_lin_forearm <- coefficients(linear_forearm_ped)$fixed[3]
age_slope_lin_forearm <- coefficients(linear_forearm_ped)$fixed[2]
spline_slope_lin_forearm <- coefficients(linear_forearm_ped)$fixed[1]
plotlinear_forearm <- function(x){
  return(ifelse(x>1 & x < knot_lin_forearm, intercept_lin_forearm + age_slope_lin_forearm*x,
                ifelse(x>=knot_lin_forearm & x <=25, intercept_lin_forearm + age_slope_lin_forearm*x + spline_slope_lin_forearm*(x-knot_lin_forearm), 0
                )))}
ggplot(forearm, aes(x=age, y=mean_forearm)) + geom_point(alpha = 0.2) + 
  geom_path(stat="function", fun=plotlinear_forearm) + 
  theme_minimal() + labs(x="Age", y="Forearm (cm)", title = 'Linear-Linear, Knot at 5.5')
ggsave("Linear piecewise forearm_knot at 5.5.jpg", width = 6, height = 4, units = "in")


# Quadratic threshold with asreml -------------------------------

### Shoulder-rump (Annotating more for this section, less for leg and forearm because it's basically the same code)
## Finding the best knot
find_knot_quad_sr <- function(knot_input){
  sr$timespline <- ifelse(sr$age <= knot_input, sr$age - knot_input, 0) # Everything before knot becomes negative; after becomes 0
  fit <- asreml(fixed = sr_cm ~ timespline + I(timespline^2),           # Run animal model for that knot location
                random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = sr) 
  save <- c(knot_input, summary(fit)$aic[1])                            # Save the knot_input and the AIC value of the model
}
knots_quad_sr <- seq(3.2,22.8, by=0.1)                 # Define the knots it should rotate through (this is just below the minimum and just past the maximum)
df_quad_sr <- sapply(knots_quad_sr, find_knot_quad_sr) # Run the function for each knot defined in 'knots'
df_quad_sr <- as.data.frame(t(df_quad_sr))             # Transpose and make into a data frame
colnames(df_quad_sr) <- c("knot_location", "AIC")      # Add column names
ggplot(df_quad_sr, aes(x=knot_location, y=AIC)) + geom_point() + geom_line() + theme_minimal() # Visualizing AIC vals over knot locations
min <- min(df_quad_sr$AIC, na.rm=T)                    # Identify min AIC
df_quad_sr$knot_location[df_quad_sr$AIC == min]       # Finds the knot location at the min AIC.
# 7.9 for sr. 


## Visualizing it at optimal knot location (Figure S3)
knot_quad_sr <- 7.9                                                        # Set knot location
sr$timespline2 <- ifelse(sr$age <= knot_quad_sr, sr$age - knot_quad_sr, 0) # Everything before knot becomes negative; after becomes 0
quad_sr_ped <- asreml(fixed = sr_cm ~ timespline2 + I(timespline2^2),      # Run animal model for that knot location
                      random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = sr)
intercept_quad_sr <- coefficients(quad_sr_ped)$fixed[3]                    # Save model coefficients
age_slope_quad_sr <- coefficients(quad_sr_ped)$fixed[2]
age_quad_sr <- coefficients(quad_sr_ped)$fixed[1]
plotquad_sr <- function(x){                                                # Function that creates an equation of the model results (to plot in next line)
  return(ifelse(x > 0 & x < knot_quad_sr, intercept_quad_sr + age_slope_quad_sr*(x-knot_quad_sr) + 
                  age_quad_sr*(x-knot_quad_sr)^2, 
                intercept_quad_sr
  ))}
ggplot(sr, aes(x=age, y=sr_cm)) + geom_point(alpha = 0.2) +                # Plot it!
  geom_path(stat="function", fun=plotquad_sr) + 
  theme_minimal() + labs(x="Age", y="Shoulder-rump (cm)", title='Quadratic Threshold, Knot at 7.9')
ggsave("Quadratic threshold sr_knot at 7.9.jpg", width = 6, height = 4, units = "in")


### Leg 
## Finding the best knot
find_knot_quad_leg <- function(knot_input){
  leg$timespline <- ifelse(leg$age <= knot_input, leg$age - knot_input, 0) # Everything before knot becomes negative; after becomes 0
  fit <- asreml(fixed = mean_leg ~ timespline2 + I(timespline2^2) + leg_rating, 
                random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = leg) 
  save <- c(knot_input, summary(fit)$aic[1]) # Save the knot_input and the AIC value of the model
}
knots_quad_leg <- seq(3.2,22.8, by=0.1) # Define the knots it should rotate through (this is just below the minimum and just past the maximum)
df_quad_leg <- sapply(knots_quad_leg, find_knot_quad_leg) # Run the function for each knot defined in 'knots'
df_quad_leg <- as.data.frame(t(df_quad_leg)) # Transpose and make into a data frame
colnames(df_quad_leg) <- c("knot_location", "AIC") # Add column names
ggplot(df_quad_leg, aes(x=knot_location, y=AIC)) + geom_point() + geom_line() + theme_minimal() # Visualizing AIC vals over knot locations
min <- min(df_quad_leg$AIC, na.rm=T)
df_quad_leg$knot_location[df_quad_leg$AIC == min] # Finds the knot location at the min AIC.
# 5.0 for leg. A local minimum around age 10 as well, and around age 7-8


## Visualizing it at optimal knot location (Figure S3)
knot_quad_leg <- 5
leg$timespline2 <- ifelse(leg$age <= knot_quad_leg, leg$age - knot_quad_leg, 0) 
quad_leg_ped <- asreml(fixed = mean_leg ~ timespline2 + I(timespline2^2) + leg_rating, 
                       random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = leg)
intercept_quad_leg <- coefficients(quad_leg_ped)$fixed[4]
age_slope_quad_leg <- coefficients(quad_leg_ped)$fixed[3]
age_quad_leg <- coefficients(quad_leg_ped)$fixed[2]
leg_rating_quad <- coefficients(quad_leg_ped)$fixed[1]
plotquad_leg <- function(x){
  return(ifelse(x > 0 & x < knot_quad_leg, intercept_quad_leg + age_slope_quad_leg*(x-knot_quad_leg) + 
                  age_quad_leg*(x-knot_quad_leg)^2 + 1.5*leg_rating_quad,
                intercept_quad_leg + 1.5*leg_rating_quad # Leg rating of 1.5
  ))}
ggplot(leg, aes(x=age, y=mean_leg)) + geom_point(alpha = 0.2) + 
  geom_path(stat="function", fun=plotquad_leg) + 
  theme_minimal() + labs(x="Age", y="Leg (cm)", title='Quadratic Threshold, Knot at 5.0')
ggsave("Quadratic threshold leg_knot at 5.jpg", width = 6, height = 4, units = "in")


### Forearm 
## Finding the best knot
find_knot_quad_forearm <- function(knot_input){
  forearm$timespline <- ifelse(forearm$age <= knot_input, forearm$age - knot_input, 0) # Everything before knot becomes negative; after becomes 0
  fit <- asreml(fixed = mean_forearm ~ timespline + I(timespline^2), 
                random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = forearm) 
  save <- c(knot_input, summary(fit)$aic[1]) # Save the knot_input and the AIC value of the model
}
knots_quad_forearm <- seq(3.2,22.8, by=0.1) # Define the knots it should rotate through (this is just below the minimum and just past the maximum)
df_quad_forearm <- sapply(knots_quad_forearm, find_knot_quad_forearm) # Run the function for each knot defined in 'knots'
df_quad_forearm <- as.data.frame(t(df_quad_forearm)) # Transpose and make into a data frame
colnames(df_quad_forearm) <- c("knot_location", "AIC") # Add column names
ggplot(df_quad_forearm, aes(x=knot_location, y=AIC)) + geom_point() + geom_line() + theme_minimal() # Visualizing AIC vals over knot locations
min <- min(df_quad_forearm$AIC, na.rm=T)
df_quad_forearm$knot_location[df_quad_forearm$AIC == min] # Finds the knot location at the min AIC.
# 4.7 for forearm. Local min-ish at 7


## Visualizing it at optimal knot location (Figure S3)
knot_quad_forearm <- 4.7
forearm$timespline2 <- ifelse(forearm$age <= knot_quad_forearm, forearm$age - knot_quad_forearm, 0) 
quad_forearm_ped <- asreml(fixed = mean_forearm ~ timespline2 + I(timespline2^2), 
                           random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = forearm)
intercept_quad_forearm <- coefficients(quad_forearm_ped)$fixed[3]
age_slope_quad_forearm <- coefficients(quad_forearm_ped)$fixed[2]
age_quad_forearm <- coefficients(quad_forearm_ped)$fixed[1]
plotquad_forearm <- function(x){
  return(ifelse(x > 0 & x < knot_quad_forearm, intercept_quad_forearm + age_slope_quad_forearm*(x-knot_quad_forearm) + 
                  age_quad_forearm*(x-knot_quad_forearm)^2,
                intercept_quad_forearm
  ))}
ggplot(forearm, aes(x=age, y=mean_forearm)) + geom_point(alpha = 0.2) + 
  geom_path(stat="function", fun=plotquad_forearm) + 
  theme_minimal() + labs(x="Age", y="Forearm (cm)", title='Quadratic Threshold, Knot at 4.7')
ggsave("Quadratic threshold forearm_knot at 4.7.jpg", width = 6, height = 4, units = "in")



# Log-log quadratic with asreml -------------------------------

### Shoulder-rump 
loglog_sr <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2),     # Run animal model
                    random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = sr)
coefficients(loglog_sr)$fixed # Extract coefficients & info from model
summary(loglog_sr)$varcomp
plot(loglog_sr)

intercept_loglog_sr <- coefficients(loglog_sr)$fixed[3]  # Extract specific coefficients
logage_loglog_sr <- coefficients(loglog_sr)$fixed[2]
logage2_loglog_sr <- coefficients(loglog_sr)$fixed[1]

plotlog1_sr <- function(x) exp(intercept_loglog_sr + log(x)*logage_loglog_sr + log(x)^2*logage2_loglog_sr) # Define function to plot model output
ggplot(sr, aes(x=age, y=sr_cm)) + geom_point(alpha = 0.2) + 
  stat_function(fun = plotlog1_sr) + 
  labs(x='Age', y='Shoulder-Rump', title='log(y) ~ log(x) + log(x)^2') + theme_minimal() # Plot datapoints and model output
ggsave("Log-log sr.jpg", width = 6, height = 4, units = "in")

# Alternative plots:
plotlog2_sr <- function(x) intercept_loglog_sr + log(x)*logage_loglog_sr + log(x)^2*logage2_loglog_sr
ggplot(sr, aes(x=age, y=log(sr_cm))) + geom_point(alpha = 0.2) + 
  stat_function(fun = plotlog2_sr) + 
  labs(x='Age', y='ln(Shoulder-Rump)', title='log(y) ~ log(x) + log(x)^2') + theme_minimal()
plotlog3_sr <- function(x) intercept_loglog_sr + x*logage_loglog_sr + x^2*logage2_loglog_sr
ggplot(sr, aes(x=log(age), y=log(sr_cm))) + geom_point(alpha = 0.2) + 
  stat_function(fun = plotlog3_sr) + 
  labs(x='ln(Age)', y='ln(Shoulder-Rump)', title='log(y) ~ log(x) + log(x)^2') + theme_minimal()

### Leg 
loglog_leg <- asreml(fixed = log(mean_leg) ~ log(age) + I(log(age)^2) + leg_rating, 
                     random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = leg)
coefficients(loglog_leg)$fixed 
summary(loglog_leg)$varcomp
plot(loglog_leg)

intercept_loglog_leg <- coefficients(loglog_leg)$fixed[4]
logage_loglog_leg <- coefficients(loglog_leg)$fixed[3]
logage2_loglog_leg <- coefficients(loglog_leg)$fixed[2]
leg_rating_loglog <- coefficients(loglog_leg)$fixed[1]

plotlog1_leg <- function(x) exp(intercept_loglog_leg + log(x)*logage_loglog_leg + log(x)^2*logage2_loglog_leg + 1.5*leg_rating_loglog)
ggplot(leg, aes(x=age, y=mean_leg)) + geom_point(alpha = 0.2) + 
  stat_function(fun = plotlog1_leg) + 
  labs(x='Age', y='Leg', title='log(y) ~ log(x) + log(x)^2') + theme_minimal()
ggsave("Log-log leg.jpg", width = 6, height = 4, units = "in")

### Forearm 
loglog_forearm <- asreml(fixed = log(mean_forearm) ~ log(age) + I(log(age)^2), 
                         random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = forearm)
coefficients(loglog_forearm)$fixed 
summary(loglog_forearm)$varcomp
plot(loglog_forearm)

intercept_loglog_forearm <- coefficients(loglog_forearm)$fixed[3]
logage_loglog_forearm <- coefficients(loglog_forearm)$fixed[2]
logage2_loglog_forearm <- coefficients(loglog_forearm)$fixed[1]

plotlog1_forearm <- function(x) exp(intercept_loglog_forearm + log(x)*logage_loglog_forearm + log(x)^2*logage2_loglog_forearm)
ggplot(forearm, aes(x=age, y=mean_forearm)) + geom_point(alpha = 0.2) + 
  stat_function(fun = plotlog1_forearm) + 
  labs(x='Age', y='Forearm', title='log(y) ~ log(x) + log(x)^2') + theme_minimal()
ggsave("Log-log forearm.jpg", width = 6, height = 4, units = "in")


# Comparing 3 models with R2 (Table S4) ------------------------------------------------------
# Performance package.
conditional_sr <- c("Shoulder-rump", "Conditional R2", unlist(r2(linear_sr))[1], unlist(r2(quad_sr))[1], unlist(r2(loglog_quad_sr))[1])
marginal_sr <- c("Shoulder-rump", "Marginal R2", unlist(r2(linear_sr))[2], unlist(r2(quad_sr))[2], unlist(r2(loglog_quad_sr))[2])
conditional_leg <- c("Leg", "Conditional R2", unlist(r2(linear_leg))[1], unlist(r2(quad_leg))[1], unlist(r2(loglog_quad_leg))[1])
marginal_leg <- c("Leg", "Marginal R2", unlist(r2(linear_leg))[2], unlist(r2(quad_leg))[2], unlist(r2(loglog_quad_leg))[2])
conditional_forearm <- c("Forearm", "Conditional R2", unlist(r2(linear_forearm))[1], unlist(r2(quad_forearm))[1], unlist(r2(loglog_quad_forearm))[1])
marginal_forearm <- c("Forearm", "Marginal R2", unlist(r2(linear_forearm))[2], unlist(r2(quad_forearm))[2], unlist(r2(loglog_quad_forearm))[2])

r2 <- data.frame(rbind(conditional_sr, marginal_sr, conditional_leg, marginal_leg, conditional_forearm, marginal_forearm))
colnames(r2) <- c("Body part", "Output", "Piecewise linear-linear", "Piecewise quadratic threshold", "Log-log quadratic")



# Plots of body size data -------------------------------------------------

### Plots with error bars (Figure S4): size SD (y-axis), and on the x it's extending to the min and max age over which the photos were taken
sr %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(sr_width = mean(sr_cm), mean_age = mean(age), sr_sd = sd(sr_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=sr_width, x=mean_age)) + 
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+ 
  labs(x='Age (years)', y='Shoulder-Rump (cm)', subtitle = "Vertical error bars are +/- SD; Horizontal error bars are min and max age") +
  geom_errorbar(aes(ymin = sr_width-sr_sd, ymax = sr_width+sr_sd), color = 'black') + 
  geom_errorbar(aes(xmin = min_age, xmax = max_age), color = 'black') +
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(34,52,2))
ggsave("SR with error bars.jpg", width = 6, height = 4, units = "in")

leg %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(leg = mean(leg_cm), mean_age = mean(age), leg_sd = sd(leg_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=leg, x=mean_age)) + 
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+ 
  labs(x='Age (years)', y='Leg (cm)', subtitle = "Vertical error bars are +/- SD; Horizontal error bars are min and max age") +
  geom_errorbar(aes(ymin = leg-leg_sd, ymax = leg+leg_sd), color = 'black') + 
  geom_errorbar(aes(xmin = min_age, xmax = max_age), color = 'black') +
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(34,48,2))
ggsave("Leg with error bars.jpg", width = 6, height = 4, units = "in")

forearm %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(forearm = mean(forearm_cm), mean_age = mean(age), forearm_sd = sd(forearm_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=forearm, x=mean_age)) + 
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+ 
  labs(x='Age (years)', y='Forearm (cm)', subtitle = "Vertical error bars are +/- SD; Horizontal error bars are min and max age") +
  geom_errorbar(aes(ymin = forearm-forearm_sd, ymax = forearm+forearm_sd), color = 'black') + 
  geom_errorbar(aes(xmin = min_age, xmax = max_age), color = 'black') +
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,26,2))
ggsave("Forearm with error bars.jpg", width = 6, height = 4, units = "in")


### Body size along with life history milestones (Figure 3)
## Inset plots with lowess curves
# Using median menarche and live birth for the  study subjects. Menarche = 4.7, live birth = 6.3
# Note that I might end up switching this to the values reported in Onyango 2016: Menarche = 4.5, live birth = 6.0 - but this code will be published before the manuscript.
sr %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(sr_width = mean(sr_cm), mean_age = mean(age), sr_sd = sd(sr_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=sr_width, x=mean_age)) +
  theme(axis.text = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,52,2)) +
  geom_vline(xintercept = 4.7, color = "#199494", size = 1.5) +
  geom_vline(xintercept = 6.3, color = "#E0BF16", size = 1.5) +
  labs(x='', y='') +
  #geom_point(size = 2.5) +
  geom_smooth(method = 'loess', color = "black", se=F)
ggsave("SR with milestones lowess.jpg", width = 6, height = 4, units = "in")

leg %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(leg = mean(leg_cm), mean_age = mean(age), leg_sd = sd(leg_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=leg, x=mean_age)) +
  theme(axis.text = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,52,2)) +
  geom_vline(xintercept = 4.7, color = "#199494", size = 1.5) +
  geom_vline(xintercept = 6.3, color = "#E0BF16", size = 1.5) +
  labs(x='', y='') +
  #geom_point(size = 2.5) +
  geom_smooth(method = 'loess', color = "black", se=F)
ggsave("Leg with milestones lowess.jpg", width = 6, height = 4, units = "in")

forearm %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(forearm = mean(forearm_cm), mean_age = mean(age), forearm_sd = sd(forearm_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=forearm, x=mean_age)) +
  theme(axis.text = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,52,2)) +
  geom_vline(xintercept = 4.7, color = "#199494", size = 1.5) +
  geom_vline(xintercept = 6.3, color = "#E0BF16", size = 1.5) +
  labs(x='', y='') +
  #geom_point(size = 2.5) +
  geom_smooth(method = 'loess', color = "black", se=F)
ggsave("Forearm with milestones lowess.jpg", width = 6, height = 4, units = "in")


## And the same again, but instead of using the lowess, using the line of best fit from the 4 year drought (with pedigree) model, 
  # with 0.787 for proportion of drought days (which was the mean for our study subjects) and a mid-ranking mom (0.5)
plotlog_sr <- function(x) exp(3.084 + (log(x)*0.612) + (log(x)^2*(-0.117)) + .787*-0.017 + 0.5*-0.001)
plotlog_leg <- function(x) exp(3.208 + (log(x)*0.591) + (log(x)^2*(-0.117)) + 1.5*0.017 + .787*-0.262 + 0.5*0.018) # Leg rating of 1.5 (half-way between 1 and 2, which are the 2 ratings that photos got)
plotlog_forearm <- function(x) exp(2.631 + (log(x)*0.598) + (log(x)^2*(-0.118)) + .787*-0.286 + 0.5*0.026)


sr %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(sr_width = mean(sr_cm), mean_age = mean(age), sr_sd = sd(sr_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=sr_width, x=mean_age)) +
  theme(axis.text = element_text(color = "black", size = 16), axis.title = element_text(size = 22), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,52,2)) +
  geom_vline(xintercept = 4.7, color = "#199494", size = 1.5) +
  geom_vline(xintercept = 6.3, color = "#E0BF16", size = 1.5) +
  labs(x='', y='Shoulder-Rump (cm)') +
  geom_point(size = 2.5) +
  stat_function(fun = plotlog_sr, size = 1.5)
ggsave("SR with milestones log-log model.jpg", width = 6, height = 4, units = "in")

leg %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(leg = mean(leg_cm), mean_age = mean(age), leg_sd = sd(leg_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=leg, x=mean_age)) +
  theme(axis.text = element_text(color = "black", size = 16), axis.title = element_text(size = 22), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,52,2), limits = c(33, 48)) +
  geom_vline(xintercept = 4.7, color = "#199494", size = 1.5) +
  geom_vline(xintercept = 6.3, color = "#E0BF16", size = 1.5) +
  labs(x='', y='Leg (cm)') +
  geom_point(size = 2.5) +
  stat_function(fun = plotlog_leg, size = 1.5)
ggsave("Leg with milestones log-log model.jpg", width = 6, height = 4, units = "in")

forearm %>% # Processing the raw data into a mean value for each baboon
  group_by(ID) %>% 
  summarise(forearm = mean(forearm_cm), mean_age = mean(age), forearm_sd = sd(forearm_cm), min_age = min(age), max_age = max(age)) %>%
ggplot(., aes(y=forearm, x=mean_age)) +
  theme(axis.text = element_text(color = "black", size = 16), axis.title = element_text(size = 22), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+
  scale_x_continuous(breaks = seq(4,22,2)) + scale_y_continuous(breaks = seq(18,52,2)) +
  geom_vline(xintercept = 4.7, color = "#199494", size = 1.5) +
  geom_vline(xintercept = 6.3, color = "#E0BF16", size = 1.5) +
  labs(x='Age (years)', y='Forearm (cm)') +
  geom_point(size = 2.5) +
  stat_function(fun = plotlog_forearm, size = 1.5)
ggsave("Forearm with milestones log-log model.jpg", width = 6, height = 4, units = "in")



# Models: Info ------------------------------------------------------------
# How to interpret:
# The ID row in $varcomp is the 'permanent environment', or basically how much variation between individuals that ISN'T explained by relatedness. (I think)
# The vm(ID, inverse_pedigree) is the additive genetic variance.
# When the ID row is sooooo close to 0 that it can't even give a score, and the vm() row isn't, it means that...
# most of the variance between individuals is due to relatedness.
# To calculate that actual percentage, we need to add up all three rows - this is the denominator - and use the vm() component value as the numerator.

# NOTE: I Removed maternal rank at birth from the ELA score model because maternal rank is one of the ELA components.

# Models: Cumulative Early Life Adversity Score  ---------------------------

## Shoulder-rump early adversity score models:
sr_ea_ped <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + early_adversity_score, 
                    random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(sr, !is.na(early_adversity_score)))
wald(sr_ea_ped, ssType = "conditional")$Wald[1,5]
summary(sr_ea_ped)$varcomp
plot(sr_ea_ped)
coefficients(sr_ea_ped)$fixed

## Leg early adversity models

leg_ea_ped <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + early_adversity_score + leg_rating, 
                     random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(leg, !is.na(early_adversity_score)))
wald(leg_ea_ped)
summary(leg_ea_ped)$varcomp
plot(leg_ea_ped)
coefficients(leg_ea_ped)$fixed

## Forearm early adversity models
# G component is higher here than it was for shoulder-rump or leg

arm_ea_ped <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + early_adversity_score, 
                     random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(forearm, !is.na(early_adversity_score)))
wald(arm_ea_ped)
summary(arm_ea_ped)$varcomp
plot(arm_ea_ped)
coefficients(arm_ea_ped)$fixed

# Percent of variance explained by relatedness:
summary(arm_ea_ped)$varcomp[3,1]/(summary(arm_ea_ped)$varcomp[1,1] + summary(arm_ea_ped)$varcomp[2,1] + summary(arm_ea_ped)$varcomp[3,1] + summary(arm_ea_ped)$varcomp[4,1])
# Maternal effects
summary(arm_ea_ped)$varcomp[1,1]/(summary(arm_ea_ped)$varcomp[1,1] + summary(arm_ea_ped)$varcomp[2,1] + summary(arm_ea_ped)$varcomp[3,1] + summary(arm_ea_ped)$varcomp[4,1])


## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
ea_p_sr <- wald(sr_ea_ped, ssType = "conditional")$Wald
ea_p_leg <- wald(leg_ea_ped, ssType = "conditional")$Wald
ea_p_arm <- wald(arm_ea_ped, ssType = "conditional")$Wald
ea_p <- rbind(ea_p_sr, ea_p_leg, ea_p_arm)
ea_p$body_part <- c("SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
ea_beta_sr <- summary(sr_ea_ped, coef = TRUE)$coef.fixed
ea_beta_leg <- summary(leg_ea_ped, coef = TRUE)$coef.fixed
ea_beta_arm <- summary(arm_ea_ped, coef = TRUE)$coef.fixed
ea_beta <- data.frame(rbind(ea_beta_sr, ea_beta_leg, ea_beta_arm))
ea_beta$body_part <- c("SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
ea_re_sr <- summary(sr_ea_ped)$varcomp
ea_re_sr <- ea_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
ea_re_leg <- summary(leg_ea_ped)$varcomp
ea_re_leg <- ea_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
ea_re_arm <- summary(arm_ea_ped)$varcomp
ea_re_arm <- ea_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
ea_re <- data.frame(rbind(ea_re_sr,ea_re_leg,ea_re_arm))

## Exporting these three tables
write.csv(ea_p, "ELA score p-vals.csv")
write.csv(ea_beta, "ELA score beta and SE.csv")
write.csv(ea_re, "ELA score random effects.csv")



# Models: Maternal loss  ----------------------------------------------------

## Shoulder-rump maternal death models
sr_md_ped <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + factor(maternal_loss_0isNo) + mom_proprank_birth, 
                    random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(sr, !is.na(maternal_loss_0isNo)))
wald(sr_md_ped)
summary(sr_md_ped)$varcomp
plot(sr_md_ped)
coefficients(sr_md_ped)$fixed

## Leg maternal death models
leg_md_ped <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + factor(maternal_loss_0isNo) + leg_rating + mom_proprank_birth, 
                     random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(leg, !is.na(maternal_loss_0isNo)))
wald(leg_md_ped)
summary(leg_md_ped)$varcomp
plot(leg_md_ped)
coefficients(leg_md_ped)$fixed

## Forearm maternal death models
arm_md_ped <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + factor(maternal_loss_0isNo) + mom_proprank_birth, 
                     random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(forearm, !is.na(maternal_loss_0isNo)))
wald(arm_md_ped)
summary(arm_md_ped)$varcomp
plot(arm_md_ped)
coefficients(arm_md_ped)$fixed

## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
md_p_sr <- wald(sr_md_ped, ssType = "conditional")$Wald
md_p_leg <- wald(leg_md_ped, ssType = "conditional")$Wald
md_p_arm <- wald(arm_md_ped, ssType = "conditional")$Wald
md_p <- rbind(md_p_sr, md_p_leg, md_p_arm)
md_p$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
md_beta_sr <- summary(sr_md_ped, coef = TRUE)$coef.fixed
md_beta_leg <- summary(leg_md_ped, coef = TRUE)$coef.fixed
md_beta_arm <- summary(arm_md_ped, coef = TRUE)$coef.fixed
md_beta <- data.frame(rbind(md_beta_sr, md_beta_leg, md_beta_arm))
md_beta$body_part <- c("SR", "SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
md_re_sr <- summary(sr_md_ped)$varcomp
md_re_sr <- md_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
md_re_leg <- summary(leg_md_ped)$varcomp
md_re_leg <- md_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
md_re_arm <- summary(arm_md_ped)$varcomp
md_re_arm <- md_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
md_re <- data.frame(rbind(md_re_sr,md_re_leg,md_re_arm))

## Exporting these three tables
write.csv(md_p, "MatDeath p-vals.csv")
write.csv(md_beta, "MatDeath beta and SE.csv")
write.csv(md_re, "MatDeath random effects.csv")


# Models: Cumulative Rainfall in 1 year  --------------------------------------

# Shoulder-rump 
rain_1year_sr <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + one_year_rain + mom_proprank_birth, 
                        random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(sr, !is.na(one_year_rain)))
wald(rain_1year_sr)
summary(rain_1year_sr)$varcomp
plot(rain_1year_sr)
coefficients(rain_1year_sr)$fixed 

# Leg 
rain_1year_leg <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + one_year_rain + leg_rating + mom_proprank_birth, 
                         random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(leg, !is.na(one_year_rain)))
wald(rain_1year_leg)
summary(rain_1year_leg)$varcomp
plot(rain_1year_leg)
coefficients(rain_1year_leg)$fixed 

# Arm 
rain_1year_arm <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + one_year_rain + mom_proprank_birth, 
                         random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(forearm, !is.na(one_year_rain)))
wald(rain_1year_arm)
summary(rain_1year_arm)$varcomp
plot(rain_1year_arm)
coefficients(rain_1year_arm)$fixed

## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
rain1_p_sr <- wald(rain_1year_sr, ssType = "conditional")$Wald
rain1_p_leg <- wald(rain_1year_leg, ssType = "conditional")$Wald
rain1_p_arm <- wald(rain_1year_arm, ssType = "conditional")$Wald
rain1_p <- rbind(rain1_p_sr, rain1_p_leg, rain1_p_arm)
rain1_p$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
rain1_beta_sr <- summary(rain_1year_sr, coef = TRUE)$coef.fixed
rain1_beta_leg <- summary(rain_1year_leg, coef = TRUE)$coef.fixed
rain1_beta_arm <- summary(rain_1year_arm, coef = TRUE)$coef.fixed
rain1_beta <- data.frame(rbind(rain1_beta_sr, rain1_beta_leg, rain1_beta_arm))
rain1_beta$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
rain1_re_sr <- summary(rain_1year_sr)$varcomp
rain1_re_sr <- rain1_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
rain1_re_leg <- summary(rain_1year_leg)$varcomp
rain1_re_leg <- rain1_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
rain1_re_arm <- summary(rain_1year_arm)$varcomp
rain1_re_arm <- rain1_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
rain1_re <- data.frame(rbind(rain1_re_sr,rain1_re_leg,rain1_re_arm))

## Exporting these three tables 
write.csv(rain1_p, "Rain 1 year p-vals.csv")
write.csv(rain1_beta, "Rain 1 year beta and SE.csv")
write.csv(rain1_re, "Rain 1 year random effects.csv")

# Models: Cumulative Rainfall in first 4 years  --------------------------------------

# Shoulder rump 
rain_4year_sr <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + four_years_rain + mom_proprank_birth, 
                        random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(sr, !is.na(four_years_rain)))
wald(rain_4year_sr)
summary(rain_4year_sr)$varcomp
plot(rain_4year_sr)
coefficients(rain_4year_sr)$fixed 

# Leg 
rain_4year_leg <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + four_years_rain + leg_rating + mom_proprank_birth, 
                         random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(leg, !is.na(four_years_rain)))
wald(rain_4year_leg)
summary(rain_4year_leg)$varcomp
plot(rain_4year_leg)
coefficients(rain_4year_leg)$fixed

# Arm 
rain_4year_arm <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + four_years_rain + mom_proprank_birth, 
                         random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(forearm,!is.na(four_years_rain)))
wald(rain_4year_arm)
summary(rain_4year_arm)$varcomp
plot(rain_4year_arm)
coefficients(rain_4year_arm)$fixed 

## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
rain4_p_sr <- wald(rain_4year_sr, ssType = "conditional")$Wald
rain4_p_leg <- wald(rain_4year_leg, ssType = "conditional")$Wald
rain4_p_arm <- wald(rain_4year_arm, ssType = "conditional")$Wald
rain4_p <- rbind(rain4_p_sr, rain4_p_leg, rain4_p_arm)
rain4_p$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
rain4_beta_sr <- summary(rain_4year_sr, coef = TRUE)$coef.fixed
rain4_beta_leg <- summary(rain_4year_leg, coef = TRUE)$coef.fixed
rain4_beta_arm <- summary(rain_4year_arm, coef = TRUE)$coef.fixed
rain4_beta <- data.frame(rbind(rain4_beta_sr, rain4_beta_leg, rain4_beta_arm))
rain4_beta$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
rain4_re_sr <- summary(rain_4year_sr)$varcomp
rain4_re_sr <- rain4_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
rain4_re_leg <- summary(rain_4year_leg)$varcomp
rain4_re_leg <- rain4_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
rain4_re_arm <- summary(rain_4year_arm)$varcomp
rain4_re_arm <- rain4_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
rain4_re <- data.frame(rbind(rain4_re_sr,rain4_re_leg,rain4_re_arm))

## Exporting these three tables
write.csv(rain4_p, "Rain 4 year p-vals.csv")
write.csv(rain4_beta, "Rain 4 year beta and SE.csv")
write.csv(rain4_re, "Rain 4 year random effects.csv")


# Models: Proportion of drought days in 1 year  --------------------------------------
# Shoulder rump 
droughtday_1year_sr <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + prop_droughtdays_1year + mom_proprank_birth, 
                              random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(sr, !is.na(prop_droughtdays_1year)))
wald(droughtday_1year_sr)
summary(droughtday_1year_sr)$varcomp
plot(droughtday_1year_sr)
coefficients(droughtday_1year_sr)$fixed 

# Leg 
droughtday_1year_leg <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + prop_droughtdays_1year + leg_rating + mom_proprank_birth, 
                               random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(leg, !is.na(prop_droughtdays_1year)))
wald(droughtday_1year_leg)
summary(droughtday_1year_leg)$varcomp
plot(droughtday_1year_leg)
coefficients(droughtday_1year_leg)$fixed 

# Arm 
droughtday_1year_arm <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + prop_droughtdays_1year + mom_proprank_birth, 
                               random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(forearm, !is.na(prop_droughtdays_1year)))
wald(droughtday_1year_arm)
summary(droughtday_1year_arm)$varcomp
plot(droughtday_1year_arm)
coefficients(droughtday_1year_arm)$fixed 

## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
droughtday1_p_sr <- wald(droughtday_1year_sr, ssType = "conditional")$Wald
droughtday1_p_leg <- wald(droughtday_1year_leg, ssType = "conditional")$Wald
droughtday1_p_arm <- wald(droughtday_1year_arm, ssType = "conditional")$Wald
droughtday1_p <- rbind(droughtday1_p_sr, droughtday1_p_leg, droughtday1_p_arm)
droughtday1_p$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
droughtday1_beta_sr <- summary(droughtday_1year_sr, coef = TRUE)$coef.fixed
droughtday1_beta_leg <- summary(droughtday_1year_leg, coef = TRUE)$coef.fixed
droughtday1_beta_arm <- summary(droughtday_1year_arm, coef = TRUE)$coef.fixed
droughtday1_beta <- data.frame(rbind(droughtday1_beta_sr, droughtday1_beta_leg, droughtday1_beta_arm))
droughtday1_beta$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
droughtday1_re_sr <- summary(droughtday_1year_sr)$varcomp
droughtday1_re_sr <- droughtday1_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
droughtday1_re_leg <- summary(droughtday_1year_leg)$varcomp
droughtday1_re_leg <- droughtday1_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
droughtday1_re_arm <- summary(droughtday_1year_arm)$varcomp
droughtday1_re_arm <- droughtday1_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
droughtday1_re <- data.frame(rbind(droughtday1_re_sr,droughtday1_re_leg,droughtday1_re_arm))

## Exporting these three tables 
write.csv(droughtday1_p, "DroughtDay 1 year p-vals.csv")
write.csv(droughtday1_beta, "DroughtDay 1 year beta and SE.csv")
write.csv(droughtday1_re, "DroughtDay 1 year random effects.csv")

# Models: Proportion drought days in first 4 years  --------------------------------------

# Shoulder rump 
droughtday_4year_sr <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + mom_proprank_birth + prop_droughtdays_4year, 
                              random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(sr, !is.na(prop_droughtdays_4year)))
wald(droughtday_4year_sr)
summary(droughtday_4year_sr)$varcomp
plot(droughtday_4year_sr)
coefficients(droughtday_4year_sr)$fixed 

# Leg
droughtday_4year_leg <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + leg_rating + mom_proprank_birth + prop_droughtdays_4year, 
                               random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(leg, !is.na(prop_droughtdays_4year)))
wald(droughtday_4year_leg)
summary(droughtday_4year_leg)$varcomp
plot(droughtday_4year_leg)
coefficients(droughtday_4year_leg)$fixed 
summary(droughtday_4year_leg, coef = TRUE)$coef.fixed # this is an alternative way to get coefficients and SE and z-score for all fixed effects

# Arm 
droughtday_4year_arm <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + mom_proprank_birth + prop_droughtdays_4year, 
                               random = ~ID + vm(ID, inverse_pedigree) + momID, maxiter = 400, data = subset(forearm, !is.na(prop_droughtdays_4year)))
wald(droughtday_4year_arm)
summary(droughtday_4year_arm)$varcomp
plot(droughtday_4year_arm)
coefficients(droughtday_4year_arm)$fixed 

## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
droughtday4_p_sr <- wald(droughtday_4year_sr, ssType = "conditional")$Wald
droughtday4_p_leg <- wald(droughtday_4year_leg, ssType = "conditional")$Wald
droughtday4_p_arm <- wald(droughtday_4year_arm, ssType = "conditional")$Wald
droughtday4_p <- rbind(droughtday4_p_sr, droughtday4_p_leg, droughtday4_p_arm)
droughtday4_p$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
droughtday4_beta_sr <- summary(droughtday_4year_sr, coef = TRUE)$coef.fixed
droughtday4_beta_leg <- summary(droughtday_4year_leg, coef = TRUE)$coef.fixed
droughtday4_beta_arm <- summary(droughtday_4year_arm, coef = TRUE)$coef.fixed
droughtday4_beta <- data.frame(rbind(droughtday4_beta_sr, droughtday4_beta_leg, droughtday4_beta_arm))
droughtday4_beta$body_part <- c("SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
droughtday4_re_sr <- summary(droughtday_4year_sr)$varcomp
droughtday4_re_sr <- droughtday4_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
droughtday4_re_leg <- summary(droughtday_4year_leg)$varcomp
droughtday4_re_leg <- droughtday4_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
droughtday4_re_arm <- summary(droughtday_4year_arm)$varcomp
droughtday4_re_arm <- droughtday4_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
droughtday4_re <- data.frame(rbind(droughtday4_re_sr,droughtday4_re_leg,droughtday4_re_arm))

## Exporting these three tables
write.csv(droughtday4_p, "DroughtDay 4 year p-vals.csv")
write.csv(droughtday4_beta, "DroughtDay 4 year beta and SE.csv")
write.csv(droughtday4_re, "DroughtDay 4 year random effects.csv")



# Models: Hybrid scores -----------------------------------------------------------
# Hybrid scores were developed and calculated by Arielle Fogel and colleagues - thank you to her for sharing them!
# Without hybrid scores, we have 121 females for SR, 124 for leg, and 125 for forearm.
# And with hybrid scores, we have 54 females for SR and 56 for leg and forearm. 
hybrid <- read_xlsx("Hybrid scores.xlsx", sheet = 2)

sr %>% select(ID) %>% distinct(ID) %>% merge(., hybrid, by = "ID") %>% nrow() #54 females
leg %>% select(ID) %>% distinct(ID) %>% merge(., hybrid, by = "ID") %>% nrow() #56 females
forearm %>% select(ID) %>% distinct(ID) %>% merge(., hybrid, by = "ID") %>% nrow() #56 females

## Running the 3 drought-days-4-years models
# Adding hybrid score into new subsetted dataset.
# Running a quadratic model because hybrids may be different from non-hybrids.
data_sr_hybrid <- merge(sr, hybrid, by = "ID") 
data_leg_hybrid <- merge(leg, hybrid, by = "ID") 
data_forearm_hybrid <- merge(forearm, hybrid, by = "ID") 

data_sr_hybrid$anubis_admix <- as.numeric(data_sr_hybrid$anubis_admix)
data_leg_hybrid$anubis_admix <- as.numeric(data_leg_hybrid$anubis_admix)
data_forearm_hybrid$anubis_admix <- as.numeric(data_forearm_hybrid$anubis_admix)

## Problem: The model will NOT run successfully when we include hybrid score, possibly because the hybrid scores is collinear with the pedigree.
# Error: Singularities in the average information matrix.
# The model DOES run successfully if I remove the pedigree. So for the models below it's just ID and mom as random effects, no pedigree.

# Shoulder rump: 
droughtday_4year_sr_hybrid <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + mom_proprank_birth + prop_droughtdays_4year + anubis_admix + I(anubis_admix^2), 
                                     random = ~ID + mom, maxiter = 400, data = subset(data_sr_hybrid, !is.na(prop_droughtdays_4year)))
wald(droughtday_4year_sr_hybrid)
summary(droughtday_4year_sr_hybrid)$varcomp
plot(droughtday_4year_sr_hybrid)
coefficients(droughtday_4year_sr_hybrid)$fixed 

# Comparing AIC scores to model without hybrid score
droughtday_4year_sr_nohybrid <- asreml(fixed = log(sr_cm) ~ log(age) + I(log(age)^2) + mom_proprank_birth + prop_droughtdays_4year, 
                                       random = ~ID + mom, maxiter = 400, data = subset(data_sr_hybrid, !is.na(prop_droughtdays_4year)))
summary(droughtday_4year_sr_nohybrid)$aic - summary(droughtday_4year_sr_hybrid)$aic   

# Leg
droughtday_4year_leg_hybrid <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + leg_rating + mom_proprank_birth + prop_droughtdays_4year + anubis_admix + I(anubis_admix^2), 
                                      random = ~ID + mom, maxiter = 400, data = subset(data_leg_hybrid, !is.na(prop_droughtdays_4year)))
wald(droughtday_4year_leg_hybrid)
summary(droughtday_4year_leg_hybrid)$varcomp
plot(droughtday_4year_leg_hybrid)
coefficients(droughtday_4year_leg_hybrid)$fixed 
summary(droughtday_4year_leg_hybrid, coef = TRUE)$coef.fixed # this is an alternative way to get coefficients and SE and z-score for all fixed effects

# Comparing AIC scores to model without hybrid score
droughtday_4year_leg_nohybrid <- asreml(fixed = log(leg_cm) ~ log(age) + I(log(age)^2) + leg_rating + mom_proprank_birth + prop_droughtdays_4year, 
                                        random = ~ID + mom, maxiter = 400, data = subset(data_leg_hybrid, !is.na(prop_droughtdays_4year)))
summary(droughtday_4year_leg_nohybrid)$aic - summary(droughtday_4year_leg_hybrid)$aic   

# Arm 
droughtday_4year_arm_hybrid <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + mom_proprank_birth + prop_droughtdays_4year + anubis_admix + I(anubis_admix^2), 
                                      random = ~ID + mom, maxiter = 400, data = subset(data_forearm_hybrid, !is.na(prop_droughtdays_4year)))
wald(droughtday_4year_arm_hybrid)
summary(droughtday_4year_arm_hybrid)$varcomp
plot(droughtday_4year_arm_hybrid)
coefficients(droughtday_4year_arm_hybrid)$fixed 

# Comparing AIC scores to model without hybrid score
droughtday_4year_arm_nohybrid <- asreml(fixed = log(forearm_cm) ~ log(age) + I(log(age)^2) + mom_proprank_birth + prop_droughtdays_4year, 
                                        random = ~ID + mom, maxiter = 400, data = subset(data_forearm_hybrid, !is.na(prop_droughtdays_4year)))
summary(droughtday_4year_arm_nohybrid)$aic - summary(droughtday_4year_arm_hybrid)$aic   

# Getting results exported:
## Table of fixed effects
# First getting p-values for the three body parts - pulling everything from this, and in excel we will use the second Pr(chisq) column, which is based on the conditional wald test (as opposed to incremental).
hybrid_p_sr <- wald(droughtday_4year_sr_hybrid, ssType = "conditional")$Wald
hybrid_p_leg <- wald(droughtday_4year_leg_hybrid, ssType = "conditional")$Wald
hybrid_p_arm <- wald(droughtday_4year_arm_hybrid, ssType = "conditional")$Wald
hybrid_p <- rbind(hybrid_p_sr, hybrid_p_leg, hybrid_p_arm)
hybrid_p$body_part <- c("SR", "SR", "SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

# Now coefficients and SE - pulling everything, and in excel we will use solution (coeff) and std error columns.
hybrid_beta_sr <- summary(droughtday_4year_sr_hybrid, coef = TRUE)$coef.fixed
hybrid_beta_leg <- summary(droughtday_4year_leg_hybrid, coef = TRUE)$coef.fixed
hybrid_beta_arm <- summary(droughtday_4year_arm_hybrid, coef = TRUE)$coef.fixed
hybrid_beta <- data.frame(rbind(hybrid_beta_sr, hybrid_beta_leg, hybrid_beta_arm))
hybrid_beta$body_part <- c("SR", "SR", "SR", "SR", "SR", "SR", "SR", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Leg", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm", "Forearm")

## Table of random effects - pulling everything, and also calculating the total variance and calculating the proportion of variance explained by each random effect.
hybrid_re_sr <- summary(droughtday_4year_sr_hybrid)$varcomp
hybrid_re_sr <- hybrid_re_sr %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "SR")
hybrid_re_leg <- summary(droughtday_4year_leg_hybrid)$varcomp
hybrid_re_leg <- hybrid_re_leg %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Leg")
hybrid_re_arm <- summary(droughtday_4year_arm_hybrid)$varcomp
hybrid_re_arm <- hybrid_re_arm %>% 
  mutate(total_variance = sum(component), proportion_variance = component/total_variance, body_part = "Forearm")
hybrid_re <- data.frame(rbind(hybrid_re_sr,hybrid_re_leg,hybrid_re_arm))

## Exporting these three tables 
write.csv(hybrid_p, "Hybrid score p-vals.csv")
write.csv(hybrid_beta, "Hybrid score beta and SE.csv")
write.csv(hybrid_re, "Hybrid score random effects.csv")



# Heritability figure (Figure S6) -----------------------------------------

# Pulling data from the Leg ~ Prop drought 4 years random effects (not the most elegant way to make this plot, I know)

random_effects <- data.frame(c("Shoulder-Rump","Shoulder-Rump","Shoulder-Rump","Shoulder-Rump","Leg","Leg","Leg","Leg","Forearm","Forearm","Forearm","Forearm"),
                             c("Additive Genetic Effect", "Maternal Effect", "ID", "Residual","Additive Genetic Effect", "Maternal Effect", "ID", "Residual","Additive Genetic Effect", "Maternal Effect", "ID", "Residual"),
                             c(0.58, 0.000000257, 0.0000000607, 0.42,0.66, 0.12, 0.00116, 0.22,0.39, 0.13, 0.0000000483, 0.48))
colnames(random_effects) <- c("Body_Part", "Component", "Estimate")

# And plotting
plot_theme <- function(...){
  theme_minimal() + theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12), legend.title = element_text(size = 14), axis.title = element_text(size = 14), strip.text.x = element_text(size = 14))
}
ggplot(random_effects, aes(x=Body_Part, y = Estimate, fill = Component)) + 
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))+ 
  geom_bar(position = "stack", stat = "identity") + plot_theme() + 
  labs(x="Body Part", subtitle = "Random effect estimates for models of 4 years drought") + 
  scale_fill_manual(values=c("#00CDAC", "#CFF800", "#0065A2", "#FF5768"))
ggsave("Heritability figure.jpg", width = 6, height = 4, units = "in")



# Importance of sample size (Figure S2) -----------------------------------------------

## Shoulder-rump
# Creating new dataframe, sr_15, for baboons with at least 15 images
sr_15 <- sr %>% 
  group_by(ID) %>% count() %>%
  right_join(., sr, by = "ID") %>%
  filter(n > 14)
length(unique(sr_15$ID)) # 54 females with at least 10 photos, 14 with at least 15 photos

# I want to pick a baboon, create a temp dataframe with only her rows of data, randomly sample 15 rows, and calculate the %CV of SR from those 15 rows. 
names <- as.list(unique(sr_15$ID)) # List of the baboons we want the function to cycle through

# Practice!
temp1 <- sr_15 %>% filter(ID == "1") # Creates a temp data frame with all the rows from one baboon
temp2 <- data.frame(sample(temp1$sr_cm, 15, FALSE)) # Randomly samples 15 photos from that baboon; keeps only th sr_cm data
colnames(temp2)[1] <- "x" # Renames that dataframe so I can easily use it (I'm sure someone better at coding would do the prior line better so this wasn't necessary)
temp3 <- temp2 %>% summarise(n=length(x), mean = mean(x), sd = sd(x), CV_prct = 100*sd/mean) # Calculate mean, sd, %CV, and # photos (sanity check) for that random sample
temp3$ID <- "APP" # Attaches the ID to this lil data frame

# The function
sr_n_photos <- function(names, nphotos){
  temp1 <- sr_15 %>% filter(ID == names) # Creates a temp data frame with all the rows from one baboon
  temp2 <- data.frame(sample(temp1$sr_cm, nphotos, FALSE)) # Randomly samples 15 photos from that baboon; keeps only th sr_cm data
  colnames(temp2)[1] <- "x" # Renames that dataframe so I can easily use it (I'm sure someone better at coding would do the prior line better so this wasn't necessary)
  temp3 <- temp2 %>% summarise(n=length(x), mean = mean(x), sd = sd(x), CV_prct = 100*sd/mean) # Calculate mean, sd, %CV, and # photos (sanity check) for that random sample
  temp3$ID <- names # Attaches the ID to this lil data frame
  return(temp3)
}
# If I had more time and/or was better at functions, I'd make it so this single function cycles through all nphoto options. But...I'm just going to hand-do that 

# Getting the data frames for all 15 subsamples
sr15 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 15))) # Running the function
sr14 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 14))) # Running the function
sr13 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 13))) # Running the function
sr12 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 12))) # Running the function
sr11 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 11))) # Running the function
sr10 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 10))) # Running the function
sr9 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 9))) # Running the function
sr8 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 8))) # Running the function
sr7 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 7))) # Running the function
sr6 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 6))) # Running the function
sr5 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 5))) # Running the function
sr4 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 4))) # Running the function
sr3 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 3))) # Running the function
sr2 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 2))) # Running the function
sr1 <- as.data.frame(t(sapply(names, sr_n_photos, nphotos = 1))) # Running the function

# Combining them into one dataset
sr_15_df <- rbind(sr15, sr14, sr13, sr12, sr11, sr10, sr9, sr8, sr7, sr6, sr5, sr4, sr3, sr2, sr1)
sr_15_df <- as.data.frame(sr_15_df)
# Make figure
sr_15_df$n <- as.numeric(sr_15_df$n)
sr_15_df$mean <- as.numeric(sr_15_df$mean)
sr_15_df$sd <- as.numeric(sr_15_df$sd)
sr_15_df$CV_prct <- as.numeric(sr_15_df$CV_prct)
sr_15_df$ID <- as.character(sr_15_df$ID)

## Leg
leg_15 <- leg %>% 
  group_by(ID) %>% count() %>%
  right_join(., leg, by = "ID") %>%
  filter(n > 14)
length(unique(leg_15$ID)) # 74 females with at least 10 photos, 35 with at least 15 photos

names <- as.list(unique(leg_15$ID)) # List of the baboons we want the function to cycle through
leg_n_photos <- function(names, nphotos){
  temp1 <- leg_15 %>% filter(ID == names) # Creates a temp data frame with all the rows from one baboon
  temp2 <- data.frame(sample(temp1$leg_cm, nphotos, FALSE)) # Randomly samples 15 photos from that baboon; keeps only th sr_cm data
  colnames(temp2)[1] <- "x" # Renames that dataframe so I can easily use it (I'm sure someone better at coding would do the prior line better so this wasn't necessary)
  temp3 <- temp2 %>% summarise(n=length(x), mean = mean(x), sd = sd(x), CV_prct = 100*sd/mean) # Calculate mean, sd, %CV, and # photos (sanity check) for that random sample
  temp3$ID <- names # Attaches the ID to this lil data frame
  return(temp3)
}

leg15 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 15))) # Running the function
leg14 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 14))) # Running the function
leg13 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 13))) # Running the function
leg12 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 12))) # Running the function
leg11 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 11))) # Running the function
leg10 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 10))) # Running the function
leg9 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 9))) # Running the function
leg8 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 8))) # Running the function
leg7 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 7))) # Running the function
leg6 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 6))) # Running the function
leg5 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 5))) # Running the function
leg4 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 4))) # Running the function
leg3 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 3))) # Running the function
leg2 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 2))) # Running the function
leg1 <- as.data.frame(t(sapply(names, leg_n_photos, nphotos = 1))) # Running the function

leg_15_df <- rbind(leg15, leg14, leg13, leg12, leg11, leg10, leg9, leg8, leg7, leg6, leg5, leg4, leg3, leg2, leg1)
leg_15_df <- as.data.frame(leg_15_df)

leg_15_df$n <- as.numeric(leg_15_df$n)
leg_15_df$mean <- as.numeric(leg_15_df$mean)
leg_15_df$sd <- as.numeric(leg_15_df$sd)
leg_15_df$CV_prct <- as.numeric(leg_15_df$CV_prct)
leg_15_df$ID <- as.character(leg_15_df$ID)

## Arm
arm_15 <- forearm %>% 
  group_by(ID) %>% count() %>%
  right_join(., forearm, by = "ID") %>%
  filter(n > 14)
length(unique(arm_15$ID)) # 78 females with at least 10 photos, 49 with at least 15 photos
names <- as.list(unique(arm_15$ID)) # List of the baboons we want the function to cycle through

arm_n_photos <- function(names, nphotos){
  temp1 <- arm_15 %>% filter(ID == names) # Creates a temp data frame with all the rows from one baboon
  temp2 <- data.frame(sample(temp1$forearm_cm, nphotos, FALSE)) # Randomly samples 15 photos from that baboon; keeps only th sr_cm data
  colnames(temp2)[1] <- "x" # Renames that dataframe so I can easily use it (I'm sure someone better at coding would do the prior line better so this wasn't necessary)
  temp3 <- temp2 %>% summarise(n=length(x), mean = mean(x), sd = sd(x), CV_prct = 100*sd/mean) # Calculate mean, sd, %CV, and # photos (sanity check) for that random sample
  temp3$ID <- names # Attaches the ID to this lil data frame
  return(temp3)
}

arm15 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 15))) # Running the function
arm14 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 14))) # Running the function
arm13 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 13))) # Running the function
arm12 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 12))) # Running the function
arm11 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 11))) # Running the function
arm10 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 10))) # Running the function
arm9 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 9))) # Running the function
arm8 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 8))) # Running the function
arm7 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 7))) # Running the function
arm6 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 6))) # Running the function
arm5 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 5))) # Running the function
arm4 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 4))) # Running the function
arm3 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 3))) # Running the function
arm2 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 2))) # Running the function
arm1 <- as.data.frame(t(sapply(names, arm_n_photos, nphotos = 1))) # Running the function

arm_15_df <- rbind(arm15, arm14, arm13, arm12, arm11, arm10, arm9, arm8, arm7, arm6, arm5, arm4, arm3, arm2, arm1)
arm_15_df <- as.data.frame(arm_15_df)

arm_15_df$n <- as.numeric(arm_15_df$n)
arm_15_df$mean <- as.numeric(arm_15_df$mean)
arm_15_df$sd <- as.numeric(arm_15_df$sd)
arm_15_df$CV_prct <- as.numeric(arm_15_df$CV_prct)
arm_15_df$ID <- as.character(arm_15_df$ID)

## Figures
cvplot1 <- ggplot(subset(sr_15_df, n > 1), aes(x=reorder(factor(n), n), y=CV_prct)) + 
  geom_violin(draw_quantiles=c(0.5)) + 
  ylim(0,13) +
  labs(x="Number of images per baboon", y = "Within-baboon percent CV", subtitle = "Shoulder-rump, 14 baboons") +
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")) 
cvplot2 <- ggplot(subset(leg_15_df, n > 1), aes(x=reorder(factor(n), n), y=CV_prct)) + 
  geom_violin(draw_quantiles=c(0.5)) + 
  ylim(0,13) +
  labs(x="Number of images per baboon", y = "", subtitle = "Leg, 35 baboons") +
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")) 
cvplot3 <- ggplot(subset(arm_15_df, n > 1), aes(x=reorder(factor(n), n), y=CV_prct)) + 
  geom_violin(draw_quantiles=c(0.5)) + 
  ylim(0,13) +
  labs(x="Number of images per baboon", y = "", subtitle = "Forearm, 49 baboons") +
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")) 
cvplot1+cvplot2+cvplot3
ggsave("CV x # Photos.jpg", width = 8, height = 4, units = "in")

# Another graph with this info, with
sr_15_df2 <- sr_15_df %>% 
  group_by(ID) %>% 
  mutate(mean15 = mean[n==15], prct_of_15 = mean/mean15*100) %>%
  ggplot(., aes(x=reorder(factor(n), n), y=prct_of_15)) + 
  geom_violin(draw_quantiles=c(0.5)) + 
  ylim(89,112) +
  labs(x="Number of images per baboon", y = "Percent of the 15-image mean", subtitle = "Shoulder-rump, 14 baboons") +
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))   
leg_15_df2 <- leg_15_df %>% 
  group_by(ID) %>% 
  mutate(mean15 = mean[n==15], prct_of_15 = mean/mean15*100) %>%
  ggplot(., aes(x=reorder(factor(n), n), y=prct_of_15)) + 
  geom_violin(draw_quantiles=c(0.5)) + 
  ylim(89,112) +
  labs(x="Number of images per baboon", y = "", subtitle = "Leg, 35 baboons") +
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))   
arm_15_df2 <- arm_15_df %>% 
  group_by(ID) %>% 
  mutate(mean15 = mean[n==15], prct_of_15 = mean/mean15*100) %>%
  ggplot(., aes(x=reorder(factor(n), n), y=prct_of_15)) + 
  geom_violin(draw_quantiles=c(0.5)) + 
  ylim(89,112) +
  labs(x="Number of images per baboon", y = "", subtitle = "Forearm, 49 baboons") +
  theme(axis.text = element_text(color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "lightgrey"), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"))   
sr_15_df2+leg_15_df2+arm_15_df2
ggsave("Mean% x # Photos.jpg", width = 8, height = 4, units = "in")


