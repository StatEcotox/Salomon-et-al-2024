
# Load packages
library(metafor)

# Import data
dat = read.csv("Data/Salomon-et-al_Raw_data.csv", header = TRUE, stringsAsFactors = TRUE)
# str(dat)
# tail(dat)

# 1. Mobile/immobile individuals -----
# Calculate the number of immobile individuals for the control
dat$c_immobile = round(dat$c_mean * dat$c_n_individuals)
# Calculate the number of mobile individuals for the control
dat$c_mobile = dat$c_n_individuals - dat$c_immobile

# Calculate the number of immobile individuals for the treatment
dat$t_immobile = round(dat$t_mean * dat$t_n_individuals)
# Calculate the number of mobile individuals for the treatment
dat$t_mobile = dat$t_n_individuals - dat$t_immobile


#Check whether all samples have complete response data: > there should be no NA values here any more
sum(is.na(dat$c_mobile)) == 0
sum(is.na(dat$t_mobile)) == 0
sum(is.na(dat$c_immobile)) == 0
sum(is.na(dat$t_immobile)) == 0

# 2. Calculate effect sizes ----
# Calculate log risk ratios (log(RR)) for immobilization  
dat2 <- escalc(measure = "RR", ai = dat$t_immobile, bi = dat$t_mobile,
               ci = dat$c_immobile, di = dat$c_mobile, data = dat, slab = pub_ID)

# Display the structure of the calculated effect sizes
str(dat2)

# 3. Select columns for analysis ----

analysis_dat = subset(dat2, select = c(pub_ID, year, 
                                       DOM_conditioned, DOM_type_grouped, DOM_conc_mg_L,
                                       yi, vi))

# Save results ----
saveRDS(analysis_dat, "Data/analysis_dat.rds")

### END
