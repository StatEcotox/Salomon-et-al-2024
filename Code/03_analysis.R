
# Load packages
library(metafor)
library(orchaRd)

# Import adjusted orchaRd 2.0 functions
source("Code/get_data_raw_adjusted.R")
source("Code/mod_results_adjusted.R")


# Import data
dat2 = readRDS("Data/analysis_dat.rds")

# Check data structure
str(dat2)
summary(as.data.frame(dat2))

# Full model ---
full = rma.mv(yi, vi, mods = ~ DOM_type_grouped + DOM_conditioned - 1 , random = ~ 1 | pub_ID, data = dat2)
# Check for residual heterogeneity
full 
# Calculate I^2:
i2_ml(full) 
# Check log-likelihood profile of sigma^2
profile(full)

# check residuals
hist(residuals(full))
plot(residuals(full) ~ dat2$DOM_conditioned)
plot(residuals(full) ~ dat2$DOM_type_grouped)


# Fit reduced models to test for moderator effects ----
notype = rma.mv(yi, vi, mods = ~ DOM_conditioned -1, random = ~ 1 | pub_ID, data = dat2)
nocond = rma.mv(yi, vi, mods = ~ DOM_type_grouped -1 , random = ~ 1 | pub_ID, data = dat2)

# Test whether DOM_type_grouped contributes to explaining the variance in the data
anova(full, notype, refit = TRUE) 
# Test whether DOM_conditioned contributes to explaining the variance in the data
anova(full, nocond, refit = TRUE) 

# Get MLE and p-values for different treatment groups:
t <- data.frame(unclass(table(dat2$DOM_type_grouped, dat2$DOM_conditioned)))

sel.cond <- rownames(t)[t$yes > 0]
(res.cond <- mod_results_adjusted(full, mod = "DOM_type_grouped", group = "pub_ID", subset = TRUE,
                                 at = list(DOM_type_grouped = sel.cond, DOM_conditioned = "yes")))

sel.coexp <- rownames(t)[t$no > 0]
(res.coexp <- mod_results_adjusted(full, mod = "DOM_type_grouped", group = "pub_ID", subset = TRUE,
                                  at = list(DOM_type_grouped = sel.coexp, DOM_conditioned = "no")))


# Check funnel plot for publication bias

funnel(full)

## save results:---
saveRDS(list(full = full, notype = notype, nocond = nocond),
        "Results/models.rds")

### END ###

