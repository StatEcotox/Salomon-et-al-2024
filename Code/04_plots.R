# Load packages
library(metafor)
library(orchaRd)
library(cowplot)
library(ggplot2)

# Import adjusted orchaRd 2.0 functions
source("Code/get_data_raw_adjusted.R")
source("Code/mod_results_adjusted.R")

# Import models
models <- readRDS("Results/models.rds")
full <- models$full

# Import data for selection of groups with at least 1 data point
dat <- readRDS("Data/analysis_dat.rds")
t <- data.frame(unclass(table(dat$DOM_type_grouped, dat$DOM_conditioned)))
sel.cond <- rownames(t)[t$yes > 0]
sel.coexp <- rownames(t)[t$no > 0]

# Orchard plot for groups with 'yes' condition (conditioned)
res.cond <- mod_results_adjusted(full, mod = "DOM_type_grouped", group = "pub_ID", subset = TRUE,
                                 at = list(DOM_type_grouped = sel.cond, DOM_conditioned = "yes"))

# Orchard plot for groups with 'no' condition (pre-exposure)
res.coexp <- mod_results_adjusted(full, mod = "DOM_type_grouped", group = "pub_ID", subset = TRUE,
                                  at = list(DOM_type_grouped = sel.coexp, DOM_conditioned = "no")) 

# Create plot for conditioned
plot1 <- orchard_plot(res.cond, xlab = "", alpha = 0.3,
                      trunk.size = 6, branch.size = 1.2,
                      twig.size = 0.5, legend.pos = "none") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14)) +
  expand_limits(x = c(0, 4), y = c(-5, 4)) +
  annotate("text", x = 4.2, y = -5, label = "A", size=5, fontface="bold") +
  scale_fill_manual(values = c("#DDCC77", "#6A51A3", "#44AA99", "#E66100"))

# Create plot for co-exposure
plot2 <- orchard_plot(res.coexp, xlab = "log(RR)", alpha = 0.3,
                      trunk.size = 6, branch.size = 1.2,
                      twig.size = 0.5, legend.pos = "none") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size=14)) +
  expand_limits(x = c(0, 4), y = c(-5, 4)) +
  annotate("text", x = 4.2, y = -5, label = "B", size=5, fontface="bold") +
  annotate("text", x = 2.9, y = -5, label = "*", size=10) +
  annotate("text", x = 1.9, y = -5, label = "*", size=10) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#C7EAE5"))

# Combine plots using cowplot
combined_plot <- plot_grid(plot1, plot2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 1))

# save orchard plot
png("Plots/Figure02_Orchard_plot.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
combined_plot
dev.off()

# Funnel plot:
cols <- palette.colors(palette="Tableau10")
cols= c("blue4","grey30",cols,"#CC6600")
cols <- cols[as.numeric(factor(dat$pub_ID))]

# save funnel plot
png("Plots/Figure03_Funne_plotl.png", width = 16, height = 14,units = "cm", res = 500)
funnel(full, col=cols, back = "white", shade = "grey90")
dev.off()

### END ###
