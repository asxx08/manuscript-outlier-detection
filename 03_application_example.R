# Code for analysing the example synthetic data

# Make sure path is ok
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################################################

# Load required packages (install if not installed already)
load.lib <- c("boot", "dplyr", "ggplot2", "ggpubr", "MASS", "tibble", "lme4", 
              "foreach", "magrittr", "DescTools", "openxlsx", "cowplot",
              "moments")

# Then we select only the packages that aren't currently installed
install.lib <- load.lib[!load.lib %in% installed.packages()]

# And finally we install the missing packages, including their dependency
for (lib in install.lib) install.packages(lib, dependencies = TRUE)

# After the installation process completes, we load all packages
sapply(load.lib, require, character = TRUE)

# Load required functions
source("02_functions.R")

################################################################################

# Load data
load("example_data.Rdata")

# Collapse to unit-level data
dat_unit <- dat_ind %>%
  group_by(id) %>%
  summarise(size  = n(),
            events = sum(observed),
            expected = sum(predicted)) %>%
  # Calculate risk-adjusted mortality
  mutate(RAR = (events/expected) * (sum(events)/sum(size)))

# Benchmark
benchmark <- with(dat_unit, (sum(events)/sum(expected)) * (sum(events)/sum(size)))
benchmark

# Estimate overdispersion factor
dat_unit$raw_z <- test.stat(dat_unit$size, benchmark, dat_unit$RAR)
dat_OF <- compute.OD.null(dat_unit$raw_z)
dat_OF

# Compute test statistics from the CMM and RELR approaches
dat_unit$z.cmm <- test.stat.CMM(benchmark, dat_unit$RAR, dat_unit$size, dat_OF)
dat_unit$z.relr <- test.stat.RELR(dat_ind, dat_ind$observed, dat_ind$predicted, 
                                  RA = T)[[1]]

# P-values from the CMM and RELR approaches
dat_unit$p.cmm <- pnorm(dat_unit$z.cmm, mean = 0, sd = 1, lower.tail = TRUE)
dat_unit$p.relr <- pnorm(dat_unit$z.relr, mean = 0, sd = 1, lower.tail = TRUE)

# Outlier status from the CMM and RELR approaches
dat_unit$outlier.cmm <- ifelse(dat_unit$p.cmm <= 0.025 | dat_unit$p.cmm >= 0.975, 1, 0)
dat_unit$outlier.type.cmm <- ifelse(dat_unit$p.cmm <= 0.025 & dat_unit$outlier.cmm == 1,
                                    "good",
                                    ifelse(dat_unit$p.cmm >= 0.975 & dat_unit$outlier.cmm == 1,
                                           "bad",
                                           NA))

dat_unit$outlier.relr <- ifelse(dat_unit$p.relr <= 0.025 | dat_unit$p.relr >= 0.975, 1, 0)
dat_unit$outlier.type.relr <- ifelse(dat_unit$p.relr <= 0.025 & dat_unit$outlier.relr == 1,
                                     "good",
                                     ifelse(dat_unit$p.relr >= 0.975 & dat_unit$outlier.relr == 1,
                                            "bad",
                                            NA))

# The CMM identifies three bad outliers
# The RELR identifies two bad outliers and a good outlier

################################################################################

# Use QQ plots to assess the normality of the test statistics from the two methods

# Create QQ plot for CMM
figure_cmm <- ggplot(data.frame(z = dat_unit$z.cmm), aes(sample = z)) +
  stat_qq_line(color = "grey", linewidth = 2) +
  stat_qq(color = "black", size = 3) +
  labs(x = "theoretical quantiles", y = "sample quantiles", 
       title = "QQ plot of test statistics\nfrom the CMM approach") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20, colour = 'black'),
        axis.text.y = element_text(size = 20, colour = 'black'),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 25, face = "bold")) +
  scale_y_continuous(limits = c(-3.5, 4.5), breaks = seq(-4, 4, 1)) +
  scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) 
figure_cmm

# Create QQ plot for RELR
figure_relr <- ggplot(data.frame(z = dat_unit$z.relr), aes(sample = z)) +
  stat_qq_line(color = "grey", linewidth = 2) +
  stat_qq(color = "black", size = 3) +
  labs(x = "theoretical quantiles", y = "sample quantiles", 
       title = "QQ plot of test statistics\nfrom the RELR approach") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20, colour = 'black'),
        axis.text.y = element_text(size = 20, colour = 'black'),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 25, face = "bold")) +
  scale_y_continuous(limits = c(-3.5, 4.5), breaks = seq(-4, 4, 1)) +
  scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) 
figure_relr

# Arrange side by side
figure <- plot_grid(figure_cmm, figure_relr, ncol = 2)
figure

################################################################################

# Use skewness, Kurtosis, and Lin's CCC to assess the normality of the test statistics
# from the two methods

normality_stats <- list(skewness = c(CMM = skewness(dat_unit$z.cmm),
                                     RELR = skewness(dat_unit$z.relr)),
                        kurtosis = c(CMM = kurtosis(dat_unit$z.cmm),
                                     RELR = kurtosis(dat_unit$z.relr)),
                        Lin_CCC = anadata.Lin(nrow(dat_unit),
                                              dat_unit$z.cmm, dat_unit$z.relr))
normality_stats


# The data seem more likely to come from RELR compared to CMM
# Therefore the RELR outlier detection method might be more appropriate
# Use of the CMM method leads to overdetection of bad outliers and underdetection 
# of good outliers

################################################################################

# Example for generating funnel plots for the CMM 

# Obtain control limits
dat.CL <- funnel_limit(dat_unit$size, 0.05, benchmark, dat_OF)

# Only show outliers' ID
fp_data <- dat_unit %>%
  mutate(id = ifelse(id %in% id[outlier.cmm == 1], id, " "))

# Generate funnel plot
ggplot(aes(x = size, y = RAR), data = fp_data) +
  # Add benchmark values    
  geom_abline(intercept = benchmark, slope = 0, linewidth = 2, colour = "grey") +
  geom_point(size = 3) + 
  # Plot control limits
  geom_line(aes(x = countColumn, y = ul), lty = "dashed", colour = "blue",
            data = dat.CL, linewidth = 2) +
  geom_line(aes(x = countColumn, y = ll), lty = "dashed", colour = "blue", 
            data = dat.CL, linewidth = 2) +
  labs(x = "hospital size", y = "risk-adjusted mortality") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20, colour = 'black'),
        axis.text.y = element_text(size = 20, colour = 'black'),
        axis.title = element_text(size = 25)) +
  geom_text(aes(label = id, hjust = "outward", vjust = "outward"), size = 8) 

