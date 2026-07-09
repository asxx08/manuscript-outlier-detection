# Code for generating an example synthetic data

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

################################################################################

# Simulate artificial dataset mimicking AugUR data
n <- 88

# Simulate unit size
set.seed(66577)
ni <- as.integer(rgamma(88, shape = 2.355565, rate = 0.000790989))

# Simulate probability of events
set.seed(98908897)
prob <- rbeta(88, 4.4871, 180.3101)

# Compute no. of events
events <- as.integer(ni * prob)

# Create unit-level data
dat_unit <- data.frame(id = seq(1, 88, 1),
                       size = ni,
                       events = events)
dat_unit <- arrange(dat_unit, id)

# Unroll unit-level data into patient-level data
expand_unit <- function(unit_id, unit_size, O_i) {
  data.frame(id = unit_id,
             observed = c(rep(1, O_i), rep(0, (unit_size - O_i))))
}
dat_ind <- do.call(rbind, with(dat_unit, 
                               mapply(expand_unit, id, size, events, SIMPLIFY = FALSE)))
dat_ind <- arrange(dat_ind, id)

# Simulate individual patients' risks, using a mixture of Beta distributions
set.seed(5676578)
# 3% extreme values
mix <- rbinom(nrow(dat_ind), 1, 0.03)  
pred <- ifelse(mix == 1, rbeta(nrow(dat_ind), 1.8, 4.2), 
               rbeta(nrow(dat_ind), 0.2044654, 10.1931))
dat_ind$predicted <- pred

################################################################################

# Save dataset
rm(list = setdiff(ls(), c("dat_ind")))

save.image("example_data.Rdata")

