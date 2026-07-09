# For the manuscript "Choosing an appropriate outlier-detection method when 
# comparing institutional performances in clinical audits"

# Functions for the Common Mean Model (CMM), Random Effects Logistic Regression 
# (RELR), and QQ-Lin

# Author: Anqi Sui  anqi.sui.18@ucl.ac.uk
# 09/07/2026

################################################################################

#' Generate test statistics from the CMM without any correction or adjustment

#' test.stat
#'
#' Inputs:
#'
#' @param size                 a vector, the unit size for all units
#' @param benchmark            a scalar, the underlying true performance for all 
#'                             units, i.e. benchmark
#' @param performance          a vector, the observed performances for all units
#' 
#' Output: a vector, the 'raw' test statistics from the CMM method 

test.stat <- function(size, benchmark, performance){

  # Compute the inverse of the binomial variance
  variance_inv <- size / (benchmark * (1 - benchmark)) 
  
  # Compute the test statistics from the CMM (without any correction)
  z_score <- (performance - benchmark) * sqrt(variance_inv)
  
  return(z_score)
}

################################################################################

#' Estimate the overdispersion factor using the multiplicative overdispersion correction

#' compute.OD.null
#'
#' Inputs:
#'
#' @param z     a vector, 'raw' test statistics from the CMM
#' 
#' Output: a scalar, the estimated overdispersion factor

compute.OD.null <- function(z){
  
  return(max(mean(z^2), 1))
}

################################################################################

#' Compute test statistics from the overdispersion-corrected CMM method

#' test.stat.CMM
#'
#' Inputs:
#'
#' @param benchmark            a scalar, the underlying true performance for all 
#'                             units, i.e. benchmark
#' @param performance          a vector, the observed performances for all units
#' @param size                 a vector, the unit sizes for all units
#' @param OF                   a scalar, estimated overdispersion factor from 
#'                             the multiplicative overdispersion correction
#' 
#' Output: a vector, test statistics from the overdispersion-corrected CMM

test.stat.CMM <- function(benchmark, performance, size, OF){
  
  # The binomial variance with multiplicative overdispersion correction 
  adj_variance <- ((benchmark * (1 - benchmark)) / size) * OF
  
  # Compute the test statistics from the overdispersion corrected CMM 
  z.cmm <- (performance - benchmark) / sqrt(adj_variance) 
  
  return(z.cmm)
}

################################################################################

#' Compute test statistics from the RELR method, with or withour risk adjustment

#' test.stat.RELR
#'
#' Inputs:
#'
#' @param ind_df       a dataframe, patient-level dataset
#' @param y            a vector, the column in 'ind_df' that represents 
#'                     patient-level binary outcomes for each patient 
#' @param pred         a vector, the column in 'ind_df' that represents 
#'                     patient-level predicted risk; used only when RA = TRUE
#' @param RA           a logical, if TRUE, fit a risk-adjusted RELR model using
#'                     logit(pred) as a fixed effect; if FALSE, fit an unadjusted 
#'                     model     
#' 
#' Output:  a list with three elements
#'          -  the test statistics from the RELR method
#'          -  the estimated random effects from the RELR method
#'          -  the diagnostic standard error from the RELR method
#'       

test.stat.RELR <- function(ind_df, y, pred = NULL, RA = FALSE){
  
  # Fit RELR model
  if (RA) {fitted <- glmer(y ~ logit(pred) + (1 | id),
                           data = ind_df, family = binomial("logit"), 
                           nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))} 
  else {fitted <- glmer(y ~ (1 | id), 
                        data = ind_df, family = binomial("logit"),
                        nAGQ = 0,
                        control = glmerControl(optimizer = "nloptwrap"))}
  
  # Extract the random effects
  randoms <- ranef(fitted, condVar = TRUE)
  RandomEffect.RELR <- randoms$id[, 1]
  
  # Compute the comparative standard error for each random effect
  variances <- attr(ranef(fitted, condVar = TRUE)[[1]], "postVar")
  comVar <- as.vector(variances)
  
  # Extract the posterior variance of the random effects
  sigmau2 <- as.numeric(VarCorr(fitted))
  
  # Calculate the diagnostic standard error
  diagnostic_var <- sigmau2 - comVar
  DiagnosticSE <- sqrt(diagnostic_var)
  
  # Compute the test statistics
  z.relr <- RandomEffect.RELR / DiagnosticSE
  
  return(list(z.relr, RandomEffect.RELR, DiagnosticSE))
}

################################################################################

#' Calculate Lin's CCC for test statistics from the CMM and RELR

#' anadata.Lin
#'
#' Inputs:
#'
#' @param N                    a scalar, no. of units
#' @param z.CMM                a vector, test statistics from the CMM method
#' @param z.RELR               a vector, test statistics from the RELR method
#' 
#' Output:  a vector with three elements
#'          -  Lin's CCC for the test statistics from the CMM method
#'          -  Lin's CCC for the test statistics from the RELR method
#'          -  name of the method whose test statistics give a higher Lin's CCC value
#'     

anadata.Lin <- function(N, z.CMM, z.RELR){
  
  # Theoretical normal quantiles
  theoretical <- qnorm(ppoints(N), mean = 0, sd = 1)
  
  # Compute Lin's CCC for the CMM test statistics
  ccc.CMM <- CCC(theoretical, sort(z.CMM))$rho.c
  ccc.CMM <- ccc.CMM[[1]]
  # Compute Lin's CCC for the RELR test statistics
  ccc.RELR <- CCC(theoretical, sort(z.RELR))$rho.c
  ccc.RELR <- ccc.RELR[[1]]
  
  decision.Lin <- NA
  
  # Output the name of the method whose test statistics give a higher Lin's CCC value
  if (ccc.CMM > ccc.RELR) {
    decision.Lin <- "CMM"
  }
  if (ccc.CMM < ccc.RELR) {
    decision.Lin <- "RELR"
  }
  
  return(c(CMM = ccc.CMM, RELR = ccc.RELR, preferred_method = decision.Lin))
}

################################################################################

#' Compute control limits for funnel plots

#' funnel_limit
#'
#' Inputs:
#'
#' @param size                 a vector, the unit sizes for all units
#' @param significance         a scalar, significance level used to detect outliers
#'                             on a funnel plot
#' @param benchmark            a scalar, the underlying true performance for all 
#'                             units, i.e. benchamrk
#' @param OF                   a scalar, estimated overdispersion factor from 
#'                             the multiplicative overdispersion correction
#' 
#' Output:  a dataframe with three columns
#'          -  unit size 
#'          -  corresponding upper control limit
#'          -  corresponding lower control limit
#'  

funnel_limit <- function(size, significance, benchmark, OF){
  
  countColumn <- seq(min(size), max(size), 1)
  
  # Critical z scores
  z_p <- qnorm(p = 1 - significance / 2, mean = 0, sd = 1, lower.tail = T)
  
  # Control limits 
  ul <- benchmark + z_p * sqrt(OF * (benchmark * (1-benchmark) / countColumn))
  ll <- benchmark - z_p * sqrt(OF * (benchmark * (1-benchmark) / countColumn))
  
  ul[ul < 0] <- 0
  ll[ll < 0] <- 0
  
  result <- data.frame(countColumn, ul, ll)
  colnames(result) <- c('countColumn', "ul", "ll")  
  
  return(result)
}
