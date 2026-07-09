"Choosing an appropriate outlier-detection method when comparing institutional 
performances in clinical audits"

Authors: A. Sui, G. Ambler, M.A. de Belder, R. Z. Omar, and M. Pavlou

-------------------------------------------------------------------------------

Code was written by A. Sui.

Version information for the software (operating system, statistical libraries 
and add-on packages) can be found in the end of this document

For any questions please contact anqi.sui.18@ucl.ac.uk

###############################################################################

This repository contains all R scripts used for the CMM, RELR, and QQ-Lin methods 
presented in the manuscript

-------------------------------------------------------------------------------
R files included

1. 01_generate_example_synthetic_data.R
2. 02_functions.R
3. 03_application_example.R

File 1 includes the code to generate an example synthetic dataset 

File 2 contains the necessary functions to implement the CMM, RELR, and QQ-Lin 
methods as presented in the manuscript

File 3 includes an application of the methods to the example synthetic dataset, producing 
similar figures and tables in the application section (Section 5) in the manuscript

-------------------------------------------------------------------------------
Run the scripts in the following order

1. 01_generate_example_synthetic_data.R
2. 02_functions.R
3. 03_application_example.R

###############################################################################

The code was written/evaluated in R with the following software versions:
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
[3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.utf8    

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] moments_0.14.1    cowplot_1.1.3     openxlsx_4.2.8.1  DescTools_0.99.59
 [5] magrittr_2.0.3    foreach_1.5.2     lme4_1.1-35.5     Matrix_1.7-0     
 [9] tibble_3.2.1      MASS_7.3-61       ggpubr_0.6.0      ggplot2_3.5.1    
[13] dplyr_1.1.4       boot_1.3-30      

loaded via a namespace (and not attached):
 [1] gld_2.6.7         gtable_0.3.6      rstatix_0.7.2     lattice_0.22-6   
 [5] vctrs_0.6.5       tools_4.4.1       generics_0.1.3    proxy_0.4-27     
 [9] fansi_1.0.6       pkgconfig_2.0.3   data.table_1.16.4 readxl_1.4.3     
[13] lifecycle_1.0.4   rootSolve_1.8.2.4 compiler_4.4.1    Exact_3.3        
[17] munsell_0.5.1     codetools_0.2-20  carData_3.0-5     class_7.3-22     
[21] Formula_1.2-5     pillar_1.9.0      car_3.1-3         nloptr_2.1.1     
[25] tidyr_1.3.1       iterators_1.0.14  abind_1.4-8       nlme_3.1-164     
[29] zip_2.3.3         tidyselect_1.2.1  stringi_1.8.4     mvtnorm_1.3-1    
[33] purrr_1.0.2       forcats_1.0.0     splines_4.4.1     grid_4.4.1       
[37] colorspace_2.1-1  lmom_3.2          expm_1.0-0        cli_3.6.3        
[41] utf8_1.2.4        broom_1.0.7       e1071_1.7-16      withr_3.0.1      
[45] scales_1.3.0      backports_1.5.0   httr_1.4.7        ggsignif_0.6.4   
[49] cellranger_1.1.0  hms_1.1.3         haven_2.5.4       rlang_1.1.4      
[53] Rcpp_1.0.13       glue_1.7.0        rstudioapi_0.17.1 minqa_1.2.8      
[57] R6_2.5.1         

###############################################################################
