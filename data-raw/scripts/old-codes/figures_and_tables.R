# DEPRECATED SHIM -- 2026-07-15
#
# The figure and table builders that used to live here are now packaged in
# R/exhibits-figures.R and exported:
#
#   ers_theme()             tab_main_specification()  fig_heterogeneity00()
#   fig_robustness()        fig_input_te()            fig_covariate_balance()
#   fig_dsistribution()
#
# This file remains only so that the studies still calling
#   source("data-raw/scripts/figures_and_tables.R")
# keep working. They are:
#
#   studies/ag_services/100_FIGTAB_ag_services.R
#   studies/disability/100_FIGTAB_disability_study.R
#   studies/education/100_FIGTAB_education_study.R
#   studies/financial_inclusion/100_FIGTAB_financial_inclusion_study.R
#   studies/resource_extraction/scripts/100_exhibits.R
#   studies/time_poverty/100_FIGTAB_time_poverty_study.R
#
# (studies/financial_inclusion/101_heterogeneity_financial_inclusion_figures.R
# sources `paste0(getwd(), "/codes/figures_and_tables.R")` -- a path that has not
# existed for some time. That script is already broken; this shim does not fix it.)
#
# WHY IT WAS PACKAGED. Seven copies of a source() path, no documentation, no
# NAMESPACE, and top-level library() calls that attached ggplot2 and friends into
# whatever session happened to source it. The package declares those in
# DESCRIPTION and reaches them through the namespace instead.
#
# TO RETIRE THIS FILE: in each study above, replace the source() line with
#
#   if (!requireNamespace("okwaayeli", quietly = TRUE)) devtools::load_all(".")
#   library(okwaayeli)
#
# and drop the library(magrittr/ggplot2/gridExtra/dplyr/gtable/stringr/cowplot)
# lines, which only existed because this file attached them. Delete this shim
# once no study sources it. studies/land_tenure/scripts/101_exhibit_figures.R is
# already ported and is the pattern to copy.
#
# The pre-packaging original is kept verbatim at
# data-raw/scripts/old-codes/figures_and_tables.R.

if (!requireNamespace("okwaayeli", quietly = TRUE)) {
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else stop("figures_and_tables.R shim: install/load the okwaayeli package first.",
            call. = FALSE)
}
suppressPackageStartupMessages(library(okwaayeli))

message("figures_and_tables.R is a deprecated shim; the builders are now in ",
        "okwaayeli (R/exhibits-figures.R). See the header to retire it.")
