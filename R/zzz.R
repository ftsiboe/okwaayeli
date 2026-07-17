.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # NSE column names used by data.table / dplyr / ggplot2, so R CMD check does
  # not report them as undefined globals.
  #
  # ONLY column names belong here. Never add a FUNCTION name: that silences the
  # check instead of declaring the dependency, and the call then works only
  # where something else happens to have attached the package. Qualify it
  # (pkg::fn) and declare pkg in Imports.
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        " .data EaId glss stat value . TE_OLS Treat DATA CoefName DONE Estimate Survey TGR Tech crop_area_list estType
    input restrict risk seed Estimate_mean GHAgricProductivityLab_control binned_range_level
     binned_range_name count count_sum estimate_count estimate_weight
     weight_tmp weights_sum
    technology_variable type study_environment Surveyx Weight",
        "\\s+"
      )[[1]]
    )
  }
}

