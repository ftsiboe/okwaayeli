.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # Register non-standard-evaluation column names used by data.table / dplyr /
  # ggplot2, so R CMD check does not report them as undefined globals.
  #
  # ONLY column names belong here. The list previously also carried translogCalc,
  # translogCheckCurvature, translogCheckMono, translogEla and translogMonoRestr
  # -- micEcon FUNCTIONS that sf_workhorse() called bare. Listing them here
  # silenced the check without declaring the dependency: the calls resolved only
  # because every 004_MSF_*.R does library(micEcon), so sf_workhorse() worked
  # from a SLURM script and would fail anywhere else. They are micEcon::
  # qualified now and micEcon is in Imports; removed from this list so the check
  # can catch it if anyone reintroduces a bare call.
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        " .data EaId glss stat value . TE_OLS Treat DATA CoefName DONE Estimate Survey TGR Tech crop_area_list estType
    group_by input restrict risk seed solve.QP summarise Estimate_mean GHAgricProductivityLab_control binned_range_level
     binned_range_name count count_sum estimate_count estimate_weight
     weight_tmp weights_sum
    technology_variable type study_environment Surveyx Weight",
        "\\s+"
      )[[1]]
    )
  }
}

