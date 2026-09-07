# probe_capture_spec_error.R ----------------------------------------------
# Read the ACTUAL error for a spec that produced no .rds, instead of inferring
# one. 004 wraps every fit in tryCatch(error = function(e) invisible()), so the
# message that would name the cause is discarded and the array task exits 0.
# This replicates 004's setup for ONE spec and calls the estimator with NO
# handler, on a SINGLE draw.
#
# Two hypotheses have already died here (see probe_spec_rank_diagnostics.R):
# thin strata, and a singular intercept-shifter block. Neither survived contact
# with the data. This script does not test a third guess -- it prints the
# condition and its call.
#
# Usage -- from the repo root, edit TARGET or set it first:
#   TARGET <- "CropID_Cocoyam_credit_hh_TL_hnormal_optimal.rds"   # cheapest crop, n=494
#   TARGET <- "CropID_Pooled_credit_hh_TL_weibull_optimal.rds"    # a Group-A distribution
#   source("studies/financial_inclusion/probes/probe_capture_spec_error.R")
# -------------------------------------------------------------------------
devtools::load_all(".", quiet = TRUE)

if (!exists("TARGET")) TARGET <- "CropID_Cocoyam_credit_hh_TL_hnormal_optimal.rds"
cat("TARGET:", TARGET, "\n\n")

project_name <- "financial_inclusion"
study_environment <- readRDS(file.path(paste0("studies/", project_name, "/data"),
                                       paste0(project_name, "_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

estimation_data <- study_environment[["estimation_data"]]
estimation_data$EduCat <- as.character(estimation_data$EduCat)
distforms <- sf_functional_forms()$distforms
fxnforms  <- sf_functional_forms()$fxnforms

model_specifications <- sf_model_specifications(
  distforms = distforms, fxnforms = fxnforms,
  data = study_environment$estimation_data,
  technology_variables = c("credit_hh", "credit_close", "credit_member",
                           "credit_spouse", "credit_self", "credit_child"))
model_specifications$file <- paste0(
  model_specifications$disasg, "_", model_specifications$level, "_",
  model_specifications$TechVar, "_", names(fxnforms)[model_specifications$f], "_",
  names(distforms)[model_specifications$d], "_", model_specifications$matching_type, ".rds")

fit <- which(model_specifications$file == TARGET)
stopifnot(length(fit) == 1)

match_specifications <- study_environment$match_specifications
match_specifications <- match_specifications[!grepl("linear", match_specifications$link), ]
match_specifications <- match_specifications[match_specifications$boot %in% 0,
                                             c("ARRAY", "method", "distance", "link")]

f <- model_specifications$f[fit]; d <- model_specifications$d[fit]
disaggregate_variable <- model_specifications$disasg[fit]
disaggregate_level    <- model_specifications$level[fit]
technology_variable   <- model_specifications$TechVar[fit]
matching_type         <- model_specifications$matching_type[fit]

# ---- 004's data-prep block, verbatim ------------------------------------
data <- estimation_data[estimation_data[, disaggregate_variable] %in% disaggregate_level, ]
data <- data[!data[, technology_variable] %in% NA, ]
data$Tech <- as.numeric(as.integer(as.factor(as.character(data[, technology_variable]))))
if (!disaggregate_variable %in% "CropID") data <- data[data[, "CropID"] %in% "Pooled", ]

technology_legend <- unique(data[c("Tech", technology_variable)])
technology_legend <- technology_legend[order(technology_legend$Tech), ]

crop_area_list <- study_environment$crop_area_list
for (crop in crop_area_list) data[, crop] <- data[, crop] / data[, "Area"]
crop_area_list <- apply(data[names(data)[names(data) %in% crop_area_list]], 2, mean) > 0.03
crop_area_list <- names(crop_area_list)[crop_area_list %in% TRUE]
for (crop in gsub("Area_", "", crop_area_list))
  data[, paste0("CROP_", crop)] <- ifelse(data[, paste0("Area_", crop)] > 0, crop, NA)
crop_area_list <- unique(c(crop_area_list, "Area_Other"))
if (length(crop_area_list) > 0) {
  data$Area_Other <- 1 - rowSums(data[crop_area_list[!crop_area_list %in% "Area_Maize"]], na.rm = TRUE)
  crop_area_list  <- unique(c(crop_area_list, "Area_Other"))
}

drawlist <- study_environment$sample_draw_list
one_draw <- unique(drawlist$ID)[1]

cat("n rows            :", nrow(data), "\n")
cat("treated / control :", sum(data[, technology_variable] %in% 1), "/",
    sum(data[, technology_variable] %in% 0), "\n")
cat("Tech levels       :", paste(technology_legend$Tech, collapse = ", "), "\n")
cat("shifters          :", paste(crop_area_list, collapse = ", "), "\n")
cat("draws available   :", length(unique(drawlist$ID)), " -- using", one_draw, "\n")
cat("distribution      :", names(distforms)[d], "   functional form:", names(fxnforms)[f], "\n\n")

# ---- the call, with NO tryCatch -----------------------------------------
cat("---- calling draw_msf_estimations on ONE draw, unguarded ----\n\n")
res <- withCallingHandlers(
  draw_msf_estimations(
    one_draw,
    data                    = data,
    surveyy                 = FALSE,
    intercept_shifters      = list(scalar_variables = crop_area_list,
                                   factor_variables = c("Survey", "Ecozon")),
    intercept_shifters_meta = list(scalar_variables = crop_area_list,
                                   factor_variables = c("Survey", "Ecozon")),
    drawlist                = drawlist,
    weight_variable         = "Weight",
    output_variable         = "HrvstKg",
    input_variables         = c("Area", "SeedKg", "HHLaborAE", "HirdHr", "FertKg", "PestLt"),
    inefficiency_covariates = list(scalar_variables = c("lnAgeYr", "lnYerEdu", "CrpMix"),
                                   factor_variables = c("Female", "Survey", "Ecozon",
                                                        "Extension", "EqipMech", "OwnLnd")),
    adoption_covariates     = list(scalar_variables = c("lnAgeYr", "lnYerEdu", "CrpMix"),
                                   factor_variables = c("Female", "Survey", "Ecozon",
                                                        "Extension", "EqipMech", "OwnLnd")),
    identifiers             = c("unique_identifier", "Survey", "CropID", "HhId", "EaId", "Mid"),
    disagscors_list         = NULL,
    f = f, d = d,
    technology_variable     = technology_variable,
    matching_type           = matching_type,
    match_specifications        = match_specifications,
    match_specification_optimal = study_environment$match_specification_optimal[
                                    c("ARRAY", "method", "distance", "link")],
    match_path                  = study_environment$wd$matching),
  warning = function(w) {
    cat("WARNING:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  })

cat("\n---- RETURNED WITHOUT ERROR ----\n")
str(res, max.level = 1)
cat("\nIf this completes, the spec is fittable and the Beocat task died for a\n",
    "reason outside the estimator -- a wall-clock kill being the obvious one.\n",
    "Check sacct before concluding anything else.\n", sep = "")
