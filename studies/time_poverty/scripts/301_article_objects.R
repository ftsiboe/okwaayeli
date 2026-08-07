# 301_article_objects.R
# Assemble the numbers the narrative pulls from and write article_objects.json.
# Working directory is always the okwaayeli repo root.
#
# Everything must be extracted from the same estimation objects that
# scripts/101_exhibit_figures.R uses, so the manuscript text and the exhibits
# cannot drift apart. That is the whole reason this stage exists rather than the
# prose quoting numbers by hand.
#
# STATUS: PARTIAL. The sample-description block below is real and runs today off
# the study environment. The estimate block does not exist -- output/estimations/
# is empty (004 has never run) and there is no manuscript telling us which
# numbers the prose will quote. See scripts/README.md.
#
# KEYING TRAP, recorded now so it is not rediscovered the hard way. In 004,
#   data$Tech <- as.numeric(as.integer(as.factor(as.character(data[, TechVar]))))
# so a 0/1 treatment becomes 1/2. `Tech` is an analysis label and must NOT be
# used to split groups. TCHLvel is the labelled column: "National" (naive),
# the two technology levels, and "Meta" (meta-frontier). Key on TCHLvel.
if (!exists("OBJECTS_JSON")) source("studies/time_poverty/scripts/article_helpers.R")
suppressPackageStartupMessages(library(jsonlite))

EST <- file.path(OUTPUT, "estimations")

se_path <- file.path(DATA, "time_poverty_study_environment.rds")
if (!file.exists(se_path))
  stop("301_article_objects.R: missing ", se_path,
       "\n  Run 001 + 002 first.", call. = FALSE)
se <- readRDS(se_path)

mspecs     <- se$match_specification_optimal
opt_sample <- if (!is.null(mspecs)) ifelse(is.na(mspecs$link), mspecs$distance, mspecs$link) else NA

objs <- list()

# --- 1) Sample description ----------------------------------------------------
# Real, and computable today. The prose will quote these regardless of how the
# results turn out.
raw <- se$study_raw_data
est <- se$estimation_data
objs$sample <- list(
  survey_rounds     = if (!is.null(raw)) sort(unique(as.character(raw$Surveyx))) else NULL,
  n_raw             = if (!is.null(raw)) nrow(raw) else NA_integer_,
  n_estimation      = if (!is.null(est)) nrow(est) else NA_integer_,
  n_pooled_crop     = if (!is.null(est)) sum(as.character(est$CropID) %in% "Pooled") else NA_integer_,
  treated_share     = if (!is.null(est) && "Treat" %in% names(est))
                        mean(est$Treat, na.rm = TRUE) else NA_real_,
  treatment_variable = "tpoor0150",
  matching_specs    = if (!is.null(se$match_specifications)) nrow(se$match_specifications) else NA_integer_,
  optimal_sample    = opt_sample
)

# TREATMENT DEFINITION -- carried into the JSON so the paper cannot describe it
# wrongly by accident. scripts/time_poverty_DATA.do computes a committed-time
# cutoff, then restricts to s1q3 == 1 and RECOMPUTES the same variable names off
# PAID time, overwriting the first. tpoor0150 in the release is the paid-time
# version, not the committed-time one its labels claim. See that file's FLAG.
objs$treatment_definition <- paste(
  "tpoor0150: indicator that daily paid time exceeds 1.5x the median of paid",
  "time, computed on the s1q3 == 1 subsample of GLSS7. NOTE: the variable label",
  "in the harmonized release says 'Committed Time'; that is stale --",
  "time_poverty_DATA.do overwrites the committed-time version. Verify before",
  "the paper describes the measure.")

# --- 2) Estimates -------------------------------------------------------------
# NOT WRITTEN. Stops rather than emitting a JSON whose estimate fields are all
# null: a half-populated article_objects.json renders as a document full of
# blanks that looks like a formatting problem, not a missing pipeline.
if (!length(list.files(EST, pattern = "\\.rds$")))
  stop("301_article_objects.R: output/estimations/ is empty -- 004 has never run.\n",
       "  Run it first: sbatch studies/time_poverty/scripts/job_msf.sbatch\n",
       "  Then write the estimate block below, keyed on TCHLvel (see the header).",
       call. = FALSE)

stop("301_article_objects.R: the estimate block is NOT WRITTEN.\n",
     "  output/estimations/ has objects now, so this is the next thing to do.\n",
     "  Copy the shape from studies/resource_extraction/scripts/301_article_objects.R;\n",
     "  key on TCHLvel, filter to opt_sample ('", opt_sample, "'), and delete this stop().",
     call. = FALSE)

# jsonlite::write_json(objs, OBJECTS_JSON, auto_unbox = TRUE, pretty = TRUE, na = "null")
