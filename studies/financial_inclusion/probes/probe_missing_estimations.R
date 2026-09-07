# probe_missing_estimations.R ---------------------------------------------
# Report-only. Which model_specifications rows produced no .rds, and how big
# each missing stratum is.
#
# 004 wraps every fit in tryCatch() and skips a spec whose file already exists,
# so a spec that dies leaves NO trace: the array task exits 0 and the run looks
# complete. After the 2026-09-07 Beocat run, output/estimations/ holds 45 files
# against 60 array tasks. This says which 15, and whether the cause is a thin
# stratum (benign, and a fact the paper must state) or something else.
#
#   source("studies/financial_inclusion/probes/probe_missing_estimations.R")
# -------------------------------------------------------------------------
devtools::load_all(".", quiet = TRUE)

se <- readRDS(file.path("studies", "financial_inclusion", "data",
                        "financial_inclusion_study_environment.rds"))
ed <- se$estimation_data
ed$EduCat <- as.character(ed$EduCat)          # exactly as 004 does before the call

ff <- sf_functional_forms()
ms <- sf_model_specifications(
  distforms = ff$distforms, fxnforms = ff$fxnforms, data = ed,
  technology_variables = c("credit_hh", "credit_close", "credit_member",
                           "credit_spouse", "credit_self", "credit_child"))

ms$file <- paste0(ms$disasg, "_", ms$level, "_", ms$TechVar, "_",
                  names(ff$fxnforms)[ms$f], "_", names(ff$distforms)[ms$d],
                  "_", ms$matching_type, ".rds")

have <- list.files(file.path("studies", "financial_inclusion", "output", "estimations"))
ms$built <- ms$file %in% have

cat(sprintf("\nspecs = %d   built = %d   missing = %d\n",
            nrow(ms), sum(ms$built), sum(!ms$built)))
cat(sprintf("files on disk not in specs: %s\n\n",
            paste(setdiff(have, ms$file), collapse = ", ")))

# For each missing spec: the subsample 004 would have fitted on, and its two
# arms. A frontier needs both arms non-empty and enough of each to identify a
# metafrontier; report them rather than assuming which bound bit.
miss <- ms[!ms$built, ]
if (nrow(miss)) {
  rows <- lapply(seq_len(nrow(miss)), function(i) {
    d <- ed[ed[, miss$disasg[i]] %in% miss$level[i], ]
    tv <- miss$TechVar[i]
    d <- d[!d[, tv] %in% NA, ]
    if (!miss$disasg[i] %in% "CropID") d <- d[d[, "CropID"] %in% "Pooled", ]
    data.frame(spec = miss$file[i], n = nrow(d),
               n_treated  = sum(d[, tv] %in% 1),
               n_control  = sum(d[, tv] %in% 0),
               n_ea = length(unique(d$EaId)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  print(out[order(out$n), ], row.names = FALSE)
  cat("\nA spec with n_treated or n_control at 0 cannot yield a credit gap at all.\n",
      "A spec with a handful in either arm is a convergence failure, not a result.\n",
      "Either way the paper must say the cut was attempted and did not identify,\n",
      "rather than silently reporting the strata that happened to converge.\n", sep = "")
}
