# probes/probe_frontier.R
# ==============================================================================
# The ONLY thing still unknown: which (TCHLvel, sample, restrict) combination
# each published column of Tables 4, S3 and S4 was drawn from.
#
# Reads ONE estimation object. Seconds, not minutes.
#
# probe_exhibits.R reads six, each carrying ef_samp/ef_dist -- one row per farmer
# per draw, tens of MB compressed apiece, and nothing here touches them. That was
# over-built: the other five files are only needed to confirm Table 5's per-tag
# frontiers exist, which a directory listing answers just as well.
#
# From the repo root:
#   source("studies/financial_inclusion/probes/probe_frontier.R")
# ==============================================================================

STUDY <- "studies/financial_inclusion"
EST   <- file.path(STUDY, "output", "estimations")
LOGD  <- file.path(STUDY, "probes", "logs")
dir.create(LOGD, recursive = TRUE, showWarnings = FALSE)

out <- utils::capture.output({

  cat("probe_frontier.R  |  ", format(Sys.time()), "\n", sep = "")

  cat("\n== Table 5 needs one frontier per person-with-credit variable ==\n")
  for (tg in c("credit_hh","credit_self","credit_spouse","credit_child",
               "credit_close","credit_member")) {
    f <- file.path(EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tg))
    cat(sprintf("  %-16s %s\n", tg, if (file.exists(f)) "present" else "MISSING"))
  }

  E <- readRDS(file.path(EST, "CropID_Pooled_credit_hh_TL_hnormal_optimal.rds"))

  cat("\n== sf_estm: TCHLvel x sample x restrict ==\n")
  print(with(E$sf_estm, table(TCHLvel, sample, restrict)))

  # v005 Table 4 prints: National 15860 | No credit 14455 | Some credit 1405
  #                      Meta matched 2810 | Meta unmatched 15860
  # Matching those numbers to the grid below names every column unambiguously.
  cat("\n== sf_estm: Nobs -- pins each published column ==\n")
  n <- E$sf_estm[E$sf_estm$CoefName == "Nobs", ]
  print(n[order(n$sample, n$TCHLvel),
          c("TCHLvel","sample","restrict","Survey","Estimate")])

  cat("\n== sf_estm: diagnostics rows present (Table 4 bottom block) ==\n")
  want <- c("Nobs","nXvar","mlLoglik","mono","curv","olsSkew","CoelliM3Test",
            "LRT","LRInef","Gamma","Sigma","sigmauSq","sigmavSq")
  print(intersect(want, unique(E$sf_estm$CoefName)))

  # v005 Table 4 "Land": National 0.753 | No credit 0.751 | Some credit 0.572
  #                      Meta matched 0.650 | Meta unmatched 0.712
  cat("\n== el_mean: el1 == v005's 'Land' elasticity row ==\n")
  x <- E$el_mean[E$el_mean$input == "el1", ]
  print(x[order(x$sample, x$TCHLvel),
          c("TCHLvel","sample","stat","restrict","Survey","Estimate","Estimate.sd","jack_pv")])

  # v005 Table 4 TGR: No credit matched 0.795 / unmatched 0.934
  #                   Some credit matched 0.810 / unmatched 0.888
  cat("\n== ef_mean: TGR/TE/MTE, mean, GLSS0, teBC ==\n")
  y <- E$ef_mean[E$ef_mean$stat == "mean" & E$ef_mean$Survey == "GLSS0" &
                 E$ef_mean$estType == "teBC", ]
  print(y[order(y$type, y$sample, y$TCHLvel),
          c("type","TCHLvel","sample","restrict","Estimate","Estimate.sd","jack_pv")])

  cat("\n== sf_estm: lnI1 == v005 Table S3 'Land [lnI1]' ==\n")
  z <- E$sf_estm[E$sf_estm$CoefName == "lnI1", ]
  print(z[order(z$sample, z$TCHLvel),
          c("TCHLvel","sample","restrict","Survey","Estimate","StdError","Estimate.sd","jack_pv")])

  cat("\n== sf_estm: Zu_factor(Female)1 == v005 Table S4 row 1 ==\n")
  w <- E$sf_estm[E$sf_estm$CoefName == "Zu_factor(Female)1", ]
  print(w[order(w$sample, w$TCHLvel),
          c("TCHLvel","sample","restrict","Survey","Estimate","StdError","Estimate.sd","jack_pv")])

  cat("\ndone.\n")
})

writeLines(out, file.path(LOGD, "probe_frontier.log"))
cat(out, sep = "\n")
cat("\nwrote ", file.path(LOGD, "probe_frontier.log"), "\n", sep = "")
