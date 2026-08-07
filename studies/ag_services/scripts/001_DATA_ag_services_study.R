# =============================================================================
#  DATA and SETUP - AGRICULTURAL SERVICES STUDY 
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This script prepares analysis-ready data for the agricultural services study within the
#  okwaayeli project. It:
#    - Initializes a study-specific environment (folders, paths, metadata),
#    - Loads harmonized farm/household and agricultural services modules,
#    - Merges them at the household-farmer level,
#    - Restricts the sample to relevant GLSS waves,
#    - Saves both the processed study dataset and the study environment object
#      to disk for downstream analysis.
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection
rm(list = ls(all = TRUE)); gc()              

# ---- Rebuild package documentation (if this is part of a package) 
# This calls roxygen2 via devtools to regenerate .Rd docs and NAMESPACE.
devtools::document()                         

run_only_for(id = 8, allowed_jobnames = "run_all")

# ---- Define study name and initialize study environment
project_name <- "ag_services"

# study_setup() is assumed to:
#   - create / verify directories,
#   - define paths (e.g., wd$home, wd$data, wd$output),
#   - and return a list-like "study_environment" object
#     containing configuration for this specific project.
study_environment <- study_setup(project_name = project_name, layout = "v2")

# =============================================================================
#  STAGE 000 - BUILD THE AG SERVICES RELEASE, OR USE THE SAVED ONE
# =============================================================================
#  Added 2026-08-07.
#
#  WHY THIS IS NOT JUST A system2() CALL
#  -------------------------------------
#  get_household_data() does NOT read data-raw/releases/harmonized_data/. It
#  downloads the .dta from the GitHub release (ftsiboe/GHAgricProductivityLab,
#  tag "hh_data") into an R user cache, and `force = TRUE` deletes that cache
#  and re-downloads. Since that published release is still PRE-AUDIT, calling it
#  here would silently pull data that does not reproduce this paper -- exactly
#  the failure data-raw/okwaayeli_DATA.do's own header describes.
#
#  So ag services is read from the LOCAL release, always.
#
#  STATA IS OPTIONAL
#  -----------------
#  Where Stata exists, the release is rebuilt from the GLSS community files
#  first. Where it does not -- an HPC node, a co-author's machine, CI -- the
#  saved release is used instead. That is a legitimate input, not a degraded
#  one: the .dta is the release.
#
#  What is NOT optional is the schema contract below. It runs on every path, so
#  a file predating the 2026-08-07 audit can never be used silently, whether it
#  came from Stata, from git, or from a colleague.
#
#  The farmer-level data is unaffected by the audit and still comes from GitHub.
# =============================================================================

HARMONIZE <- TRUE   # TRUE = rebuild with Stata when available; FALSE = never try

.REL <- "data-raw/releases/harmonized_data"
.DO  <- "studies/ag_services/scripts/000_HARMONIZE_ag_services_data.do"
.DST <- file.path(.REL, "harmonized_ag_services_data.dta")
.rebuilt <- FALSE

if (isTRUE(HARMONIZE)) {

  # -- locate Stata; absence is not an error --------------------------------
  .stata <- Sys.getenv("STATA_EXE", unset = NA)
  if (is.na(.stata)) {
    .w <- Sys.which(c("stata-se", "stata-mp", "stata"))
    .cand <- c(Sys.glob("C:/Program Files/Stata*/Stata*-64.exe"),
               Sys.glob("C:/Program Files (x86)/Stata*/Stata*-64.exe"),
               .w[nzchar(.w)])
    .cand <- .cand[nzchar(.cand) & file.exists(.cand)]
    .stata <- if (length(.cand)) .cand[1] else NA
  }

  if (is.na(.stata)) {
    message("001: no Stata on this machine -- using the SAVED release at ", .DST, ".\n",
            "     Set STATA_EXE to rebuild it here. The schema check below still applies.")
  } else {
    message("001: rebuilding the release with ", basename(.stata))
    stopifnot(file.exists(.DO))
    .log <- "studies/ag_services/scripts/logs/harmonize.log"
    unlink(.log)
    .flag <- if (.Platform$OS.type == "windows") "/e" else "-b"
    .rc <- system2(.stata, c(.flag, "do", shQuote(normalizePath(.DO, winslash = "/"))),
                   wait = TRUE)

    # Stata batch mode returns 0 even on error, so READ THE LOG. A Stata error
    # is a line matching ^r(NNN); -- that is the only reliable signal, and
    # trusting the exit code would let a failed harmonization pass as success.
    if (!file.exists(.log))
      stop("001: Stata produced no log at ", .log, " (exit ", .rc, ").\n",
           "  The do-file did not start.", call. = FALSE)
    .lines <- readLines(.log, warn = FALSE)
    .err   <- grep("^r\\([0-9]+\\);", .lines, value = TRUE)
    if (length(.err))
      stop("001: 000_HARMONIZE failed -- Stata returned ",
           paste(unique(.err), collapse = " "), "\n  Last 40 log lines:\n",
           paste("   ", utils::tail(.lines, 40), collapse = "\n"),
           "\n  Full log: ", .log, call. = FALSE)

    # A run that errored before the save would leave the previous file in place
    # and look like success. Freshness applies ONLY on this path.
    if (!file.exists(.DST))
      stop("001: 000_HARMONIZE reported no error but wrote no ", basename(.DST), ".",
           call. = FALSE)
    if (difftime(Sys.time(), file.mtime(.DST), units = "mins") > 30)
      stop("001: ", basename(.DST), " is older than 30 minutes. Stata did not ",
           "rewrite it,\n  so this run would proceed on a stale release.", call. = FALSE)
    .rebuilt <- TRUE
  }
}

# ---- Load harmonized farmer-level data (unaffected by the audit; from GitHub)
farmer_data <- get_household_data("harmonized_crop_farmer_data", force = TRUE)

# ---- Load ag services from the LOCAL release, never via get_household_data()
if (!file.exists(.DST))
  stop("001: no ag services release at ", .DST, ".\n",
       "  Nothing to fall back on. Run ", .DO, " on a machine with Stata, or\n",
       "  copy the .dta in from one.", call. = FALSE)
ag_services_data <- as.data.frame(haven::read_dta(.DST))
message("001: ag services read from ", .DST, "  (", nrow(ag_services_data), " rows, ",
        ncol(ag_services_data), " cols, built ",
        format(file.mtime(.DST), "%Y-%m-%d %H:%M"),
        if (.rebuilt) ", rebuilt this run)" else ", SAVED build)")

# ---- Schema contract: enforced on EVERY path, rebuilt or saved --------------
# These columns exist only in builds from 2026-08-07 onward. Their absence means
# the file predates the audit and carries the GLSS5 MOFA zeros and the
# fabricated GLSS4/GLSS5 compliance values. Failing here is the point: a
# pre-audit release must never be used by accident on a machine that cannot
# rebuild it.
.need <- c("services0_strict", "services1_strict", "services2_strict",
           "services3_strict", "extension_office", "extension_officer",
           "extension_officer_visit")
.miss <- setdiff(.need, names(ag_services_data))
if (length(.miss))
  stop("001: the ag services release is missing ", paste(.miss, collapse = ", "),
       ".\n  It predates the 2026-08-07 audit and does not reproduce this paper.\n",
       "  Rebuild it with ", .DO, ", or obtain a build from after that date.",
       call. = FALSE)

# ---- Merge farmer and agricultural services data at the household-member level
# Merge keys:
#   - Surveyx : survey round 
#   - EaId    : enumeration area
study_data <- dplyr::inner_join(
  farmer_data,
  ag_services_data,
  by = c("Surveyx", "EaId")
)

# ---- Restrict to relevant survey rounds and drop certain variables
study_data <- study_data[
  study_data$Surveyx %in% c("GLSS5","GLSS6","GLSS7"),
]

# ---- Attach raw data to study environment (potential issue)
study_environment$study_raw_data <- study_data

# ---- Save study environment object
# Save the entire study environment configuration (paths, metadata, etc.)
# for reproducibility and to simplify subsequent scripts.
saveRDS(
  study_environment,
  file.path(study_environment$wd$data, paste0(project_name,"_study_environment.rds"))
)
