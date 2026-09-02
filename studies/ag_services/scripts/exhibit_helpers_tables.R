# exhibit_helpers_tables.R
# Build the manuscript tables as flextable objects.
#
# A LIBRARY, NOT A STEP: no position in a sequence, hence no number. Sourced by
# narrative/ag-services.Rmd at knit time and by 102. See scripts/README.md.
#
# STATUS 2026-08-07: chassis + ft_table7() only. The remaining builders are
# stubs that stop() with the name of the builder to write. That is deliberate --
# see NO FALLBACKS below. Verify ft_table7() against the frozen reference before
# the next builder is written; the pattern proven here is the pattern reused.
#
# SOURCES
#   Table 7 (draft) == `Table4` sheet of the retired ag_services_results-msf.xlsx
#                      ef_mean, matched sample, services0-3
#
# NO FALLBACKS. Every builder errors rather than degrading to a stored value. A
# builder that falls back to a frozen CSV lets the knit "succeed" while printing
# stale numbers beside prose citing live ones. A failed render is cheaper.
#
# KEYING -- READ THIS BEFORE ADDING A BUILDER.
# RE-PINNED 2026-08-07 from the DATA, by reverse lookup: searching ef_mean for
# the published Table 7 values and reading off the rows that produced them.
# (scripts/probe-ag_services.R, step 2b.) The earlier pin, taken from the
# retired workbook's column headers, was wrong in three places -- it is kept
# below only as a warning about what NOT to assume.
#
# ef_mean is 3,840 rows x 20 cols. A published cell needs EIGHT keys to be
# unique; fewer silently matches several rows:
#
#   sample     "mahalanobis" (the optimal match) | "unmatched"
#   Survey     "GLSS0" (pooled) | GLSS5 | GLSS6 | GLSS7
#   type       "TGR" | "TE" | "MTE" | "TE0"     <- THE METRIC lives here
#   estType    "teBC" | "teJLMS" | "teMO"       <- Battese-Coelli is published
#   stat       "wmean" | mean | median | mode   <- weighted mean is published
#   restrict   "Restricted" | "Unrestricted"
#   CoefName   "efficiency"          -> a LEVEL column
#              "efficiencyGap_lvl"   -> the DIFFERENCE column
#              "efficiencyGap_pct"   -> not used in Table 7
#   TCHLvel    "0" = NO SERVICES (reference) | "1" = SOME SERVICES | "National"
#
# WHAT THE WORKBOOK-BASED PIN GOT WRONG, and must not be reintroduced:
#   - there is NO `level_type` column. Level vs gap is CoefName.
#   - there is NO `Std..Error` / `Pr...z..`. Use `Estimate.sd` and `jack_pv`.
#   - CoefName does NOT carry the metric. `type` does.
#   - there is NO Tech == 999 and no TCHLvel == "Meta". The meta-frontier is
#     type == "MTE", crossed with the SAME TCHLvel levels as TGR and TE.
#
# On Tech vs TCHLvel: in ef_mean the two agree one-to-one and the cross-tab has
# no off-diagonal mass (-999<->National, 1<->"0", 2<->"1"), so the transposition
# trap does not arise HERE. It may still arise in sf_estm, which is a different
# object -- Tables 6, S7 and S8 read that one. Do not generalise this result.
# .level_key() below still refuses to fall back to Tech, on purpose.
#
# Row count checks out exactly: sample(2) x type(4) x estType(3) x Survey(4) x
# stat(4) x restrict(2) = 768 per TCHLvel level. "National" and "0" carry only
# CoefName == "efficiency" (768 each); "1" carries all three CoefName values
# (2,304), because a gap is defined only against the reference. 768+768+2304 =
# 3,840.
#
# services0-3 are four SEPARATE binary treatments (four estimation objects),
# not one multi-level treatment. Each has its own two-group frontier.
.ft_ok <- tryCatch({ loadNamespace("flextable"); TRUE },
                   error = function(e) conditionMessage(e))
if (!isTRUE(.ft_ok))
  stop("exhibit_helpers_tables.R could not load 'flextable'.\n",
       "  Reason: ", .ft_ok, "\n",
       "  If the package is missing:  install.packages(\"flextable\")\n",
       "  If a dependency is broken (systemfonts / gdtools / textshaping), restart R\n",
       "  in a clean session and run:\n",
       "    install.packages(c(\"systemfonts\",\"textshaping\",\"gdtools\",\"flextable\"))",
       call. = FALSE)
suppressPackageStartupMessages(library(flextable))

set_flextable_defaults(font.family = "Times New Roman")

# Self-contained path resolution: sourced BOTH from the repo root (run_article.R)
# and from narrative/ (the Rmd's knit_root_dir).
.STUDY_ROOT <- if (dir.exists("output/estimations")) {
  "."
} else if (dir.exists("../output/estimations")) {
  ".."
} else {
  "studies/ag_services"
}

# ---- Memoization -------------------------------------------------------------
# Once tbl_num() routes through the builders, a manuscript with ~140 lookups
# rebuilds a handful of tables that many times, each re-reading estimation
# objects that are tens of MB compressed. Cache per table id and per estimation
# object. Session-lived; call exhibit_cache_clear() after re-running a stage.
.CACHE <- new.env(parent = emptyenv())

.memo <- function(key, f) {
  if (!exists(key, envir = .CACHE, inherits = FALSE))
    assign(key, f(), envir = .CACHE)
  get(key, envir = .CACHE)
}

#' Drop the exhibit cache. Call after re-running 004 or 100 in a live session.
exhibit_cache_clear <- function() {
  rm(list = ls(.CACHE, all.names = TRUE), envir = .CACHE)
  invisible(TRUE)
}

.EST <- file.path(.STUDY_ROOT, "output", "estimations")

# Keep only the summary components. The objects also carry ef_samp/ef_dist (one
# row per farmer per draw), which dwarf everything else and which no table here
# touches -- fig_distribution() reads those from the *_fullset* file in 101.
.EST_PARTS <- c("ef_mean", "el_mean", "sf_estm", "disagscors")

.read_est <- function(tag)
  .memo(paste0("est:", tag), function() {
    f <- file.path(.EST, sprintf("CropID_Pooled_%s_TL_hnormal_optimal.rds", tag))
    if (!file.exists(f))
      stop("exhibit_helpers_tables.R: missing estimation object\n  ", f,
           "\n  Run scripts/004_MSF_ag_services_study.R (or the MSF stage of ",
           "run_article.R).", call. = FALSE)
    p <- readRDS(f)
    keep <- intersect(names(p), .EST_PARTS)
    if (!length(keep))
      stop("exhibit_helpers_tables.R: ", tag, " has none of ",
           paste(.EST_PARTS, collapse = "/"), ". Components present: ",
           paste(names(p), collapse = ", "), call. = FALSE)
    p[keep]
  })

# The optimal match specification, for keying the matched sample.
.se_path <- file.path(.STUDY_ROOT, "data", "ag_services_study_environment.rds")
.mspecs  <- if (file.exists(.se_path))
  readRDS(.se_path)$match_specification_optimal else NULL
.opt <- if (!is.null(.mspecs))
  ifelse(is.na(.mspecs$link), .mspecs$distance, .mspecs$link) else NA_character_

.samp_id <- function(s) if (identical(s, "matched")) .opt else "unmatched"

# ---- Value columns -----------------------------------------------------------
# ef_mean reports jackknife moments, not a fitted covariance matrix: the
# dispersion is Estimate.sd over Estimate.length replicates, and the test
# statistic is jack_zv / jack_pv. There is no Std..Error or Pr...z.. column.
.V_EST <- "Estimate"; .V_SE <- "Estimate.sd"; .V_P <- "jack_pv"

.check_val_cols <- function(d) {
  miss <- setdiff(c(.V_EST, .V_SE, .V_P), names(d))
  if (length(miss))
    stop("exhibit_helpers_tables.R: ef_mean lacks ", paste(miss, collapse = ", "),
         ".\n  Columns present: ", paste(names(d), collapse = ", "),
         "\n  Run scripts/probe-ag_services.R --schema and re-pin before editing.",
         call. = FALSE)
  invisible(TRUE)
}

.stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}

# Published cell format: 0.941*** (0.002)
.cell <- function(est, se, p) {
  if (length(est) == 0 || is.na(est)) return("-")
  sprintf("%.3f%s (%.3f)", est, .stars(p), se)
}

# ---- Level keying ------------------------------------------------------------
# Resolve the frontier level on the LABELLED column. If the expected labels are
# absent this STOPS and prints the codes actually present -- it never falls
# through to the numeric Tech column. Silently keying on Tech is the failure
# that produces a fully populated, correctly starred, TRANSPOSED table.
.LEVEL_LABELS <- c("0", "1", "National", "Meta")

.level_key <- function(d) {
  nm <- names(d)
  if ("TCHLvel" %in% nm) {
    present <- sort(unique(as.character(d$TCHLvel)))
    if (!any(.LEVEL_LABELS %in% present))
      stop("exhibit_helpers_tables.R: TCHLvel present but carries none of the ",
           "expected labels.\n  Expected any of: ",
           paste(.LEVEL_LABELS, collapse = ", "),
           "\n  Actually present:  ", paste(present, collapse = ", "),
           "\n  The level coding for ag_services was pinned on 2026-08-07 from ",
           "the workbook;\n  if these labels differ, re-pin against the data ",
           "BEFORE editing any builder.\n  See the KEYING block at the top of ",
           "this file.", call. = FALSE)
    return("TCHLvel")
  }
  stop("exhibit_helpers_tables.R: no labelled level column (TCHLvel) in this ",
       "object.\n  Columns present: ", paste(nm, collapse = ", "),
       "\n  Do NOT substitute the numeric Tech column -- it disagrees with the ",
       "labelled one\n  and keying on it transposes the group columns with ",
       "every star intact.", call. = FALSE)
}

.pick <- function(d, keys, col) {
  ok <- rep(TRUE, nrow(d))
  for (k in names(keys)) {
    if (!k %in% names(d))
      stop("exhibit_helpers_tables.R: column '", k, "' not in this object.\n",
           "  Columns present: ", paste(names(d), collapse = ", "), call. = FALSE)
    ok <- ok & as.character(d[[k]]) == as.character(keys[[k]])
  }
  v <- d[[col]][ok]
  if (length(v) > 1L)
    stop("exhibit_helpers_tables.R: ", length(v), " rows matched a lookup that ",
         "must be unique.\n  Keys: ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         call. = FALSE)
  v
}

# ---- Table 7 -----------------------------------------------------------------
# Draft Table 7: "Agricultural Service Parity in Technology Level and Technical
# Efficiency Based on Source of Service".
#
# Three blocks (TGR, TE, MTE) x four service sources x three columns
# (No services [A], Some services [B], Difference [B-A]).
#
# Reference for verification: msf::Table4 in
# narrative/diagnostics/verification_reference_2026-08-07.json.
# NOTE the draft disagrees with that reference in 6 of 36 cells -- all standard
# errors, all point estimates identical, and the TE row-2/row-3 "No services"
# SEs are transposed between the two. The pipeline is the authority. Expect the
# live build to match the WORKBOOK, not the draft, and log any third answer.
.SERVICE_TAGS <- c(services0 = "Any source",
                   services1 = "Agricultural/fishing association",
                   services2 = "Agricultural cooperative",
                   services3 = "Agricultural extension")

.T7_BLOCKS <- list(
  list(type = "TGR", label = "Technology gap ratio (TGR)"),
  list(type = "TE",  label = "Pure farmer technical efficiency (TE)"),
  list(type = "MTE", label = "Meta-frontier technical efficiency (MTE)"))

# Held constant across every cell of Table 7. Without estType/stat/restrict a
# TGR lookup matches FOUR rows (teBC and teJLMS x Restricted and Unrestricted),
# all carrying the same estimate -- so an unguarded .pick() would look fine on
# TGR and then break on TE. .pick()'s uniqueness check catches it either way.
.T7_FIXED <- list(Survey = "GLSS0", estType = "teBC", stat = "wmean",
                  restrict = "Restricted")

# The three published columns. TCHLvel "0" is the REFERENCE (no services).
.T7_COLS <- list(
  list(id = "A",    lvl = "0", coef = "efficiency"),
  list(id = "B",    lvl = "1", coef = "efficiency"),
  list(id = "diff", lvl = "1", coef = "efficiencyGap_lvl"))

.t7_cell <- function(tag, type, cc) {
  d  <- .read_est(tag)$ef_mean
  .check_val_cols(d)
  kc <- .level_key(d)
  keys <- c(.T7_FIXED, list(sample = .samp_id("matched"), type = type,
                            CoefName = cc$coef))
  keys[[kc]] <- cc$lvl
  .cell(.pick(d, keys, .V_EST), .pick(d, keys, .V_SE), .pick(d, keys, .V_P))
}

.tbl7_live <- function() .memo("tbl:table7", function() {
  rows <- list()
  for (b in .T7_BLOCKS) {
    rows[[length(rows) + 1L]] <- c(b$type, b$label, "", "", "")
    for (tg in names(.SERVICE_TAGS))
      rows[[length(rows) + 1L]] <- c(
        paste0(b$type, "/", unname(.SERVICE_TAGS[[tg]])),
        unname(.SERVICE_TAGS[[tg]]),
        vapply(.T7_COLS, function(cc) .t7_cell(tg, b$type, cc), character(1)))
  }
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c(".key", "Variable", "A", "B", "diff")
  if (anyDuplicated(m$.key))
    stop("exhibit_helpers_tables.R: duplicate lookup keys in table7: ",
         paste(m$.key[duplicated(m$.key)], collapse = ", "), call. = FALSE)

  # A guard that only catches EMPTY output catches nothing that matters, but an
  # all-blank table still means a broken key, so check it anyway -- and diff
  # against the frozen reference for anything subtler.
  body <- as.matrix(m[!m$A %in% "" | !m$B %in% "", c("A", "B", "diff")])
  if (mean(body %in% c("", "-")) > 0.5)
    stop("exhibit_helpers_tables.R: ft_table7() produced a mostly blank table. ",
         "That means the level key is wrong, not that the data are missing. ",
         "See the KEYING block at the top of this file.", call. = FALSE)
  m
})

ft_table7 <- function() {
  m <- .tbl7_live()
  hdr <- which(m$A == "" & m$B == "")
  m <- m[, setdiff(names(m), ".key"), drop = FALSE]   # lookup key never prints
  ft <- flextable(m)
  ft <- set_header_labels(ft, Variable = "", A = "No services\n[A]",
                          B = "Some services\n[B]", diff = "Difference\n[B - A]")
  ft <- align(ft, j = 2:4, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- italic(ft, i = hdr, j = 1, italic = TRUE)
  ft <- padding(ft, i = setdiff(seq_len(nrow(m)), hdr), j = 1, padding.left = 14)
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = c(
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
    paste("Meta Stochastic Frontier Analysis was jointly performed on Ghana",
          "Living Standards Survey [waves 5-7]."),
    paste("Standard errors were estimated via the jackknife resampling method",
          "by iteratively generating 100 resampled datasets by randomly",
          "excluding one enumeration area from each survey for every resample.")))
  ft <- fontsize(ft, size = 8, part = "footer")
  ft
}

# ---- Curated exhibits --------------------------------------------------------
# Tables S1 and S2 are the ONLY legitimate curated exhibits in this study: they
# transcribe questionnaire structure and variable construction, which no
# estimation object can compute. Everything else builds from the pipeline. If a
# third CSV appears here, that is drift, not a new exception.
#
# Provenance, recorded 2026-08-07:
#   tableS1.csv  <- the v000 draft's Table S1 (36 rows incl. 6 section headers).
#                   Preferred over the workbook's `Sheet1`, whose six section
#                   headers did not survive extraction (they came back as bare
#                   numbers 750/758/759/760/761/764 -- unresolved shared strings).
#                   Otherwise the two agree.
#   tableS2.csv  <- the workbook's `Sheet2`, preferred over the draft because it
#                   keeps GLSS6 SEPARATE from GLSS7 and carries fuller recoding
#                   notes. The draft merges them into one "GLSS7/GLSS6" column,
#                   which silently loses the "decoded to ...x" step on two rows
#                   (Extension services provided; Association services).
#                   25 rows, matching the draft exactly, after dropping the
#                   workbook's "Survey identifier" and "Enumeration area"
#                   rows -- neither is an agricultural service question, which
#                   is what the table's title promises.
.TBL_DIR <- file.path(.STUDY_ROOT, "data", "tables")

.read_tbl <- function(nm) .memo(paste0("tbl:", nm), function() {
  p <- file.path(.TBL_DIR, nm)
  if (!file.exists(p))
    stop("exhibit_helpers_tables.R: missing curated exhibit ", p,
         "\n  This file is a transcription, not a pipeline output -- it cannot ",
         "be regenerated by\n  re-running a stage. Restore it from git.",
         call. = FALSE)
  d <- utils::read.csv(p, check.names = FALSE, colClasses = "character",
                       encoding = "UTF-8")
  if (!nrow(d))
    stop("exhibit_helpers_tables.R: ", p, " is empty.", call. = FALSE)
  d
})

# Both tables carry the same note in the v000 draft. The final sentence is kept
# because it is true of the harmonization pipeline, with the bracketed caveat
# added 2026-08-07: study_raw_data holds GLSS5, GLSS6 and GLSS7 only, so GLSS4
# is harmonized upstream and then excluded, and Table S2 therefore has no GLSS4
# column.
.S_NOTES <- paste(
  "Notes: GLSS7 is used as the reference naming convention for harmonization.",
  "GLSS6 follows the same structure as GLSS7 for the main agricultural service",
  "questions. GLSS5 uses a similar structure, but the association, cooperative,",
  "and tractor questions are shifted relative to GLSS7; these are renamed before",
  "appending. GLSS4 uses an older numeric structure, so its corresponding",
  "questions are mapped into the same service categories before the final",
  "harmonized dataset is created [GLSS4 is not part of this study's estimation",
  "sample, which spans GLSS5-GLSS7].")

.curated_ft <- function(d, labels, widths, header_rows = integer(0), size = 8) {
  ft <- flextable::flextable(d)
  ft <- flextable::set_header_labels(ft, values = labels)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  if (length(header_rows)) {
    ft <- flextable::bold(ft, i = header_rows, part = "body")
    ft <- flextable::bg(ft, i = header_rows, bg = "#F2F2F2", part = "body")
    ft <- flextable::merge_h(ft, i = header_rows, part = "body")
    ft <- flextable::padding(ft, i = setdiff(seq_len(nrow(d)), header_rows),
                             j = 1, padding.left = 12)
  }
  for (j in seq_along(widths))
    ft <- flextable::width(ft, j = j, width = widths[[j]])
  ft <- flextable::fontsize(ft, size = size, part = "all")
  ft <- flextable::add_footer_lines(ft, values = .S_NOTES)
  ft <- flextable::fontsize(ft, size = 7, part = "footer")
  ft
}

.tblS1_live <- function() .memo("tbl:tableS1", function() {
  d <- .read_tbl("tableS1.csv")
  need <- c("variable", "label", "source", "construction", "is_header")
  if (!all(need %in% names(d)))
    stop("exhibit_helpers_tables.R: tableS1.csv needs columns ",
         paste(need, collapse = ", "), "; found ",
         paste(names(d), collapse = ", "), call. = FALSE)
  d
})

ft_tableS1 <- function() {
  d <- .tblS1_live()
  hdr <- which(d$is_header == "1")
  body <- d[, c("variable", "label", "source", "construction")]
  .curated_ft(body,
    labels = list(variable = "Final variable", label = "Label / meaning",
                  source = "GLSS7 source variable(s)", construction = "Construction"),
    widths = c(1.7, 2.5, 2.1, 3.0), header_rows = hdr)
}

.tblS2_live <- function() .memo("tbl:tableS2", function() {
  d <- .read_tbl("tableS2.csv")
  need <- c("concept", "glss7", "glss6", "glss5")
  if (!all(need %in% names(d)))
    stop("exhibit_helpers_tables.R: tableS2.csv needs columns ",
         paste(need, collapse = ", "), "; found ",
         paste(names(d), collapse = ", "), call. = FALSE)
  d
})

ft_tableS2 <- function()
  .curated_ft(.tblS2_live(),
    labels = list(concept = "Harmonized concept", glss7 = "GLSS7 reference",
                  glss6 = "GLSS6 source", glss5 = "GLSS5 source and recoding"),
    widths = c(2.4, 2.2, 1.9, 2.8))

# ---- Builders not yet written ------------------------------------------------
# Each stops with the name of what to write. No fallbacks, no placeholders that
# render. See narrative/sections/98_tables_and_figures.Rmd and 99_appendix.Rmd
# for which draft exhibit each corresponds to.
# PREVIEW MODE. Off by default: an unwritten builder STOPS, which is the whole
# point of "no fallbacks". Set AG_PREVIEW=1 to render a loud placeholder box
# instead, so the layout, prose flow and section breaks can be checked before
# the exhibits exist. The placeholder says what it is on its face and carries no
# numbers, so it cannot be mistaken for data or quietly survive into a draft.
#
#   Sys.setenv(AG_PREVIEW = "1")   # or AG_PREVIEW=1 in the shell
#
# NEVER set this for a build anyone will read as final.
.PREVIEW <- function() identical(Sys.getenv("AG_PREVIEW"), "1")

if (.PREVIEW())
  message("exhibit_helpers_tables.R: AG_PREVIEW=1 -- unwritten exhibits will ",
          "render as PLACEHOLDER boxes, not data. Unset it for a real build.")

.not_yet <- function(nm, note) {
  if (!.PREVIEW())
    stop("exhibit_helpers_tables.R: ", nm, "() is not written yet.\n  ", note,
         "\n  Verify ft_table7() against ",
         "narrative/diagnostics/verification_reference_2026-08-07.json first; ",
         "the pattern proven there is the pattern to reuse.",
         "\n  To preview the document layout without this exhibit, set ",
         "AG_PREVIEW=1 -- but never for a build anyone will read as final.",
         call. = FALSE)

  d <- data.frame(x = c(sprintf("PLACEHOLDER - %s() IS NOT WRITTEN", nm), note,
                        "AG_PREVIEW=1. This box contains no data."),
                  stringsAsFactors = FALSE)
  names(d) <- " "
  if (!requireNamespace("flextable", quietly = TRUE)) return(d)
  ft <- flextable::flextable(d)
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#FFF3CD", part = "body")
  ft <- flextable::color(ft, i = 1, color = "#B00020", part = "body")
  ft <- flextable::bold(ft, i = 1, part = "body")
  ft <- flextable::italic(ft, i = 3, part = "body")
  ft <- flextable::fontsize(ft, size = 9, part = "body")
  ft <- flextable::align(ft, align = "left", part = "body")
  ft <- flextable::width(ft, j = 1, width = 6.3)
  ft
}

# NOTE 2026-08-13: nothing calls .not_yet() any more -- every builder below is
# written. It and .PREVIEW() are kept so a NEWLY ADDED exhibit can be stubbed the
# same way: declare it, let it stop, and preview the layout with AG_PREVIEW=1
# until it is built. Never for a build anyone will read as final.
# ==============================================================================
#  THE REMAINING BUILDERS
# ==============================================================================
# Written 2026-08-13, in the pattern ft_table7() proved: key on labelled
# columns, look up uniquely, fail rather than degrade.
#
# LAYOUTS were recovered from the v000 draft itself
# (narrative/v000_AgricServicesProdGapGhana_FT.docx, 15 tables, read with
# python-docx) rather than guessed from the workbook, so row labels, column
# headers, section breaks and cell formats are the draft's. The NUMBERS are the
# pipeline's. Where the two disagree the disagreement is recorded at the builder
# concerned -- it is a finding, not a thing to reconcile by adjusting the code.
#
# Three such disagreements are known and deliberate. Read them before trusting a
# diff against the draft:
#
#   1. TABLES 1, S3 and S4, the "Female farmer (dummy)" row. The draft's group
#      columns there are not Female's group means -- they are the GROUP SHARES
#      (8,304/22,519 = 0.37 and 14,215/22,519 = 0.63 in Table 1; 11,752/22,519 =
#      0.52, 3,690/22,519 = 0.16, 5,713/22,519 = 0.25 in Table S3). That is the
#      Stata collision the .do fixed on 2026-07-15: its disagCat0/disagCat1 loop
#      wrote `mat roweq A = Female` and its rows landed on top of the real
#      Female outcome rows. The v000 draft predates the fix. The live build
#      prints Female's actual group means (0.24 / 0.25), which is the correction,
#      not a regression.
#
#   2. INPUTS 5 AND 6 -- FERTILIZER AND PESTICIDE -- ARE EXCHANGED BETWEEN THE
#      DRAFT AND THIS BUILD, in Table 6 and Table S7 alike, and by exactly a
#      swap:
#          draft T6  Fertilizer 0.021 / Pesticide 0.012
#          object    el5        0.012 / el6       0.021
#          draft S7  lnI5       0.031 / lnI6      0.024
#          object    lnI5       0.024 / lnI6      0.031
#      The mapping used here is positional and comes from the code as it stands:
#      004 passes input_variables = c("Area","SeedKg","HHLaborAE","HirdHr",
#      "FertKg","PestLt"); R/stochastic_frontier-core.R line 1112 assigns
#      data[, paste0("I", i)] <- data[, input_variables[i]], and line 1090 builds
#      xNames <- paste0("I", 1:number_of_inputs), which micEcon::translogEla()
#      consumes in that order before the columns are renamed el1..elN. So on the
#      CURRENT code path lnI5/el5 is fertilizer and lnI6/el6 is pesticide, which
#      is what these builders print.
#      Two readings fit the swap and this file cannot choose between them: the
#      draft mislabelled the pair, or input_variables was ordered
#      ..., "PestLt", "FertKg" when the draft was built. Resolving it means
#      looking at 004 as of the draft, not at anything here. Until then, expect
#      these two rows to disagree with the draft and do not "fix" them by
#      swapping the labels.
#
#   3. THE ESTIMATES HAVE MOVED since the frozen reference. output/estimations/
#      was rebuilt 2026-08-12 and Table 7's TE row now reads 0.561 / 0.606 /
#      0.045 against the workbook's 0.560 / 0.609 / 0.049. Table 6's meta-frontier
#      LR test and monotonicity rate moved too. Every builder here reports the
#      current build; nothing is pinned to the old one.

# ---- Cell formats -------------------------------------------------------------
# Three formats, one per kind of cell, so a builder cannot invent a fourth.
#   mean  : 46.87 (15.26)     estimate (sd)
#   trend : 0.39*** [0.06]    estimate stars [se]        -- brackets, not parens
#   coef  : 0.723*** (0.001)  estimate stars (se)
# The bracket/paren distinction is the draft's and it is load-bearing: it is how
# a reader tells a dispersion from a standard error at a glance.
.DAG <- "†"

.na_dash <- function(x) length(x) == 0L || is.na(x)

.fmt_mean <- function(m, s, digits = 2) {
  if (.na_dash(m)) return("-")
  if (.na_dash(s)) return(sprintf(paste0("%.", digits, "f"), m))
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), m, s)
}
.fmt_trend <- function(e, se, p, digits = 2) {
  if (.na_dash(e)) return("-")
  if (.na_dash(se))
    return(sprintf(paste0("%.", digits, "f%s"), e, .stars(p)))
  sprintf(paste0("%.", digits, "f%s [%.", digits, "f]"), e, .stars(p), se)
}
.fmt_coef <- function(e, se, p, digits = 3) {
  if (.na_dash(e)) return("-")
  if (.na_dash(se)) return(sprintf(paste0("%.", digits, "f%s"), e, .stars(p)))
  sprintf(paste0("%.", digits, "f%s (%.", digits, "f)"), e, .stars(p), se)
}
.fmt_plain <- function(x, digits = 0) {
  if (.na_dash(x)) return("-")
  formatC(x, format = "f", digits = digits, big.mark = "")
}

# ==============================================================================
#  THE DESCRIPTIVE CACHE  (Engine A and Engine B -> Tables 1, 2, 3, 4, 5, S3, S4)
# ==============================================================================
# 100_exhibit_descriptive_stats.R writes data/descriptive_exhibits.rds:
#   $table1  Engine A, long and KEYED -- (treatment, crop, outcome, wave, group,
#            statistic). statistic is "mean" | "trend_pct" | "cat_diff" |
#            "trend_diff"; group is "pooled" | the treatment level, or NA on the
#            two Wald rows.
#   $shares  Engine B -- (crop, outcome, wave, statistic). wave is "pooled" for
#            the share and "trend" for the semi-elasticity.
#
# NO FALLBACK to a stored CSV. If the cache is missing, the answer is to run
# 100, not to print last month's numbers.
.DESC_PATH <- file.path(.STUDY_ROOT, "data", "descriptive_exhibits.rds")

.desc <- function() .memo("desc", function() {
  if (!file.exists(.DESC_PATH))
    stop("exhibit_helpers_tables.R: missing the descriptive cache\n  ",
         .DESC_PATH,
         "\n  Run scripts/100_exhibit_descriptive_stats.R (the DESCRIPTIVE ",
         "stage of run_article.R).\n  It is slow -- roughly 1,600 model fits -- ",
         "which is why it is cached rather than\n  computed inside the knit.",
         call. = FALSE)
  d <- readRDS(.DESC_PATH)
  for (p in c("table1", "shares"))
    if (is.null(d[[p]]) || !nrow(d[[p]]))
      stop("exhibit_helpers_tables.R: the descriptive cache has no `", p,
           "`.\n  Re-run 100; a half-written cache is worse than none.",
           call. = FALSE)
  d
})

# Keyed lookup into a long frame. Zero rows -> NA (the cell prints "-", which is
# the draft's own convention for an unestimable cell). More than one row -> STOP:
# that means the key stopped being unique, and taking the first would print a
# real number under the wrong label.
.dpick <- function(d, value = "estimate", ...) {
  k <- list(...)
  ok <- rep(TRUE, nrow(d))
  for (n in names(k)) {
    if (!n %in% names(d))
      stop("exhibit_helpers_tables.R: column '", n, "' not in the descriptive ",
           "cache.\n  Columns present: ", paste(names(d), collapse = ", "),
           call. = FALSE)
    ok <- ok & !is.na(d[[n]]) & as.character(d[[n]]) == as.character(k[[n]])
  }
  v <- d[[value]][ok]
  if (length(v) > 1L)
    stop("exhibit_helpers_tables.R: ", length(v), " rows matched a descriptive ",
         "lookup that must be unique.\n  Keys: ",
         paste(sprintf("%s=%s", names(k), unlist(k)), collapse = ", "),
         call. = FALSE)
  if (!length(v)) NA_real_ else as.numeric(v[1])
}

# ---- The shared row spec for Tables 1, S3 and S4 -----------------------------
# One spec, three tables, because the draft prints the same variables in the
# same order in all three and a second copy would drift. `section` non-empty
# marks a header row. The two label columns are the draft's own wording, which
# differs between Table 1 and the appendix pair.
.ROWS_DESC <- rbind(
  data.frame(section = "Farmer", section_s = "Farmer a",
             label = "", label_s = "", outcome = "", crop = "",
             stringsAsFactors = FALSE),
  data.frame(section = "", section_s = "", stringsAsFactors = FALSE,
             label   = c("Female farmer (dummy)", "Age (years)", "Education (years)"),
             label_s = c("Female farmer (dummy)", "Age (years)", "Education (years)"),
             outcome = c("Female", "AgeYr", "YerEdu"),
             crop    = c("Pooled", "Pooled", "Pooled")),
  data.frame(section = "Selected crop production", section_s = "Cereal production a",
             label = "", label_s = "", outcome = "", crop = "",
             stringsAsFactors = FALSE),
  data.frame(section = "", section_s = "", stringsAsFactors = FALSE,
             label = c("All crops (real GH₵/ha)", "Maize (Kg/ha)", "Rice (Kg/ha)",
                       "Millet (Kg/ha)", "Sorghum (Kg/ha)", "Beans (Kg/ha)",
                       "Peanut (Kg/ha)", "Cassava (Kg/ha)", "Yam (Kg/ha)",
                       "Cocoyam (Kg/ha)", "Plantain (Kg/ha)", "Pepper (Kg/ha)",
                       "Okra (kg/ha)", "Tomato (kg/ha)", "Cocoa (Kg/ha)",
                       "Palm (Kg/ha)",
                       "Land (ha)", "Land owned (dummy)",
                       "Crop diversification (index)", "Seed (real GH₵/ha)",
                       "Household labor (AE)", "Hired labor (man-days/ha)",
                       "Fertilizer (Kg/ha)", "Pesticide (Liter/ha)",
                       "Mechanization (dummy)", "Irrigation (dummy)",
                       "Credit (dummy)"),
             label_s = c("All crops (maize kg/ha)", "Maize (Kg/ha)", "Rice (Kg/ha)",
                       "Millet (Kg/ha)", "Sorghum (Kg/ha)", "Beans (Kg/ha)",
                       "Peanut (Kg/ha)", "Cassava (Kg/ha)", "Yam (Kg/ha)",
                       "Cocoyam (Kg/ha)", "Plantain (Kg/ha)", "Pepper (Kg/ha)",
                       "Okra (kg/ha)", "Tomatoe (kg/ha)", "Cocoa (Kg/ha)",
                       "Palm (Kg/ha)",
                       "Land (ha)", "Land owned (dummy)",
                       "Crop diversification (index)", "Seed (GHC/ha)",
                       "Household labor (AE)", "Hired labor (man-days/ha)",
                       "Fertilizer (Kg/ha)", "Pesticide (Liter/ha)",
                       "Mechanization (dummy)", "Irrigation (dummy)",
                       "Credit (dummy)"),
             outcome = c(rep("Yield", 16),
                         "Area", "OwnLnd", "CrpMix", "SeedKg", "HHLaborAE",
                         "HirdHr", "FertKg", "PestLt", "EqipMech", "EqipIrig",
                         "Credit"),
             crop    = c("Pooled", "Maize", "Rice", "Millet", "Sorghum", "Beans",
                         "Peanut", "Cassava", "Yam", "Cocoyam", "Plantain",
                         "Pepper", "Okra", "Tomatoe", "Cocoa", "Palm",
                         rep("Pooled", 11))),
  data.frame(section = "Household", section_s = "Household b",
             label = "", label_s = "", outcome = "", crop = "",
             stringsAsFactors = FALSE),
  data.frame(section = "", section_s = "", stringsAsFactors = FALSE,
             label   = c("Size (AE)", "Dependency (ratio)"),
             label_s = c("Size (AE)", "Dependency (ratio)"),
             outcome = c("HHSizeAE", "Depend"),
             crop    = c("Pooled", "Pooled")))

# ==============================================================================
#  Table 1 -- Summary Statistics of Crop Producers in Ghana (2005-2017)
# ==============================================================================
# Treatment services0. Six value columns: mean (sd) and trend (%/yr) for the
# pooled sample, the no-services group and the some-services group.
#
# The dagger. In the draft it sits on the GROUP cells and marks the Wald test
# that the two groups differ: cat_diff (a test on the treatment main effect) for
# the mean columns, trend_diff (on the trend x treatment interaction) for the
# trend columns. Both come from the same fit as the cells they annotate, so a
# dagger cannot outlive the number it qualifies.
.T1_TREAT <- "services0"

.tbl1_live <- function() .memo("tbl:table1", function() {
  d <- .desc()$table1
  if (!.T1_TREAT %in% as.character(d$treatment))
    stop("exhibit_helpers_tables.R: the descriptive cache has no treatment '",
         .T1_TREAT, "'.\n  Treatments present: ",
         paste(sort(unique(as.character(d$treatment))), collapse = ", "),
         "\n  Re-run 100 with TREATMENTS covering it.", call. = FALSE)
  d <- d[as.character(d$treatment) == .T1_TREAT, , drop = FALSE]

  # Header n's, read from the data rather than transcribed. The draft prints
  # 22,519 / 8,304 / 14,215; if the analysis sample moves, the header moves with
  # it instead of quietly lying.
  n_of <- function(g) .dpick(d, value = "n", crop = "Pooled", outcome = "Female",
                             wave = "all", group = g, statistic = "mean")
  N <- c(pooled = n_of("pooled"), `0` = n_of("0"), `1` = n_of("1"))
  if (anyNA(N))
    stop("exhibit_helpers_tables.R: could not read Table 1's header counts. ",
         "The Female/Pooled mean row is missing from the cache.", call. = FALSE)

  rows <- lapply(seq_len(nrow(.ROWS_DESC)), function(i) {
    r <- .ROWS_DESC[i, ]
    if (nzchar(r$section))
      return(c(paste0("sec:", r$section), r$section, rep("", 6)))
    key <- list(crop = r$crop, outcome = r$outcome, wave = "all")
    m  <- function(g) .fmt_mean(do.call(.dpick, c(list(d), key, list(group = g, statistic = "mean"))),
                                do.call(.dpick, c(list(d, value = "sd"), key, list(group = g, statistic = "mean"))))
    tr <- function(g) .fmt_trend(do.call(.dpick, c(list(d), key, list(group = g, statistic = "trend_pct"))),
                                 do.call(.dpick, c(list(d, value = "se"), key, list(group = g, statistic = "trend_pct"))),
                                 do.call(.dpick, c(list(d, value = "p"),  key, list(group = g, statistic = "trend_pct"))))
    pcat <- do.call(.dpick, c(list(d, value = "p"), key, list(statistic = "cat_diff")))
    ptrd <- do.call(.dpick, c(list(d, value = "p"), key, list(statistic = "trend_diff")))
    dcat <- if (!is.na(pcat) && pcat < 0.10) paste0(" ", .DAG) else ""
    dtrd <- if (!is.na(ptrd) && ptrd < 0.10) paste0(" ", .DAG) else ""
    c(paste(r$crop, r$outcome, sep = "/"), r$label,
      m("pooled"), paste0(m("0"), dcat), paste0(m("1"), dcat),
      tr("pooled"), paste0(tr("0"), dtrd), paste0(tr("1"), dtrd))
  })

  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c(".key", "Variable", "mp", "m0", "m1", "tp", "t0", "t1")
  attr(m, "N") <- N
  .guard_blank(m, c("mp", "m0", "m1"), "ft_table1")
  m
})

# A table whose value columns are mostly "-" means a broken key, not missing
# data. Shared by every builder below.
.guard_blank <- function(m, cols, who, limit = 0.5) {
  body <- as.matrix(m[, cols, drop = FALSE])
  body <- body[!apply(body, 1, function(r) all(r %in% "")), , drop = FALSE]
  if (nrow(body) && mean(trimws(body) %in% c("", "-")) > limit)
    stop("exhibit_helpers_tables.R: ", who, "() produced a mostly blank table. ",
         "That means the\n  lookup keys are wrong, not that the data are ",
         "missing. Check the statistic/group\n  labels against the cache before ",
         "changing anything else.", call. = FALSE)
  invisible(TRUE)
}

.desc_footer <- function(extra = character(0))
  c(paste("Notes: Standard deviations in parentheses; clustered standard errors",
          "in brackets. Significance levels: * p<0.10, ** p<0.05, *** p<0.01."),
    paste("a Trend is the average semi-elasticity from a regression of the",
          "variable on a linear year trend interacted with service status,",
          "expressed as percent per year. Standard errors are clustered by",
          "survey-ecozone-enumeration area-household."),
    paste0(.DAG, " marks a group difference significant at the 10 percent level",
           " (Wald test on the service-status term for means, on the",
           " trend-by-service interaction for trends)."),
    extra)

ft_table1 <- function() {
  m <- .tbl1_live()
  N <- attr(m, "N")
  hdr <- grep("^sec:", m$.key)
  out <- m[, setdiff(names(m), ".key"), drop = FALSE]
  ft <- flextable(out)
  ft <- set_header_labels(
    ft, Variable = "Variable",
    mp = sprintf("Pooled\n(n=%s)", formatC(as.integer(N[["pooled"]]), format = "d", big.mark = ",")),
    m0 = sprintf("No services\n(n=%s)", formatC(as.integer(N[["0"]]), format = "d", big.mark = ",")),
    m1 = sprintf("Some services\n(n=%s)", formatC(as.integer(N[["1"]]), format = "d", big.mark = ",")),
    tp = sprintf("Pooled\n(n=%s)", formatC(as.integer(N[["pooled"]]), format = "d", big.mark = ",")),
    t0 = sprintf("No services\n(n=%s)", formatC(as.integer(N[["0"]]), format = "d", big.mark = ",")),
    t1 = sprintf("Some services\n(n=%s)", formatC(as.integer(N[["1"]]), format = "d", big.mark = ",")))
  ft <- add_header_row(ft, values = c("", "Mean (SD)", "Trend (%) a"),
                       colwidths = c(1, 3, 3), top = TRUE)
  ft <- align(ft, j = 2:7, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- italic(ft, i = hdr, j = 1, italic = TRUE)
  ft <- merge_h(ft, i = hdr, part = "body")
  ft <- padding(ft, i = setdiff(seq_len(nrow(out)), hdr), j = 1, padding.left = 12)
  ft <- fontsize(ft, size = 8, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = .desc_footer())
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ==============================================================================
#  Tables S3 and S4 -- summary statistics and trends by SOURCE of service
# ==============================================================================
# Four columns: the pooled sample, then the treated group of each of three
# treatments. S3 prints the mean cells, S4 the trend cells; the rows are Table
# 1's.
#
# THE POOLED COLUMN COMES FROM services0, deliberately. services1/2/3 are coded
# 0 = no services at all, 1 = this source, NA = served by another source, and
# descriptive_group_summary() drops missing-treatment rows before it computes
# anything -- so the services3 grid's own pooled row sits on n = 20,056, not
# 22,519. Reading the header's 22,519 off that grid would print one sample under
# another's label.
.SRC_COLS <- list(
  list(id = "pooled", treatment = "services0",             group = "pooled", label = "Pooled"),
  list(id = "ext",    treatment = "services3",             group = "1",      label = "Extension"),
  list(id = "coop",   treatment = "community_cooperative", group = "1",      label = "Community cooperative"),
  list(id = "assoc",  treatment = "farm_association",      group = "1",      label = "Farm association"))

.tblS34_live <- function(kind) {
  d0 <- .desc()$table1
  miss <- setdiff(vapply(.SRC_COLS, function(c) c$treatment, ""),
                  unique(as.character(d0$treatment)))
  if (length(miss))
    stop("exhibit_helpers_tables.R: the descriptive cache is missing treatment(s) ",
         paste(miss, collapse = ", "),
         ".\n  Tables S3/S4 need all of services0, services3, ",
         "community_cooperative and farm_association.\n  Re-run 100 with ",
         "TREATMENTS covering them.", call. = FALSE)

  n_of <- function(cc) .dpick(d0[as.character(d0$treatment) == cc$treatment, ],
                              value = "n", crop = "Pooled", outcome = "Female",
                              wave = "all", group = cc$group, statistic = "mean")
  N <- vapply(.SRC_COLS, n_of, numeric(1))
  names(N) <- vapply(.SRC_COLS, function(c) c$id, "")

  rows <- lapply(seq_len(nrow(.ROWS_DESC)), function(i) {
    r <- .ROWS_DESC[i, ]
    if (nzchar(r$section_s))
      return(c(paste0("sec:", r$section_s), r$section_s, rep("", length(.SRC_COLS))))
    cells <- vapply(.SRC_COLS, function(cc) {
      dd  <- d0[as.character(d0$treatment) == cc$treatment, , drop = FALSE]
      key <- list(crop = r$crop, outcome = r$outcome, wave = "all")
      # The dagger is only meaningful on a group column; the pooled column has
      # no group to differ from.
      dag <- ""
      if (!identical(cc$group, "pooled")) {
        pw <- do.call(.dpick, c(list(dd, value = "p"), key,
                                list(statistic = if (kind == "mean") "cat_diff" else "trend_diff")))
        if (!is.na(pw) && pw < 0.10) dag <- paste0(" ", .DAG)
      }
      v <- if (kind == "mean")
        .fmt_mean(do.call(.dpick, c(list(dd), key, list(group = cc$group, statistic = "mean"))),
                  do.call(.dpick, c(list(dd, value = "sd"), key, list(group = cc$group, statistic = "mean"))))
      else
        .fmt_trend(do.call(.dpick, c(list(dd), key, list(group = cc$group, statistic = "trend_pct"))),
                   do.call(.dpick, c(list(dd, value = "se"), key, list(group = cc$group, statistic = "trend_pct"))),
                   do.call(.dpick, c(list(dd, value = "p"),  key, list(group = cc$group, statistic = "trend_pct"))))
      if (identical(v, "-")) v else paste0(v, dag)
    }, character(1))
    c(paste(r$crop, r$outcome, sep = "/"), r$label_s, cells)
  })

  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c(".key", "Variable", vapply(.SRC_COLS, function(c) c$id, ""))
  attr(m, "N") <- N
  .guard_blank(m, vapply(.SRC_COLS, function(c) c$id, ""),
               if (kind == "mean") "ft_tableS3" else "ft_tableS4")
  m
}

.tblS3_live <- function() .memo("tbl:tableS3", function() .tblS34_live("mean"))
.tblS4_live <- function() .memo("tbl:tableS4", function() .tblS34_live("trend"))

.ft_S34 <- function(m, extra_note) {
  N   <- attr(m, "N")
  hdr <- grep("^sec:", m$.key)
  out <- m[, setdiff(names(m), ".key"), drop = FALSE]
  labs <- c(list(Variable = "Variable"),
            stats::setNames(lapply(.SRC_COLS, function(cc)
              sprintf("%s\n(n=%s)", cc$label,
                      formatC(as.integer(N[[cc$id]]), format = "d", big.mark = ","))),
              vapply(.SRC_COLS, function(c) c$id, "")))
  ft <- flextable(out)
  ft <- do.call(set_header_labels, c(list(ft), labs))
  ft <- align(ft, j = 2:ncol(out), align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- italic(ft, i = hdr, j = 1, italic = TRUE)
  ft <- merge_h(ft, i = hdr, part = "body")
  ft <- padding(ft, i = setdiff(seq_len(nrow(out)), hdr), j = 1, padding.left = 12)
  ft <- fontsize(ft, size = 8, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = .desc_footer(extra_note))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

ft_tableS3 <- function()
  .ft_S34(.tblS3_live(),
          paste("b The pooled column is the full analysis sample. The three",
                "source columns are the farmers served by that source; because a",
                "farmer served by another source is excluded from a source's",
                "comparison, the source columns do not sum to the pooled column."))

ft_tableS4 <- function()
  .ft_S34(.tblS4_live(),
          paste("b Cells are percent change per year. See Table S3 for the",
                "levels the trends are computed on."))

# ==============================================================================
#  Tables 2, 3, 4 and 5 -- Engine B, community service headcounts by crop
# ==============================================================================
# One chassis, four tables. Rows are crops; columns are indicators; the top block
# is the pooled share (mean (sd)) and the bottom block the semi-elasticity trend
# (percent per year, se in brackets).
#
# CROP ORDER is the v000 draft's, recovered from the .docx and hardcoded. It is
# not derivable from the data -- the draft sorts each block by its own first
# column, so its two blocks are in DIFFERENT orders. One order is used here for
# both, so a row can be read across the whole table. Diffing against the draft
# means matching on the crop label, not on the row number.
.CROPS_B <- c("Millet", "Sorghum", "Rice", "Okra", "Maize", "Beans", "Pooled",
              "Peanut", "Cocoa", "Cassava", "Banana", "Plantain", "Pepper",
              "Yam", "Cocoyam", "Tomatoe", "Eggplant", "Palm")
.CROP_LABEL <- function(x) ifelse(x == "Pooled", "All crops listed",
                                  ifelse(x == "Tomatoe", "Tomato", x))

.tblB_live <- function(inds, digits, who) {
  s <- .desc()$shares
  miss <- setdiff(inds, unique(as.character(s$outcome)))
  if (length(miss))
    stop("exhibit_helpers_tables.R: ", who, "() needs indicator(s) ",
         paste(miss, collapse = ", "), " which the descriptive cache does not ",
         "carry.\n  Indicators present: ",
         paste(sort(unique(as.character(s$outcome))), collapse = ", "),
         "\n  These are built by descriptive_expand_category() in 100; if a ",
         "level count changed\n  upstream the dummy NUMBERING changed with it. ",
         "Re-pin before editing.", call. = FALSE)

  crops <- intersect(.CROPS_B, unique(as.character(s$crop)))
  gone  <- setdiff(.CROPS_B, crops)
  blk <- function(kind) do.call(rbind, lapply(crops, function(cr) {
    sc <- s[as.character(s$crop) == cr, , drop = FALSE]
    cells <- vapply(inds, function(v) {
      if (kind == "share")
        .fmt_mean(.dpick(sc, outcome = v, wave = "pooled", statistic = "mean"),
                  .dpick(sc, value = "sd", outcome = v, wave = "pooled", statistic = "mean"),
                  digits = digits)
      else
        .fmt_trend(.dpick(sc, outcome = v, wave = "trend", statistic = "trend_pct"),
                   .dpick(sc, value = "se", outcome = v, wave = "trend", statistic = "trend_pct"),
                   NA_real_, digits = digits)
    }, character(1))
    c(paste(kind, cr, sep = "/"), .CROP_LABEL(cr), cells)
  }))

  m <- rbind(
    c("sec:share", "Headcount ratio over the study period", rep("", length(inds))),
    blk("share"),
    c("sec:trend", "Percentage change in the headcount ratio per year",
      rep("", length(inds))),
    blk("trend"))
  m <- as.data.frame(m, stringsAsFactors = FALSE)
  names(m) <- c(".key", "Crop", paste0("v", seq_along(inds)))
  attr(m, "dropped") <- gone
  .guard_blank(m, paste0("v", seq_along(inds)), who)
  m
}

# The trend cells carry no stars: descriptive_indicator_shares() returns the
# semi-elasticity and its se but not a p-value, and inventing one from
# estimate/se would assume a normal reference distribution the engine never
# claimed. The draft prints them starless too.
.ft_B <- function(m, labels, title_note) {
  hdr  <- grep("^sec:", m$.key)
  out  <- m[, setdiff(names(m), ".key"), drop = FALSE]
  gone <- attr(m, "dropped")
  ft <- flextable(out)
  ft <- do.call(set_header_labels,
                c(list(ft), list(Crop = "Crop"),
                  stats::setNames(as.list(labels), paste0("v", seq_along(labels)))))
  ft <- align(ft, j = 2:ncol(out), align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- italic(ft, i = hdr, j = 1, italic = TRUE)
  ft <- merge_h(ft, i = hdr, part = "body")
  ft <- padding(ft, i = setdiff(seq_len(nrow(out)), hdr), j = 1, padding.left = 12)
  ft <- fontsize(ft, size = 8, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = c(
    title_note,
    paste("Shares are unweighted headcount ratios over GLSS5-GLSS7 (2005/06,",
          "2012/13 and 2016/17); standard deviations in parentheses. Trends are",
          "average semi-elasticities from a logit on a linear year trend,",
          "expressed as percent per year, with clustered standard errors in",
          "brackets. \"-\" marks a cell the data cannot estimate."),
    if (length(gone))
      paste0("Crops named by the v000 draft but absent from this build: ",
             paste(gone, collapse = ", "), ".") else NULL))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ---- Table 2: agencies providing extension ------------------------------------
.T2_IND <- c("extension_agency_mofa", "extension_agency_ngo", "extension_agency_coop")
.T2_LAB <- c("Ministry of Food\nand Agriculture", "Non-Profit\nOrganization",
             "Agricultural\nCooperatives")
.tbl2_live <- function() .memo("tbl:table2", function() .tblB_live(.T2_IND, 3, "ft_table2"))
ft_table2 <- function()
  .ft_B(.tbl2_live(), .T2_LAB,
        paste("Notes: Agency providing agricultural extension in the",
              "respondent's community. The three agencies are not mutually",
              "exclusive."))

# ---- Table 3: patterns in extension access ------------------------------------
# `extension` is an ordinal 0..k. descriptive_expand_category() names the dummies
# extension_1..extension_(k+1) in LEVEL order, so extension_2..extension_7 are
# levels 1..6 -- the six access patterns the draft prints. Level 0 ("no
# agricultural extension") is the omitted reference and is not a pattern.
#
# The release now carries an EIGHTH level (7) with no value label, where the
# .do's `tab extension, gen(extensionCat)` produced seven dummies. Levels 1-6 are
# unaffected, so the printed columns are the draft's; the unlabelled level is
# reported by 100 rather than absorbed here.
.T3_IND <- paste0("extension_", 2:7)
.T3_LAB <- c("Extension\nofficer\naccess only", "Extension\noffice\naccess only",
             "Extension\nvisits\nonly", "Extension\nvisit plus\nofficer access",
             "Extension\nvisit plus\noffice access",
             "Extension office\nand officer access,\nplus visit")
.tbl3_live <- function() .memo("tbl:table3", function() .tblB_live(.T3_IND, 2, "ft_table3"))
ft_table3 <- function()
  .ft_B(.tbl3_live(), .T3_LAB,
        paste("Notes: Mutually exclusive patterns of extension access. The",
              "omitted category is no agricultural extension of any kind."))

# ---- Table 4: access to community agricultural services -----------------------
# Column order is the draft's. Two of the ten harmonized service items --
# husbandry and employment -- are not printed by the draft; they are named in the
# footnote rather than dropped in silence.
.T4_IND <- c("services_planting", "services_agchemicals", "services_labour",
             "services_irrigation", "services_post_harvest", "services_credit",
             "services_mechanization", "services_records")
.T4_LAB <- c("Planting", "Use of agro\nchemicals", "Communal labor", "Irrigation",
             "Post harvest/\nmarketing", "Credit facilities", "Mechanization",
             "Records/\nBook-keeping")
.tbl4_live <- function() .memo("tbl:table4", function() .tblB_live(.T4_IND, 2, "ft_table4"))
ft_table4 <- function()
  .ft_B(.tbl4_live(), .T4_LAB,
        paste("Notes: Community-level agricultural services. Two further",
              "harmonized items -- animal husbandry and employment services --",
              "are collected but are not printed here, following the v000",
              "draft."))

# ---- Table 5: compliance with advisory recommendations ------------------------
# extension_compliance is 0..3 (None / Did not comply / Partially / Fully), so
# the dummies are extension_compliance_1..4 and the draft's three columns are
# levels 1..3.
.T5_IND <- paste0("extension_compliance_", 2:4)
.T5_LAB <- c("Did not comply", "Partially complied", "Fully complied")
.tbl5_live <- function() .memo("tbl:table5", function() .tblB_live(.T5_IND, 2, "ft_table5"))
ft_table5 <- function()
  .ft_B(.tbl5_live(), .T5_LAB,
        paste("Notes: Compliance with the advice received, among farmers",
              "reporting any advisory contact. The omitted category is no",
              "advisory contact."))

# ==============================================================================
#  Table 6 -- input elasticities, efficiency and model diagnostics
# ==============================================================================
# services0 only. Six columns: the naive national frontier, the two group
# frontiers and their difference, and the meta-frontier on the matched and
# unmatched samples.
#
# WHICH SAMPLE EACH COLUMN USES, and why it is not uniform:
#   naive / none / any   the UNMATCHED sample -- these are frontier PARAMETERS,
#                        estimated on the full data
#   difference           the STORED elasticityGap_lvl on the MATCHED sample,
#                        which carries its own jackknife SE. Not any - none.
#   meta matched         the matched sample; meta unmatched, the unmatched one
# The efficiency block prints its Matched and Unmatched rows explicitly, because
# there the sample IS the comparison.
#
# el5 IS FERTILIZER and el6 IS PESTICIDE. See note 2 at the top of this block:
# the v000 draft has these two rows transposed.
.EL_ROWS <- list(
  list(id = "el1", label = "Land"),
  list(id = "el2", label = "Planting material"),
  list(id = "el3", label = "Family labor"),
  list(id = "el4", label = "Hired labor"),
  list(id = "el5", label = "Fertilizer"),
  list(id = "el6", label = "Pesticide"),
  list(id = "el7", label = "Returns to scale"))

.T6_METRICS <- list(
  list(type = "TGR", label = "Technology gap ratio (TGR)"),
  list(type = "TE",  label = "Pure farmer technical efficiency (TE)"),
  list(type = "MTE", label = "Meta-frontier technical efficiency (MTE)"))

# CoefName -> printed label, for the diagnostics block. Each is a single sf_estm
# row per (TCHLvel, sample).
.T6_DIAG <- list(
  list(coef = "Nobs",         label = "Sample size",                       fmt = "plain", digits = 0),
  list(coef = "mono",         label = "Monotonicity satisfaction rate",    fmt = "pct"),
  list(coef = "curv",         label = "Curvature satisfaction rate",       fmt = "pct"),
  list(coef = "olsSkew",      label = "Schmidt & Lin (1984) a",            fmt = "stars"),
  list(coef = "CoelliM3Test", label = "Coelli (1995) a",                   fmt = "stars"),
  list(coef = "LRInef",       label = "Gutierrez (2001) a",                fmt = "stars"),
  list(coef = "mlLoglik",     label = "Log likelihood",                    fmt = "plain", digits = 0),
  list(coef = "nParam",       label = "No. of parameters",                 fmt = "plain", digits = 0),
  list(coef = "LRT",          label = "Meta frontier LR test",             fmt = "stars"),
  list(coef = "Gamma",        label = "Ratio variance due to inefficiency", fmt = "coef"))

.tbl6_live <- function() .memo("tbl:table6", function() {
  p   <- .read_est6("services0")
  el  <- p$el_mean
  ef  <- p$ef_mean
  sf  <- p$sf_estm
  opt <- .samp_id("matched")

  elx <- el[el$stat %in% "wmean" & el$Survey %in% "GLSS0" &
            el$restrict %in% "Restricted" & el$CoefName %in% "elasticity", ]
  elg <- el[el$stat %in% "wmean" & el$Survey %in% "GLSS0" &
            el$restrict %in% "Restricted" &
            el$CoefName %in% "elasticityGap_lvl", ]
  .check_level_labels(elx, "el_mean")

  ecell <- function(d, id, lvl, samp)
    .fmt_coef(.pick2(d, list(input = id, TCHLvel = lvl, sample = samp), "Estimate"),
              .pick2(d, list(input = id, TCHLvel = lvl, sample = samp), "Estimate.sd"),
              .pick2(d, list(input = id, TCHLvel = lvl, sample = samp), "jack_pv"))

  rows <- list(c("sec:el", "Elasticity", rep("", 6)))
  for (r in .EL_ROWS)
    rows[[length(rows) + 1L]] <- c(
      paste0("el/", r$id), r$label,
      ecell(elx, r$id, "National", "unmatched"),
      ecell(elx, r$id, "0",        "unmatched"),
      ecell(elx, r$id, "1",        "unmatched"),
      ecell(elg, r$id, "1",        opt),
      ecell(elx, r$id, "Meta",     opt),
      ecell(elx, r$id, "Meta",     "unmatched"))

  # ---- efficiency block ------------------------------------------------------
  # THE NAIVE COLUMN IS NOT ONE LOOKUP. Pinned 2026-08-13 against the v000
  # draft, whose unmatched cells reproduce exactly:
  #   TE  block -> type "TE0" at TCHLvel "National"  (0.598 unmatched)
  #   MTE block -> type "MTE" at TCHLvel "National"  (0.555 unmatched)
  #   TGR block -> blank in the draft
  # TE0 is the single-frontier score -- the efficiency you get ignoring the
  # technology split -- so it is the naive TE. The naive meta-score has no TE0
  # analogue and comes from MTE at the national level. Using TE0 for both, which
  # is the obvious-looking simplification, puts 0.586 where 0.528 belongs and
  # the cell still looks entirely reasonable.
  # TGR at National DOES exist in the object (0.895 matched / 0.913 unmatched);
  # the draft omits it and that omission is preserved here and stated in the
  # footer rather than quietly filled in.
  efx <- ef[ef$estType %in% "teBC" & ef$stat %in% "wmean" &
            ef$Survey %in% "GLSS0" & ef$restrict %in% "Restricted", ]
  .check_level_labels(efx, "ef_mean")
  fcell <- function(type, lvl, coef, samp)
    .fmt_coef(.pick2(efx, list(type = type, TCHLvel = lvl, CoefName = coef, sample = samp), "Estimate"),
              .pick2(efx, list(type = type, TCHLvel = lvl, CoefName = coef, sample = samp), "Estimate.sd"),
              .pick2(efx, list(type = type, TCHLvel = lvl, CoefName = coef, sample = samp), "jack_pv"))

  rows[[length(rows) + 1L]] <- c("sec:te", "Technology/efficiency", rep("", 6))
  for (b in .T6_METRICS) {
    rows[[length(rows) + 1L]] <- c(paste0("te/", b$type), b$label, rep("", 6))
    for (sm in list(list(id = opt, label = "Matched"),
                    list(id = "unmatched", label = "Unmatched")))
      rows[[length(rows) + 1L]] <- c(
        paste0("te/", b$type, "/", sm$label), sm$label,
        switch(b$type,
               TGR = "-",
               TE  = fcell("TE0",   "National", "efficiency", sm$id),
               MTE = fcell("MTE",   "National", "efficiency", sm$id)),
        fcell(b$type, "0", "efficiency", sm$id),
        fcell(b$type, "1", "efficiency", sm$id),
        fcell(b$type, "1", "efficiencyGap_lvl", sm$id),
        "-", "-")
  }

  # ---- diagnostics block -----------------------------------------------------
  sfx <- sf[sf$restrict %in% "Restricted", ]
  .check_level_labels(sfx, "sf_estm")
  # Estimate.sd and jack_pv, NOT StdError/Pvalue. sf_estm reports jackknife
  # moments like every other summary in this object: StdError is NA on 64% of
  # rows and Pvalue returns an exact 1.000 on the naive frontier. Reading them
  # would print a coefficient table with no dispersion and no stars on one whole
  # column -- which renders, and is wrong. Checked against the draft: Gamma
  # naive is 0.604*** (0.001), and Estimate.sd is what gives 0.001.
  dcell <- function(spec, lvl, samp) {
    e  <- .pick2(sfx, list(CoefName = spec$coef, TCHLvel = lvl, sample = samp), "Estimate")
    se <- .pick2(sfx, list(CoefName = spec$coef, TCHLvel = lvl, sample = samp), "Estimate.sd")
    pv <- .pick2(sfx, list(CoefName = spec$coef, TCHLvel = lvl, sample = samp), "jack_pv")
    switch(spec$fmt,
           plain = .fmt_plain(e, spec$digits %||% 0),
           pct   = .fmt_plain(100 * e, 2),
           stars = .fmt_coef(e, NA_real_, pv),
           .fmt_coef(e, se, pv))
  }
  # nParam is not an sf_estm row. It is COUNTED: the number of estimated
  # coefficients reported for this frontier, which is every CoefName in the cell
  # that is not a fit statistic.
  #
  # DO NOT use nXvar + nuZUvar + nvZVvar, which is what the draft's "35" is.
  # nXvar does not count the frontier's coefficients: on the translog fit it is
  # 20 against 46 frontier coefficients actually reported, and on the
  # Cobb-Douglas fit it is 25 -- LARGER, though Cobb-Douglas is the nested,
  # smaller model. Differencing the two gives a NEGATIVE degrees of freedom,
  # which is how the defect was found. Counting the rows gives 61 against 40,
  # a difference of exactly the 21 second-order translog terms.
  nparam <- function(lvl, samp) {
    r <- sfx[!is.na(sfx$TCHLvel) & as.character(sfx$TCHLvel) == lvl &
             as.character(sfx$sample) == samp, , drop = FALSE]
    if (!nrow(r)) return(NA_real_)
    length(setdiff(unique(as.character(r$CoefName)), .SF_STATS))
  }
  dcell0 <- dcell
  dcell <- function(spec, lvl, samp)
    if (identical(spec$coef, "nParam")) .fmt_plain(nparam(lvl, samp), 0)
    else dcell0(spec, lvl, samp)

  rows[[length(rows) + 1L]] <- c("sec:diag", "Model diagnostics", rep("", 6))
  for (spec in .T6_DIAG)
    rows[[length(rows) + 1L]] <- c(
      paste0("diag/", spec$coef), spec$label,
      dcell(spec, "National", "unmatched"),
      dcell(spec, "0",        "unmatched"),
      dcell(spec, "1",        "unmatched"),
      "-",
      dcell(spec, "Meta",     opt),
      dcell(spec, "Meta",     "unmatched"))

  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c(".key", "Variable", "naive", "none", "any", "diff",
                "meta_m", "meta_u")
  .guard_blank(m, c("naive", "none", "any", "meta_m", "meta_u"), "ft_table6")
  m
})

ft_table6 <- function() {
  m   <- .tbl6_live()
  hdr <- grep("^sec:", m$.key)
  sub <- grep("^te/(TGR|TE|MTE)$", m$.key)
  out <- m[, setdiff(names(m), ".key"), drop = FALSE]
  ft <- flextable(out)
  ft <- set_header_labels(ft, Variable = "",
                          naive = "Naïve national\nfrontier",
                          none = "No services\n[A]", any = "Some services\n[B]",
                          diff = "Difference\n[B - A]",
                          meta_m = "Matched", meta_u = "Unmatched")
  ft <- add_header_row(ft, values = c("", "", "Group frontier", "Meta-frontier"),
                       colwidths = c(1, 1, 3, 2), top = TRUE)
  ft <- align(ft, j = 2:7, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- italic(ft, i = c(hdr, sub), j = 1, italic = TRUE)
  ft <- merge_h(ft, i = c(hdr, sub), part = "body")
  ft <- padding(ft, i = setdiff(seq_len(nrow(out)), c(hdr, sub)), j = 1,
                padding.left = 14)
  ft <- fontsize(ft, size = 8, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = c(
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01.",
    paste("Elasticities are evaluated at each observation and averaged with",
          "sampling weights; the difference column is the stored matched-sample",
          "gap, which carries its own jackknife standard error, not the",
          "arithmetic difference of the two unmatched columns."),
    paste("a Tests of the sign of the residual skewness (Schmidt and Lin),",
          "the third-moment statistic (Coelli) and the likelihood ratio for the",
          "presence of inefficiency (Gutierrez)."),
    paste("The naive column reports the single-frontier score: TE0 for the",
          "technical-efficiency block and the national-level meta-score for the",
          "meta-frontier block. The naive technology gap ratio is omitted,",
          "following the v000 draft; it is carried in output/estimations as",
          "type = TGR at TCHLvel = National."),
    paste("Standard errors were estimated via the jackknife resampling method",
          "by iteratively generating 100 resampled datasets by randomly",
          "excluding one enumeration area from each survey for every resample.")))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ==============================================================================
#  Tables S7 and S8 -- the frontier and inefficiency coefficients
# ==============================================================================
# Both read sf_estm, five columns: naive, the two group frontiers, and the
# meta-frontier matched and unmatched. Cells are Estimate stars (StdError).
#
# ROWS ARE ENUMERATED FROM THE OBJECT, not hand-listed, then ordered by an
# explicit spec. Anything the object carries that no block claims is appended
# under "Other terms" rather than dropped -- a coefficient table that silently
# omits a coefficient is the failure this design exists to prevent.
.S7_LABELS <- c(
  lnI1 = "Land [lnI1]", lnI2 = "Planting material [lnI2]",
  lnI3 = "Family labor [lnI3]", lnI4 = "Hired labor [lnI4]",
  lnI5 = "Fertilizer [lnI5]", lnI6 = "Pesticide [lnI6]")

.ECO_LABELS <- c("Forest Zone" = "Forest", "Guinea Savanah" = "Guinea Savanah",
                 "Sudan Savanah" = "Sudan Savanah",
                 "Transitional Zone" = "Transitional")
.SURVEY_LABELS <- c(GLSS6 = "2012/13", GLSS7 = "2016/17")

# Everything that is a fit statistic rather than a coefficient. Excluded from the
# coefficient tables by name, so a new statistic appears as an unclaimed
# coefficient (loud) rather than silently joining the table.
.SF_STATS <- c("AIC", "BIC", "HQIC", "Nobs", "mlLoglik", "nXvar", "nuZUvar",
               "nvZVvar", "mono", "curv", "olsSkew", "olsM3Okay", "CoelliM3Test",
               "AgostinoSkw", "AgostinoKrt", "AgostinoOmn", "LRT", "LRInef",
               "Gamma", "Sigma", "Varu", "Eu", "Expu", "sigmauSq", "sigmavSq")

.sf_grid <- function(specs, who) {
  p   <- .read_est6("services0")
  sfx <- p$sf_estm[p$sf_estm$restrict %in% "Restricted", ]
  .check_level_labels(sfx, "sf_estm")
  opt <- .samp_id("matched")
  # Estimate.sd / jack_pv, not StdError / Pvalue -- see the KEYING block at the
  # top of this file, and the note in .tbl6_live(). StdError is NA on most rows.
  cell <- function(coef, lvl, samp)
    .fmt_coef(.pick2(sfx, list(CoefName = coef, TCHLvel = lvl, sample = samp), "Estimate"),
              .pick2(sfx, list(CoefName = coef, TCHLvel = lvl, sample = samp), "Estimate.sd"),
              .pick2(sfx, list(CoefName = coef, TCHLvel = lvl, sample = samp), "jack_pv"))
  rows <- list()
  for (s in specs) {
    if (!is.null(s$section)) {
      rows[[length(rows) + 1L]] <- c(paste0("sec:", s$section), s$section, rep("", 5))
      next
    }
    rows[[length(rows) + 1L]] <- c(
      paste0(who, "/", s$coef), s$label,
      cell(s$coef, "National", "unmatched"),
      cell(s$coef, "0",        "unmatched"),
      cell(s$coef, "1",        "unmatched"),
      cell(s$coef, "Meta",     opt),
      cell(s$coef, "Meta",     "unmatched"))
  }
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c(".key", "Variable", "naive", "none", "any", "meta_m", "meta_u")
  .guard_blank(m, c("naive", "none", "any", "meta_m", "meta_u"), who)
  m
}

.tblS7_live <- function() .memo("tbl:tableS7", function() {
  p    <- .read_est6("services0")
  have <- unique(as.character(p$sf_estm$CoefName))
  claimed <- character(0)
  specs   <- list()
  add <- function(coef, label) {
    if (!coef %in% have) return(invisible(FALSE))
    claimed <<- c(claimed, coef)
    specs[[length(specs) + 1L]] <<- list(coef = coef, label = label)
    invisible(TRUE)
  }
  sec <- function(nm) specs[[length(specs) + 1L]] <<- list(section = nm)

  sec("Production function")
  for (i in 1:6) add(paste0("lnI", i), unname(.S7_LABELS[[paste0("lnI", i)]]))
  for (i in 1:6) {
    add(sprintf("I(1/2 * lnI%d * lnI%d)", i, i), sprintf("1/2 * lnI%d * lnI%d", i, i))
    for (j in seq_len(6)[-seq_len(i)])
      add(sprintf("lnI%d:lnI%d", i, j), sprintf("lnI%d*lnI%d", i, j))
  }
  # Crop area shares: enumerated, not listed, so a crop added upstream appears.
  areas <- sort(grep("^Area_", have, value = TRUE))
  if (length(areas)) {
    sec("Proportion of area under listed crop")
    for (a in areas) add(a, sub("^Area_", "", a))
  }
  ecos <- grep("^factor\\(Ecozon\\)", have, value = TRUE)
  if (length(ecos)) {
    sec("Ecological zone [base = Coastal Savanna]")
    for (e in ecos) {
      lv <- sub("^factor\\(Ecozon\\)", "", e)
      add(e, if (lv %in% names(.ECO_LABELS)) unname(.ECO_LABELS[[lv]]) else lv)
    }
  }
  svy <- grep("^factor\\(Survey\\)", have, value = TRUE)
  if (length(svy)) {
    sec("Period [base = 2005/06]")
    for (s in svy) {
      lv <- sub("^factor\\(Survey\\)", "", s)
      add(s, if (lv %in% names(.SURVEY_LABELS)) unname(.SURVEY_LABELS[[lv]]) else lv)
    }
  }
  add("(Intercept)", "Intercept")
  sec("Production risk function")
  add("Zv_(Intercept)", "Intercept")

  # Anything left that is neither a claimed coefficient, an inefficiency term
  # (Table S8's) nor a fit statistic.
  leftover <- setdiff(have, c(claimed, .SF_STATS, grep("^Zu_", have, value = TRUE)))
  if (length(leftover)) {
    sec("Other terms")
    for (co in leftover) add(co, co)
  }
  m <- .sf_grid(specs, "tableS7")
  attr(m, "leftover") <- leftover
  m
})

ft_tableS7 <- function() .ft_sf(.tblS7_live(), c(
  paste("Meta stochastic frontier analysis of a translog production function in",
        "six inputs, jointly estimated on Ghana Living Standards Survey waves",
        "5-7."),
  paste("The naive national frontier pools both groups; the group frontiers are",
        "estimated separately for communities with and without agricultural",
        "services.")))

.tblS8_live <- function() .memo("tbl:tableS8", function() {
  p    <- .read_est6("services0")
  have <- unique(as.character(p$sf_estm$CoefName))
  zu   <- grep("^Zu_", have, value = TRUE)
  if (!length(zu))
    stop("exhibit_helpers_tables.R: sf_estm carries no Zu_* inefficiency ",
         "coefficients.\n  Table S8 has no content without them.", call. = FALSE)

  # The named block, in the draft's order; anything else Zu_ carries follows.
  named <- c("Zu_factor(Female)1"   = "Female farmer (dummy)",
             "Zu_lnAgeYr"           = "Age (years)",
             "Zu_lnYerEdu"          = "Education (years)",
             "Zu_factor(OwnLnd)1"   = "Land owned (dummy)",
             "Zu_CrpMix"            = "Crop diversification (index)",
             "Zu_factor(EqipMech)1" = "Mechanization (dummy)",
             "Zu_factor(Credit)1"   = "Credit (dummy)")
  specs <- list(); claimed <- character(0)
  add <- function(coef, label) {
    if (!coef %in% zu) return(invisible(FALSE))
    claimed <<- c(claimed, coef)
    specs[[length(specs) + 1L]] <<- list(coef = coef, label = label)
  }
  sec <- function(nm) specs[[length(specs) + 1L]] <<- list(section = nm)

  for (co in names(named)) add(co, unname(named[[co]]))
  ecos <- grep("^Zu_factor\\(Ecozon\\)", zu, value = TRUE)
  if (length(ecos)) {
    sec("Ecological zone [base = Coastal Savanna]")
    for (e in ecos) {
      lv <- sub("^Zu_factor\\(Ecozon\\)", "", e)
      add(e, if (lv %in% names(.ECO_LABELS)) unname(.ECO_LABELS[[lv]]) else lv)
    }
  }
  svy <- grep("^Zu_factor\\(Survey\\)", zu, value = TRUE)
  if (length(svy)) {
    sec("Period [base = 2005/06]")
    for (s in svy) {
      lv <- sub("^Zu_factor\\(Survey\\)", "", s)
      add(s, if (lv %in% names(.SURVEY_LABELS)) unname(.SURVEY_LABELS[[lv]]) else lv)
    }
  }
  leftover <- setdiff(zu, c(claimed, "Zu_(Intercept)"))
  if (length(leftover)) {
    sec("Other inefficiency covariates")
    for (co in leftover) add(co, sub("^Zu_", "", co))
  }
  add("Zu_(Intercept)", "Intercept")
  m <- .sf_grid(specs, "tableS8")
  attr(m, "leftover") <- leftover
  m
})

ft_tableS8 <- function() .ft_sf(.tblS8_live(), c(
  paste("Determinants of technical inefficiency. A POSITIVE coefficient raises",
        "inefficiency, and therefore lowers technical efficiency."),
  paste("Age and education enter in logs, as specified in",
        "004_MSF_ag_services_study.R.")))

.ft_sf <- function(m, notes) {
  hdr <- grep("^sec:", m$.key)
  out <- m[, setdiff(names(m), ".key"), drop = FALSE]
  ft <- flextable(out)
  ft <- set_header_labels(ft, Variable = "",
                          naive = "Naïve national\nfrontier",
                          none = "No services", any = "Some services",
                          meta_m = "Matched", meta_u = "Unmatched")
  ft <- add_header_row(ft, values = c("", "", "Group frontier", "Meta-frontier"),
                       colwidths = c(1, 1, 2, 2), top = TRUE)
  ft <- align(ft, j = 2:6, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, i = hdr, j = 1, part = "body")
  ft <- italic(ft, i = hdr, j = 1, italic = TRUE)
  ft <- merge_h(ft, i = hdr, part = "body")
  ft <- padding(ft, i = setdiff(seq_len(nrow(out)), hdr), j = 1, padding.left = 12)
  ft <- fontsize(ft, size = 7, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = c(
    "Standard errors in parentheses. Significance levels: * p<0.10, ** p<0.05, *** p<0.01.",
    notes))
  ft <- fontsize(ft, size = 6.5, part = "footer")
  ft
}

# ==============================================================================
#  Tables S5 and S6 -- covariate balance
# ==============================================================================
# These read the STUDY ENVIRONMENT, not an estimation object: balance is a
# property of the matching stage. 002 writes balance_table and
# match_specification_ranking into it.
.se_obj <- function() .memo("study_environment", function() {
  if (!file.exists(.se_path))
    stop("exhibit_helpers_tables.R: missing the study environment\n  ", .se_path,
         "\n  Run the DATA + MATCHING stages.", call. = FALSE)
  readRDS(.se_path)
})

# ---- Table S5: covariate balancing, long --------------------------------------
# One row per (covariate, statistic), one column per candidate matching
# algorithm, plus the unadjusted sample. The same construction
# fig_covariate_balance() plots, so Figure S1 and this table cannot disagree.
.tblS5_live <- function() .memo("tbl:tableS5", function() {
  se  <- .se_obj()
  bal <- se$balance_table
  rk  <- se$match_specification_ranking
  if (is.null(bal) || !nrow(bal))
    stop("exhibit_helpers_tables.R: the study environment carries no ",
         "balance_table.\n  Run 002 with the cov_bal job.", call. = FALSE)
  need <- c("sample", "stat", "Coef", "value", "ARRAY", "distance", "link")
  miss <- setdiff(need, names(bal))
  if (length(miss))
    stop("exhibit_helpers_tables.R: balance_table lacks ",
         paste(miss, collapse = ", "), ". Columns present: ",
         paste(names(bal), collapse = ", "), call. = FALSE)

  # Unadjusted balance is identical across specifications (same data before
  # matching), so any ranked ARRAY serves. min() rather than a literal, matching
  # the fix already made in fig_covariate_balance().
  d <- rbind(bal[bal$sample %in% "Un" & bal$ARRAY %in% min(rk$ARRAY), ],
             bal[bal$sample %in% "Adj", ])
  d$algo <- ifelse(d$sample %in% "Un", "Unmatched",
                   ifelse(is.na(d$link), as.character(d$distance),
                          as.character(d$link)))
  d <- d[!is.na(d$value) & !is.na(d$Coef), ]
  d$stat <- c(Diff = "Std. mean difference", V_Ratio = "Variance ratio",
              KS = "KS statistic")[as.character(d$stat)]
  d <- d[!is.na(d$stat), ]

  algos <- c("Unmatched", setdiff(unique(d$algo), "Unmatched"))
  keys  <- unique(d[c("Coef", "stat")])
  keys  <- keys[order(keys$Coef, keys$stat), ]

  rows <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i, ]
    c(paste(k$Coef, k$stat, sep = "/"), as.character(k$Coef), as.character(k$stat),
      vapply(algos, function(a) {
        v <- d$value[d$algo == a & d$Coef == k$Coef & d$stat == k$stat]
        if (!length(v)) "-" else sprintf("%.4f", as.numeric(v[1]))
      }, character(1)))
  })
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- c(".key", "Covariate", "Statistic", paste0("a", seq_along(algos)))
  attr(m, "algos") <- algos
  .guard_blank(m, paste0("a", seq_along(algos)), "ft_tableS5")
  m
})

ft_tableS5 <- function() {
  m     <- .tblS5_live()
  algos <- attr(m, "algos")
  out   <- m[, setdiff(names(m), ".key"), drop = FALSE]
  ft <- flextable(out)
  ft <- do.call(set_header_labels,
                c(list(ft), list(Covariate = "Covariate", Statistic = "Statistic"),
                  stats::setNames(as.list(algos), paste0("a", seq_along(algos)))))
  ft <- align(ft, j = 3:ncol(out), align = "center", part = "all")
  ft <- align(ft, j = 1:2, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = 6.5, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = c(
    paste("Balance is best where the standardised mean difference is near zero,",
          "the variance ratio near one and the KS statistic near zero."),
    paste("The unadjusted column is the sample before matching and is identical",
          "across specifications.")))
  ft <- fontsize(ft, size = 6.5, part = "footer")
  ft
}

# ---- Table S6: balance summary and the selection criterion --------------------
# The ranking 002 computes. The selection criterion is
# ((A-0)^2 + (B-1)^2 + (C-0)^2)/3 -- distance from perfect balance -- and the
# chosen specification is the minimum.
.tblS6_live <- function() .memo("tbl:tableS6", function() {
  se <- .se_obj()
  rk <- se$match_specification_ranking
  if (is.null(rk) || !nrow(rk))
    stop("exhibit_helpers_tables.R: the study environment carries no ",
         "match_specification_ranking.", call. = FALSE)
  # NB no `.mean` suffix on these. The retired 100_FIGTAB script asked for
  # Diff.mean / V_Ratio.mean / KS.mean / rate.mean and got nothing.
  need <- c("name", "Diff", "V_Ratio", "KS", "rate")
  miss <- setdiff(need, names(rk))
  if (length(miss))
    stop("exhibit_helpers_tables.R: match_specification_ranking lacks ",
         paste(miss, collapse = ", "), ". Columns present: ",
         paste(names(rk), collapse = ", "), call. = FALSE)
  rk <- rk[order(rk$rate), ]
  m <- data.frame(
    .key      = as.character(rk$name),
    Scaling   = as.character(rk$name),
    Diff      = sprintf("%.4f", as.numeric(rk$Diff)),
    V_Ratio   = sprintf("%.4f", as.numeric(rk$V_Ratio)),
    KS        = sprintf("%.4f", as.numeric(rk$KS)),
    Criterion = sprintf("%.4f", as.numeric(rk$rate)),
    stringsAsFactors = FALSE)
  attr(m, "chosen") <- as.character(rk$name)[1]
  .guard_blank(m, c("Diff", "V_Ratio", "KS", "Criterion"), "ft_tableS6")
  m
})

ft_tableS6 <- function() {
  m   <- .tblS6_live()
  out <- m[, setdiff(names(m), ".key"), drop = FALSE]
  ft <- flextable(out)
  ft <- set_header_labels(ft, Scaling = "Scaling\nmatrix",
                          Diff = "Mean\nstandardized\ndifferences\n[A]",
                          V_Ratio = "Mean\nvariance\nratio\n[B]",
                          KS = "Kolmogorov-\nSmirnov (KS)\nStatistics\n[C]",
                          Criterion = "Selection criteria\n[((A-0)²+(B-1)²+(C-0)²)/3]")
  ft <- align(ft, j = 2:5, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, i = 1, part = "body")
  ft <- fontsize(ft, size = 8, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = paste0(
    "Rows are ordered by the selection criterion; the chosen specification is ",
    attr(m, "chosen"), ", shown in bold. [PS] marks a propensity-score link ",
    "rather than a scaling matrix."))
  ft <- fontsize(ft, size = 7, part = "footer")
  ft
}

# ---- Shared estimation-object helpers used by 6, S7 and S8 --------------------
# .read_est() keeps only ef_mean/el_mean/sf_estm/disagscors, which is all Table 7
# needs. Tables 6, S7 and S8 need the same set, so they share it.
.read_est6 <- .read_est

`%||%` <- function(a, b) if (is.null(a)) b else a

# Level-label check with a caller-supplied object name, so the error names the
# object that actually broke.
.check_level_labels <- function(d, what) {
  if (!"TCHLvel" %in% names(d))
    stop("exhibit_helpers_tables.R: ", what, " has no labelled level column ",
         "(TCHLvel).\n  Columns present: ", paste(names(d), collapse = ", "),
         "\n  Do NOT substitute the numeric Tech column -- it disagrees with ",
         "the labelled one\n  and keying on it transposes the group columns ",
         "with every star intact.", call. = FALSE)
  present <- sort(unique(as.character(d$TCHLvel)))
  if (!any(.LEVEL_LABELS %in% present))
    stop("exhibit_helpers_tables.R: ", what, "'s TCHLvel carries none of the ",
         "expected labels.\n  Expected any of: ",
         paste(.LEVEL_LABELS, collapse = ", "),
         "\n  Actually present:  ", paste(present, collapse = ", "),
         call. = FALSE)
  invisible(TRUE)
}

# As .pick(), but takes a named list and returns NA on no match rather than
# character(0). Zero rows is legitimate here -- a naive frontier has no TGR --
# and prints "-"; more than one row is still fatal.
.pick2 <- function(d, keys, col) {
  ok <- rep(TRUE, nrow(d))
  for (k in names(keys)) {
    if (!k %in% names(d))
      stop("exhibit_helpers_tables.R: column '", k, "' not in this object.\n",
           "  Columns present: ", paste(names(d), collapse = ", "), call. = FALSE)
    ok <- ok & !is.na(d[[k]]) & as.character(d[[k]]) == as.character(keys[[k]])
  }
  v <- d[[col]][ok]
  if (length(v) > 1L)
    stop("exhibit_helpers_tables.R: ", length(v), " rows matched a lookup that ",
         "must be unique.\n  Keys: ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         call. = FALSE)
  if (!length(v)) NA_real_ else as.numeric(v[1])
}


# ---- Cell lookups for the prose ----------------------------------------------
# A lookup MUST return the same build the exhibit prints, not a file. Route
# through the builders via this switch, keeping the id spelled like a filename
# so call sites need not change when a table moves.
#
# EVERY table must appear here. One left out is one section of the paper quietly
# citing a frozen value.
.live_table <- function(id) {
  switch(as.character(id),
    table1  = .tbl1_live(),
    table2  = .tbl2_live(),
    table3  = .tbl3_live(),
    table4  = .tbl4_live(),
    table5  = .tbl5_live(),
    table6  = .tbl6_live(),
    table7  = .tbl7_live(),
    tableS1 = .tblS1_live(),
    tableS2 = .tblS2_live(),
    tableS3 = .tblS3_live(),
    tableS4 = .tblS4_live(),
    tableS5 = .tblS5_live(),
    tableS6 = .tblS6_live(),
    tableS7 = .tblS7_live(),
    tableS8 = .tblS8_live(),
    stop("exhibit_helpers_tables.R: .live_table('", id, "') has no entry. ",
         "Add the table to the switch when its builder is written -- a table ",
         "missing here is prose citing a value nothing rebuilds.", call. = FALSE))
}

#' Look up a published cell by table id, row label and column.
#' Stops on a missing key: a failed knit is the designed alternative to a stale
#' number.
tbl_num <- function(id, row, col) {
  m <- .live_table(id)
  if (!col %in% names(m))
    stop("tbl_num('", id, "'): no column '", col, "'. Columns: ",
         paste(names(m), collapse = ", "), call. = FALSE)
  # Prefer the qualified .key ("TGR/Any source"); fall back to the printed label
  # only when that is itself unique. Never guess between duplicates.
  i <- if (".key" %in% names(m)) which(m$.key == row) else integer(0)
  if (!length(i)) i <- which(m[[if (".key" %in% names(m)) 2L else 1L]] == row)
  if (length(i) != 1L)
    stop("tbl_num('", id, "'): row '", row, "' matched ", length(i),
         " rows, need exactly 1.",
         # List the keys of the rows that ACTUALLY matched. The earlier version
         # filtered on `A`, a column only Table 7 has, so on every other table
         # the hint came back empty exactly when it was needed.
         if (".key" %in% names(m)) {
           cand <- if (length(i)) m$.key[i] else m$.key[nzchar(m$.key) & !grepl("^sec:", m$.key)]
           paste0("\n  Qualify it with the row key, one of: ",
                  paste(utils::head(cand, 12), collapse = ", "),
                  if (length(cand) > 12) ", ..." else "")
         } else "",
         call. = FALSE)
  v <- m[[col]][i]
  if (is.na(v) || v %in% c("", "-"))
    stop("tbl_num('", id, "', '", row, "', '", col, "') is blank. The prose ",
         "cites a value the build does not produce.", call. = FALSE)
  v
}

#' The leading numeric value of a published cell.
#'
#' tbl_num() returns the cell AS PRINTED -- "46.87 (15.26)", "-0.71** [0.31]" --
#' which is what an exhibit shows and what a sentence quoting the exhibit should
#' agree with. Prose usually needs the point estimate on its own, so this parses
#' the leading number off the same string. Parsing the printed cell rather than
#' re-reading the cache is deliberate: it cannot drift from what the table shows,
#' because it IS what the table shows.
tbl_val <- function(id, row, col) {
  v <- tbl_num(id, row, col)
  n <- suppressWarnings(as.numeric(sub("^\\s*(-?[0-9.]+).*$", "\\1", v)))
  if (is.na(n))
    stop("tbl_val('", id, "', '", row, "', '", col, "'): '", v,
         "' has no leading number.", call. = FALSE)
  n
}

#' A table's header counts.
#'
#' Tables 1, S3 and S4 carry their column n's as an attribute rather than a row,
#' because they belong to the header. The prose needs them too -- "of the 22,519
#' operators, 14,215 are in served communities" -- and typing them in by hand is
#' how a sample-size sentence outlives the sample it describes.
#'
#' `which` is the column id: "pooled"/"0"/"1" for Table 1, and
#' "pooled"/"ext"/"coop"/"assoc" for Tables S3 and S4.
tbl_n <- function(id, which) {
  N <- attr(.live_table(id), "N")
  if (is.null(N))
    stop("tbl_n('", id, "'): this table carries no header counts.", call. = FALSE)
  if (!which %in% names(N))
    stop("tbl_n('", id, "', '", which, "'): no such column. Columns: ",
         paste(names(N), collapse = ", "), call. = FALSE)
  as.numeric(N[[which]])
}

#' As tbl_num(), formatted as a percentage of a proportion-valued cell.
tbl_pct <- function(id, row, col, digits = 1) {
  v <- tbl_num(id, row, col)
  n <- suppressWarnings(as.numeric(sub("^(-?[0-9.]+).*$", "\\1", v)))
  if (is.na(n))
    stop("tbl_pct('", id, "', '", row, "', '", col, "'): '", v,
         "' is not numeric.", call. = FALSE)
  sprintf(paste0("%.", digits, "f%%"), 100 * n)
}

# ---- Page sections ------------------------------------------------------------
# Ported from land_tenure 2026-08-07, after the first render failed with
# "could not find function sec_portrait": 98/99 call these, and the earlier port
# took the chassis from the top of that file and stopped before this block.
#
# officer::block_section with type = "nextPage". officedown's BLOCK_LANDSCAPE
# markers emit type = "oddPage", which injects blank pages. reference.docx is
# US Letter with 1in margins; both are passed explicitly because officer's
# defaults would silently reflow the document.
#
# A section's properties apply to the content BEFORE it:
#   sec_portrait()  closes the preceding portrait run
#   sec_landscape() closes the preceding landscape run
# Emitting the two back to back creates an empty section, which Word renders as
# a blank page -- hence landscape exhibits are grouped into ONE run in 98/99.
.PAGE <- c(w = 8.5, h = 11)

.MAR <- function() officer::page_mar(top = 1, bottom = 1, left = 1, right = 1,
                                     header = 0.5, footer = 0.5, gutter = 0)

.sec <- function(orient) {
  # Inert for the html render: section breaks are a Word concept, and html
  # renders in the same pass. Without this guard the html output carries stray
  # officer objects.
  # requireNamespace, not knitr:: directly: 102 and run_article.R source this
  # file outside a knit, where erroring on a missing knitr would be gratuitous.
  if (!requireNamespace("knitr", quietly = TRUE)) return(invisible(NULL))
  if (!identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "docx"))
    return(invisible(NULL))
  if (!requireNamespace("officer", quietly = TRUE))
    stop("exhibit_helpers_tables.R: sec_", orient, "() needs 'officer'.\n",
         "  install.packages(\"officer\")", call. = FALSE)
  officer::block_section(officer::prop_section(
    page_size    = officer::page_size(width = .PAGE[["w"]], height = .PAGE[["h"]],
                                      orient = orient),
    page_margins = .MAR(),
    type         = "nextPage"))
}

sec_portrait  <- function() .sec("portrait")
sec_landscape <- function() .sec("landscape")
