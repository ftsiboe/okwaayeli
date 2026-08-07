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

ft_table1  <- function() .not_yet("ft_table1",  "Draft Table 1  -- summary statistics; needs 100_exhibit_descriptive_stats.R.")
ft_table2  <- function() .not_yet("ft_table2",  "Draft Table 2  -- extension agencies; NO workbook counterpart.")
ft_table3  <- function() .not_yet("ft_table3",  "Draft Table 3  -- extension access patterns; NO workbook counterpart.")
ft_table4  <- function() .not_yet("ft_table4",  "Draft Table 4  -- community services; ref sumstat::Table2-services.")
ft_table5  <- function() .not_yet("ft_table5",  "Draft Table 5  -- advisory compliance; NO workbook counterpart.")
ft_table6  <- function() .not_yet("ft_table6",  "Draft Table 6  -- input elasticities; ref msf::Table3.")
ft_tableS3 <- function() .not_yet("ft_tableS3", "Draft Table S3 -- summary stats; ref sumstat::TableS2.")
ft_tableS4 <- function() .not_yet("ft_tableS4", "Draft Table S4 -- trends; ref sumstat::TableS3.")
ft_tableS5 <- function() .not_yet("ft_tableS5", "Draft Table S5 -- covariate balancing; ref msf::CovBalDATA.")
ft_tableS6 <- function() .not_yet("ft_tableS6", "Draft Table S6 -- balance summary; ref msf::ranking.")
ft_tableS7 <- function() .not_yet("ft_tableS7", "Draft Table S7 -- MSF production function; ref msf::TableS4.")
ft_tableS8 <- function() .not_yet("ft_tableS8", "Draft Table S8 -- inefficiency determinants; ref msf::Table5.")

# ---- Cell lookups for the prose ----------------------------------------------
# A lookup MUST return the same build the exhibit prints, not a file. Route
# through the builders via this switch, keeping the id spelled like a filename
# so call sites need not change when a table moves.
#
# EVERY table must appear here. One left out is one section of the paper quietly
# citing a frozen value.
.live_table <- function(id) {
  switch(as.character(id),
    table7  = .tbl7_live(),
    tableS1 = .tblS1_live(),
    tableS2 = .tblS2_live(),
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
         if (".key" %in% names(m))
           paste0("\n  Qualify it as one of: ",
                  paste(m$.key[m$A != ""], collapse = ", ")) else "",
         call. = FALSE)
  v <- m[[col]][i]
  if (is.na(v) || v %in% c("", "-"))
    stop("tbl_num('", id, "', '", row, "', '", col, "') is blank. The prose ",
         "cites a value the build does not produce.", call. = FALSE)
  v
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
