# probe_table4_duplicates.R
# Does Table 4 print the same number in two different columns, and if so, why?
#
# A PROBE, NOT A STAGE: no number, never sourced by run_article.R, changes
# nothing. It reads the estimation objects through the manuscript's own builders,
# compares Table 4's six value columns to each other cell by cell, and writes
# exactly ONE file -- probes/logs/probe_table4_duplicates.log. It does not edit
# exhibit_helpers_tables.R, does not rebuild the workbook, does not write to
# output/, and offers no fix. REPORT ONLY.
#
# ------------------------------------------------------------------------------
# WHY THIS EXISTS
# ------------------------------------------------------------------------------
# v002 comment [155] (Appiah-Twumasi, 2025-08-16), anchored on Table 4's
# elasticity block: "These look suspicious. @FT please confirm correctness."
# The anchor is a run of adjacent cells carrying the same value.
#
# The current build still prints an identical planting-material elasticity in
# column 1 (naive national frontier) and column 6 (unmatched meta-frontier):
# 0.051 in both, with different jackknife standard errors. Two readings, and the
# paper cannot go out without knowing which:
#
#   (a) COINCIDENCE -- the two columns read two DIFFERENT rows of el_mean that
#       agree to three decimals. Nothing is wrong; the table reports a real, if
#       unlucky, fact, and the reviewer needs one sentence saying so.
#   (b) COLUMN-MAPPING ERROR -- the two columns resolve to the SAME row (the same
#       TCHLvel / sample / restrict / Survey key), so one column is a copy of the
#       other and one of the paper's frontier columns does not exist.
#
# Indistinguishable from the printed table; trivially distinguishable from the
# objects. So this probe reports, for every pair of columns and every row: the
# printed values, exact and near equality, THE FULL KEY EACH CELL READS, and
# whether the two cells select the same underlying row(s) of the same frame.
# same_key=YES or same_rows=YES is reading (b) and is a defect; same_key=no and
# same_rows=no is reading (a).
#
# THE COLUMN MAP UNDER TEST. exhibit_helpers_tables.R's .FRONT_COLS (search for
# "COLUMN MAPPING, pinned by the Nobs grid") documents what each column is
# SUPPOSED to read:
#
#   col  header                     TCHLvel    sample
#   1    Naive national frontier    National   unmatched
#   2    No credit [A]              "0"        unmatched
#   3    Some credit [B]            "1"        unmatched
#   4    Difference [B-A]           "1"        unmatched   (gap coefficient)
#   5    Meta-frontier Matched      Meta       <optimal link>
#   6    Meta-frontier Unmatched    Meta       unmatched
#
# with restrict = .RESTRICT and Survey = "GLSS0" on every cell except the
# sample-size row, which sums GLSS6 and GLSS7 (.sum_waves). The technology/
# efficiency block overrides `sample` per ROW (Matched -> optimal, Unmatched ->
# unmatched) for columns 1-4 and prints "-" in columns 5-6.
#
# ------------------------------------------------------------------------------
# SELF-VALIDATION
# ------------------------------------------------------------------------------
# The key map in .cell_spec() below is TRANSCRIBED from .tbl4_live() and
# .FRONT_COLS. A transcription can drift from the thing it describes, and a
# drifted probe reports confident nonsense. So every cell is ALSO re-resolved by
# calling the helper's own .fcell() / .fplain() / .sum_waves() with the
# transcribed arguments, and the result compared with the string the built table
# prints. Any mismatch is reported as TRANSCRIPTION DRIFT and the probe stops:
# fix the map here before believing anything else it says.
#
# ------------------------------------------------------------------------------
# HOW TO RUN
# ------------------------------------------------------------------------------
#   From the repo root, the working directory 102_exhibit_table_workbook.R uses:
#     Rscript studies/financial_inclusion/probes/probe_table4_duplicates.R
#   or interactively at the repo root:
#     source("studies/financial_inclusion/probes/probe_table4_duplicates.R")
#
# NB it opens with rm(list = ls(all = TRUE)), as 004/101/102 do. Do not source it
# into a session holding work you want to keep.
#
# Requires output/estimations/CropID_Pooled_credit_hh_TL_hnormal_optimal.rds
# (004) and the flextable dependency exhibit_helpers_tables.R loads on source.
# It does NOT need devtools::document(): every function it calls is defined in
# the helper file, not in the package.
#
# The log is written at each exit point the probe controls (success, missing
# object, transcription drift). An unanticipated error stops R before the log is
# written -- the console transcript is then the record.
#
# READING THE LOG: start at section 4 (FINDINGS) and section 6 (SUMMARY).
# Sections 1-3 are provenance and the self-check.
# ==============================================================================

rm(list = ls(all = TRUE)); gc()

# ---- Knobs -------------------------------------------------------------------
# Every threshold is named and printed into the log, so a reader can see what
# "near" meant without opening this file.
NEAR_ABS  <- 1e-3   # one unit in the last decimal of a 3dp cell: two cells this
                    # close are indistinguishable at the precision Table 4 prints
NEAR_REL  <- 0.01   # or within 1% of the larger magnitude -- for the diagnostics
                    # block, whose cells run from 0.22 to 46,612
COPY_MOST <- 0.50   # a pair sharing at least this fraction of comparable cells
                    # is reported as "largely a copy"

# ---- Output buffer -----------------------------------------------------------
# Everything goes through say(): once to the console, once to the buffer that
# becomes the log. Nothing is written anywhere else, by anything, ever.
.BUF <- character(0)
say  <- function(...) {
  line <- paste0(...)
  .BUF <<- c(.BUF, line)
  cat(line, "\n", sep = "")
  invisible(NULL)
}
rule <- function(ch = "-") say(strrep(ch, 78))
hdr  <- function(n, title) { say(""); rule("="); say(n, ". ", title); rule("=") }

# ---- Locate the builders; the log lives beside them ---------------------------
.CANDIDATES <- c(
  file.path("studies", "financial_inclusion", "scripts", "exhibit_helpers_tables.R"),
  file.path("scripts", "exhibit_helpers_tables.R"),
  file.path("..", "scripts", "exhibit_helpers_tables.R"),
  "exhibit_helpers_tables.R")
.HELPERS <- .CANDIDATES[file.exists(.CANDIDATES)][1]
if (is.na(.HELPERS))
  stop("probe_table4_duplicates: cannot find exhibit_helpers_tables.R from '",
       getwd(), "'.\n  Run this probe from the repo root, as 102 is run.\n",
       "  Looked in: ", paste(.CANDIDATES, collapse = ", "), call. = FALSE)

# studies/financial_inclusion/scripts/... -> studies/financial_inclusion/probes/logs
LOG <- Sys.getenv("ARTICLE_PROBE_LOG", unset = file.path(
  dirname(dirname(.HELPERS)), "probes", "logs", "probe_table4_duplicates.log"))

flush_log <- function() {
  dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)
  writeLines(.BUF, LOG)
  cat("\nprobe_table4_duplicates: wrote ", LOG, "\n", sep = "")
  invisible(LOG)
}

source(.HELPERS)   # into the global environment, exactly as 102 does

# Named up front so a rename in the helper gives one clear error, not six
# obscure ones. The probe's key map is transcribed from these objects and is
# worthless if any of them has moved.
.NEED <- c(".tbl4_live", ".FRONT_COLS", ".FRONT_HDR", ".RESTRICT", ".read_est",
           ".samp_of", ".opt", ".EST", ".fcell", ".fplain", ".sum_waves",
           ".T4_EL", ".T4_DIAG", ".T5_BLOCKS", "exhibit_cache_clear")
.missing <- .NEED[!vapply(.NEED, exists, logical(1))]
if (length(.missing))
  stop("probe_table4_duplicates: exhibit_helpers_tables.R no longer defines: ",
       paste(.missing, collapse = ", "),
       "\n  Re-read the Table 4 section of the helper and update .cell_spec() ",
       "before running this probe again.", call. = FALSE)

exhibit_cache_clear()   # cold read, for the reason 102 gives in its header

# ==============================================================================
hdr(1, "PROVENANCE")
# ==============================================================================
say("probe_table4_duplicates.R -- duplicate cells across Table 4's value columns.")
say("Run at:         ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say("Working dir:    ", getwd())
say("Builders:       ", .HELPERS)
say("Estimations:    ", .EST)
say("restrict pin:   ", .RESTRICT, "   (.RESTRICT in exhibit_helpers_tables.R)")
say("matched sample: ", if (is.na(.opt)) "<UNRESOLVED>" else .opt,
    "   (match_specification_optimal)")
if (is.na(.opt))
  say("  WARNING: the optimal matching sample did not resolve, so every column ",
      "keyed to it is blank. Section 4 will have little to compare.")
say("Thresholds:     NEAR_ABS = ", NEAR_ABS, ", NEAR_REL = ", NEAR_REL,
    ", COPY_MOST = ", COPY_MOST)
say("Writes:         ", LOG)
say("                (and nothing else -- this probe reports, it never patches)")

.OBJ <- file.path(.EST, "CropID_Pooled_credit_hh_TL_hnormal_optimal.rds")
if (!file.exists(.OBJ)) {
  say("")
  say("FATAL: ", .OBJ, " is absent, so Table 4 cannot be built.")
  say("       Run 004 (MSF) on the HPC, then re-run this probe.")
  flush_log()
  stop("probe_table4_duplicates: missing ", .OBJ, call. = FALSE)
}
say("Object:         ", .OBJ)
say("Object mtime:   ", format(file.info(.OBJ)$mtime, "%Y-%m-%d %H:%M:%S"))
say("                (the estimation vintage every cell below belongs to)")

T4   <- .tbl4_live()          # the same build ft_table4() prints and tbl_num() reads
VCOL <- grep("^c[0-9]+$", names(T4), value = TRUE)
NC   <- length(VCOL)
if (NC != length(.FRONT_COLS))
  say("WARNING: the build has ", NC, " value columns but .FRONT_COLS declares ",
      length(.FRONT_COLS), " -- the map below covers ",
      min(NC, length(.FRONT_COLS)), " of them.")

E      <- .read_est("credit_hh")
FRAMES <- list(el_mean = E$el_mean, ef_mean = E$ef_mean, sf_estm = E$sf_estm)
for (nm in names(FRAMES))
  say(sprintf("Frame %-8s rows: %s", nm,
              if (is.null(FRAMES[[nm]])) "<absent>" else
                format(nrow(FRAMES[[nm]]), big.mark = ",")))

# ==============================================================================
hdr(2, "WHAT EACH COLUMN READS")
# ==============================================================================
say("Keys are TCHLvel / sample / restrict / Survey. `sample` is shown both as ",
    ".FRONT_COLS")
say("declares it and as it resolves (\"OPT\" -> the optimal matching link).")
say("")
say(sprintf("%-4s %-27s %-9s %-12s %-14s %-11s %-5s",
            "col", "header", "TCHLvel", "sample(raw)", "sample(resolved)",
            "restrict", "gap"))
rule()
for (j in seq_len(min(NC, length(.FRONT_COLS)))) {
  cc <- .FRONT_COLS[[j]]
  say(sprintf("%-4d %-27s %-9s %-12s %-14s %-11s %-5s",
              j, .FRONT_HDR[j], cc$key, cc$samp,
              as.character(.samp_of(cc$samp)), .RESTRICT,
              if (isTRUE(cc$gap)) "TRUE" else "FALSE"))
}
rule()
say("Survey = \"GLSS0\" (the across-wave mean) on every cell except the ",
    "sample-size row,")
say("which sums the GLSS6 and GLSS7 rows. In the technology/efficiency block ",
    "the `sample`")
say("above is overridden by the ROW: Matched -> ",
    if (is.na(.opt)) "<unresolved>" else .opt, ", Unmatched -> unmatched, for ",
    "columns 1-4 only.")
say("")
say("KEY COLLISION CHECK. Two columns declaring the same (TCHLvel, resolved ",
    "sample, gap)")
say("triple cannot be two different columns: that is a mapping error by ",
    "construction,")
say("whatever the printed values happen to be.")
.coll <- 0L
for (a in seq_len(NC - 1)) for (b in (a + 1):NC) {
  ca <- .FRONT_COLS[[a]]; cb <- .FRONT_COLS[[b]]
  if (identical(ca$key, cb$key) &&
      identical(.samp_of(ca$samp), .samp_of(cb$samp)) &&
      identical(isTRUE(ca$gap), isTRUE(cb$gap))) {
    .coll <- .coll + 1L
    say("  COLLISION: c", a, " [", .FRONT_HDR[a], "] and c", b, " [",
        .FRONT_HDR[b], "] declare identical keys.")
  }
}
if (.coll == 0L)
  say("  none -- all ", NC, " columns declare distinct keys.")

# ==============================================================================
# Transcribed cell map (no output of its own; validated in section 3)
# ==============================================================================
# Mirrors .tbl4_live() row for row. `literal = TRUE` marks a cell the builder
# writes without a lookup (the "-" placeholders).
.cell_spec <- function(block, label, j) {
  if (j > length(.FRONT_COLS)) return(NULL)
  cc  <- .FRONT_COLS[[j]]
  gap <- isTRUE(cc$gap)

  if (identical(block, "Elasticity")) {
    i <- match(label, .T4_EL$label)
    if (is.na(i)) return(NULL)
    return(list(frame = "el_mean", literal = FALSE, kind = "cell", col = cc,
                base = list(input = .T4_EL$input[i], stat = "mean"),
                level_coef = "elasticity", gap_coef = "elasticityGap_lvl",
                keys = list(input = .T4_EL$input[i], stat = "mean",
                            CoefName = if (gap) "elasticityGap_lvl" else "elasticity",
                            TCHLvel = cc$key, sample = .samp_of(cc$samp),
                            restrict = .RESTRICT, Survey = "GLSS0")))
  }

  if (block %in% unname(.T5_BLOCKS)) {
    ty <- names(.T5_BLOCKS)[match(block, unname(.T5_BLOCKS))]
    if (!label %in% c("Matched", "Unmatched")) return(NULL)
    if (j > 4) return(list(frame = NA_character_, literal = TRUE, kind = "dash"))
    sm  <- if (identical(label, "Matched")) "OPT" else "unmatched"
    cc2 <- utils::modifyList(cc, list(samp = sm))
    return(list(frame = "ef_mean", literal = FALSE, kind = "cell", col = cc2,
                base = list(type = ty, stat = "mean", estType = "teBC"),
                level_coef = "efficiency", gap_coef = "efficiencyGap_lvl",
                keys = list(type = ty, stat = "mean", estType = "teBC",
                            CoefName = if (gap) "efficiencyGap_lvl" else "efficiency",
                            TCHLvel = cc2$key, sample = .samp_of(cc2$samp),
                            restrict = .RESTRICT, Survey = "GLSS0")))
  }

  if (identical(block, "Model diagnostics")) {
    i <- match(label, .T4_DIAG$label)
    if (is.na(i)) return(NULL)
    if (gap) return(list(frame = NA_character_, literal = TRUE, kind = "dash"))
    cf <- .T4_DIAG$coef[i]
    if (identical(cf, "Nobs"))
      return(list(frame = "sf_estm", literal = FALSE, kind = "sum_waves",
                  col = cc, coefname = cf,
                  keys = list(CoefName = cf, TCHLvel = cc$key,
                              sample = .samp_of(cc$samp), restrict = .RESTRICT,
                              Survey = "GLSS6 + GLSS7 (summed)")))
    return(list(frame = "sf_estm", literal = FALSE, kind = "plain", col = cc,
                coefname = cf, digits = .T4_DIAG$digits[i],
                keys = list(CoefName = cf, TCHLvel = cc$key,
                            sample = .samp_of(cc$samp), restrict = .RESTRICT,
                            Survey = "GLSS0")))
  }
  NULL
}

# Keys as printed vs keys usable for row matching. The sample-size row's Survey
# entry is a description of two lookups, not a value in the frame, so it is
# dropped and the match returns the rows of both waves.
.match_keys <- function(sp) {
  k <- sp$keys
  if (identical(k$Survey, "GLSS6 + GLSS7 (summed)")) k$Survey <- NULL
  k
}

# Row indices a key set selects, by .jcell()'s own matching rule. This is the
# decisive diagnostic: two cells selecting the SAME row index are the same
# estimate printed twice, whatever their declared keys say.
.rows_for <- function(df, keys) {
  if (is.null(df)) return(list(idx = integer(0), note = "frame absent"))
  miss <- setdiff(names(keys), names(df))
  if (length(miss))
    return(list(idx = integer(0),
                note = paste0("frame lacks column(s): ",
                              paste(miss, collapse = ", "))))
  ok <- rep(TRUE, nrow(df))
  for (k in names(keys)) ok <- ok & !is.na(df[[k]]) & df[[k]] == keys[[k]]
  list(idx = which(ok), note = "")
}

.key_str <- function(sp)
  paste(sprintf("%s=%s", names(sp$keys), unlist(sp$keys)), collapse = ", ")

# Walk the built table, attaching block context. Header rows carry both the
# three top-level sections and, inside "Technology/efficiency", the TGR/TE/MTE
# sub-blocks whose row labels ("Matched"/"Unmatched") repeat.
TOP  <- c("Elasticity", "Technology/efficiency", "Model diagnostics")
rows <- list(); cur_top <- NA_character_; cur_block <- NA_character_
for (i in seq_len(nrow(T4))) {
  if (identical(T4$header[i], "1")) {
    if (T4$label[i] %in% TOP) { cur_top <- T4$label[i]; cur_block <- T4$label[i] }
    else cur_block <- T4$label[i]
    next
  }
  rows[[length(rows) + 1]] <- list(i = i, label = T4$label[i], top = cur_top,
                                   block = cur_block)
}

# ==============================================================================
hdr(3, "SELF-VALIDATION (transcribed keys vs the builder)")
# ==============================================================================
drift <- 0L
for (r in rows) for (j in seq_len(NC)) {
  printed <- T4[[VCOL[j]]][r$i]
  sp <- .cell_spec(r$block, r$label, j)
  if (is.null(sp)) {
    if (nzchar(printed) && !identical(printed, "-")) {
      drift <- drift + 1L
      say("DRIFT: no spec for [", r$block, " / ", r$label, "] col ", j,
          ", yet the table prints '", printed, "'")
    }
    next
  }
  if (isTRUE(sp$literal)) {
    if (!identical(printed, "-")) {
      drift <- drift + 1L
      say("DRIFT: spec says literal '-' for [", r$block, " / ", r$label,
          "] col ", j, ", yet the table prints '", printed, "'")
    }
    next
  }
  got <- tryCatch({
    if (identical(sp$kind, "cell"))
      .fcell(FRAMES[[sp$frame]], sp$col, sp$base,
             level_coef = sp$level_coef, gap_coef = sp$gap_coef)
    else if (identical(sp$kind, "sum_waves"))
      .sum_waves(FRAMES[[sp$frame]], sp$col, sp$coefname)
    else
      .fplain(FRAMES[[sp$frame]], sp$col, sp$coefname, sp$digits)
  }, error = function(e) paste0("<ERROR: ", conditionMessage(e), ">"))
  if (!identical(as.character(got), as.character(printed))) {
    drift <- drift + 1L
    say("DRIFT: [", r$block, " / ", r$label, "] col ", j, " prints '", printed,
        "' but the transcribed key gives '", got, "'")
  }
}
if (drift == 0L) {
  say("OK: all ", length(rows) * NC, " cells reproduce exactly from the ",
      "transcribed keys,")
  say("so the map in section 2 is the map the table actually uses.")
} else {
  say("")
  say(drift, " cell(s) did not reproduce: this probe's transcription has ",
      "drifted from")
  say(".tbl4_live(). Re-read the Table 4 section of exhibit_helpers_tables.R, ",
      "update")
  say(".cell_spec(), and re-run. Section 4 is NOT reported, because it would ",
      "be wrong.")
  flush_log()
  stop("probe_table4_duplicates: transcription drift in ", drift, " cell(s).",
       call. = FALSE)
}

# ==============================================================================
hdr(4, "FINDINGS -- every pair of columns, cell by cell")
# ==============================================================================
.parse_cell <- function(x) {
  out <- list(raw = x, kind = "empty", est = NA_real_, stars = "", se = NA_real_)
  if (is.na(x) || !nzchar(trimws(x))) return(out)
  s <- trimws(x)
  if (identical(s, "-")) { out$kind <- "dash"; return(out) }
  m <- regmatches(s, regexpr("^-?[0-9][0-9,]*(\\.[0-9]+)?", s))
  if (!length(m)) { out$kind <- "text"; return(out) }
  out$est <- as.numeric(gsub(",", "", m))
  st <- regmatches(s, regexpr("\\*+", s)); if (length(st)) out$stars <- st
  p  <- regmatches(s, regexpr("\\([-0-9.]+\\)", s))
  if (length(p)) out$se <- as.numeric(gsub("[()]", "", p))
  out$kind <- if (is.na(out$se)) "plain" else "estimate"
  out
}
.comparable <- function(p) p$kind %in% c("estimate", "plain") && !is.na(p$est)

PC <- lapply(rows, function(r)
  lapply(seq_len(NC), function(j) .parse_cell(T4[[VCOL[j]]][r$i])))

say("A pair is compared only where BOTH cells parse to a number; \"-\" and ",
    "blanks are skipped.")
say("")
say("  EXACT      identical printed strings (estimate, stars and SE all equal)")
say("  EST-EQ     equal point estimates, differing stars and/or standard error")
say("  NEAR       |a-b| <= ", NEAR_ABS, "  or  |a-b|/max(|a|,|b|) <= ", NEAR_REL)
say("  same_key   both cells resolve to the SAME key set        -> mapping error")
say("  same_rows  both cells select the SAME row(s) of the frame -> the same")
say("             estimate printed twice, whatever the keys say")

pair_stat <- list(); findings <- character(0)
for (a in seq_len(NC - 1)) for (b in (a + 1):NC) {
  n_cmp <- 0L; n_exact <- 0L; n_est <- 0L; n_near <- 0L; lines <- character(0)
  for (ri in seq_along(rows)) {
    r  <- rows[[ri]]
    pa <- PC[[ri]][[a]]; pb <- PC[[ri]][[b]]
    if (!.comparable(pa) || !.comparable(pb)) next
    n_cmp <- n_cmp + 1L
    d   <- abs(pa$est - pb$est)
    den <- max(abs(pa$est), abs(pb$est))
    rel <- if (den > 0) d / den else 0
    is_exact <- identical(pa$raw, pb$raw)
    is_est   <- !is_exact && pa$est == pb$est
    is_near  <- !is_exact && !is_est && (d <= NEAR_ABS || rel <= NEAR_REL)
    if (!(is_exact || is_est || is_near)) next

    # Spec first: a cell with no spec, or a literal "-", has no key to report and
    # must not be counted as a duplicate. Section 3 has already established that
    # neither case can carry a parsed number, so this guard should never fire --
    # it is here so that a future change to .tbl4_live() degrades to a skipped
    # row rather than to a subscript error on FRAMES[[NA]].
    sa <- .cell_spec(r$block, r$label, a); sb <- .cell_spec(r$block, r$label, b)
    if (is.null(sa) || is.null(sb) || isTRUE(sa$literal) || isTRUE(sb$literal)) {
      say("  SKIPPED ", r$label, " c", a, "/c", b,
          ": a cell printed a number the transcribed map does not key. ",
          "Re-read .cell_spec().")
      next
    }
    n_exact <- n_exact + is_exact; n_est <- n_est + is_est; n_near <- n_near + is_near
    same_key  <- identical(sa$keys, sb$keys)
    ra <- .rows_for(FRAMES[[sa$frame]], .match_keys(sa))
    rb <- .rows_for(FRAMES[[sb$frame]], .match_keys(sb))
    same_rows <- length(ra$idx) > 0 && identical(ra$idx, rb$idx)
    tag <- if (is_exact) "EXACT" else if (is_est) "EST-EQ" else "NEAR"
    verdict <- if (same_key || same_rows) "MAPPING ERROR" else
      if (is_exact || is_est) "coincidence: distinct rows, equal values" else
        "close, distinct rows"

    lines <- c(lines, sprintf(
      "  %-6s %-26s c%d=%-20s c%d=%-20s d=%.6f rel=%.4f same_key=%-3s same_rows=%-3s %s",
      tag, r$label, a, pa$raw, b, pb$raw, d, rel,
      if (same_key) "YES" else "no", if (same_rows) "YES" else "no", verdict))
    lines <- c(lines, sprintf(
      "         block=%s frame=%s rows(c%d)=[%s] rows(c%d)=[%s]%s",
      r$block, sa$frame, a, paste(ra$idx, collapse = ","),
      b, paste(rb$idx, collapse = ","),
      if (nzchar(ra$note) || nzchar(rb$note))
        paste0(" NOTE: ", ra$note, rb$note) else ""))
    lines <- c(lines, sprintf("         c%d keys: %s", a, .key_str(sa)))
    lines <- c(lines, sprintf("         c%d keys: %s", b, .key_str(sb)))
    if (same_key || same_rows)
      findings <- c(findings, sprintf(
        "c%d [%s] vs c%d [%s] -- %s / %s: same %s",
        a, .FRONT_HDR[a], b, .FRONT_HDR[b], r$block, r$label,
        if (same_key) "key set" else "source row(s)"))
  }
  frac <- if (n_cmp) (n_exact + n_est) / n_cmp else NA_real_
  pair_stat[[length(pair_stat) + 1]] <- list(a = a, b = b, n = n_cmp,
    exact = n_exact, est = n_est, near = n_near, frac = frac)
  say("")
  say("c", a, " [", .FRONT_HDR[a], "]  vs  c", b, " [", .FRONT_HDR[b], "]")
  say("  comparable rows: ", n_cmp, " | exact: ", n_exact, " | est-eq: ", n_est,
      " | near: ", n_near,
      if (is.na(frac)) "" else sprintf(" | duplicate fraction: %.2f", frac))
  if (length(lines)) for (l in lines) say(l) else
    say("  (no duplicates or near-duplicates)")
}

# ==============================================================================
hdr(5, "IS ANY COLUMN A COPY OF ANOTHER?")
# ==============================================================================
say("Duplicate fraction = (exact + est-eq) / comparable rows, over the whole ",
    "table.")
say("1.00 means the two columns are indistinguishable wherever both print a ",
    "number.")
say("")
say(sprintf("%-5s %-5s %-6s %-7s %-7s %-6s %-7s %s",
            "colA", "colB", "n", "exact", "est-eq", "near", "frac", "verdict"))
rule()
for (p in pair_stat) {
  verdict <- if (is.na(p$frac) || p$n == 0) "no comparable cells" else
    if (p$frac >= 1)          "WHOLLY A COPY -- check the column map" else
      if (p$frac >= COPY_MOST) "LARGELY A COPY -- check the column map" else
        "distinct"
  say(sprintf("%-5d %-5d %-6d %-7d %-7d %-6d %-7s %s", p$a, p$b, p$n, p$exact,
              p$est, p$near, if (is.na(p$frac)) "-" else sprintf("%.2f", p$frac),
              verdict))
}
rule()

# ==============================================================================
hdr(6, "SUMMARY")
# ==============================================================================
if (length(findings)) {
  say("MAPPING ERRORS -- a cell reads the same key set, or the same source ",
      "row(s), in two columns:")
  for (f in unique(findings)) say("  - ", f)
  say("")
  say("Each of these means one Table 4 column is not the frontier its header ",
      "names. Fix")
  say(".FRONT_COLS (or the row map that overrides it) in ",
      "exhibit_helpers_tables.R, then")
  say("re-run this probe and 102 before anything is re-rendered.")
} else {
  say("No cell in Table 4 resolves to the same key set or the same source ",
      "row(s) in two columns.")
  say("Every duplicate listed in section 4 is two DISTINCT estimates agreeing ",
      "at the precision")
  say("the table prints -- a coincidence, not a copied column. That is the ",
      "answer comment [155]")
  say("asks for, and section 4 is the cell-by-cell evidence for it.")
}
say("")
say("Caveats a reader of this log must carry:")
say("  1. This tests the INTERNAL consistency of the column mapping. It cannot ",
    "tell you whether")
say("     .FRONT_COLS names the right frontier for each header -- that was ",
    "pinned by the Nobs")
say("     grid, and the sample-size row of the current build is blank, so the ",
    "pin is unverified.")
say("  2. Every number belongs to the estimation vintage stamped in section 1. ",
    "The objects were")
say("     fitted before 002_MATCHING was last re-run; a 004 re-run changes ",
    "every cell, and this")
say("     probe must be re-run after it.")
say("  3. restrict is pinned to '", .RESTRICT, "' here, while Tables 5 and 6 ",
    "read 'Unrestricted'.")
say("     A duplicate under one pin need not appear under the other; flip ",
    ".RESTRICT in the")
say("     helper and re-run if that question arises.")
say("  4. Repetition is EXPECTED in parts of the diagnostics block -- the ",
    "number of parameters")
say("     is 19 in every column by construction. Such rows are reported, not ",
    "suppressed: this")
say("     probe does not decide which coincidences are innocent.")
say("  5. Nothing here was changed. The only file written is this log.")

flush_log()
invisible(NULL)
