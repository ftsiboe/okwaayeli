# probes/probe_wiring.R
# ==============================================================================
# Resolve EVERY inline R expression in narrative/sections/ and report which
# succeed and which fail -- in one pass, before a render.
#
# Why: a table lookup that misses stops the knit (by design, no fallbacks), and
# a render surfaces exactly one failure per attempt. With ~100 wired numbers
# that is ~100 renders. This evaluates them all and prints the failures
# together, so wiring converges in one iteration instead of many.
#
# READ-ONLY apart from probes/logs/. From the repo root:
#   source("studies/financial_inclusion/probes/probe_wiring.R")
#
# ------------------------------------------------------------------------------
# TWO DEFECTS FIXED 2026-08-08, both found by reading a PASSING log
# ------------------------------------------------------------------------------
# 1. The HTML-comment stripper did not strip. `gsub("<!--.*?-->", ..., perl=TRUE)`
#    leaves `.` not matching newline, so every multi-line section banner survived
#    and its worked examples were evaluated as if they were call sites. The run
#    reported 101 expressions where the sections contain 82; the 19 extra were
#    the banners. Nothing was wrong in the document -- the COUNT was wrong, and a
#    count you cannot trust is worse than no count. Fixed with (?s).
#
# 2. "Resolved" was equated with "produced a number". It is not. fmt_pct(NULL)
#    returned the string "%" -- no error, so the probe called it ok -- and five
#    credit shares in the Data section would have rendered as bare percent signs.
#    article_helpers.R now errors on NULL/NA/empty, and .suspect() below catches
#    the same shape from any other source. A probe that cannot fail a blank cell
#    is a probe that certifies blank cells.
# ==============================================================================

STUDY <- "studies/financial_inclusion"
SECD  <- file.path(STUDY, "narrative", "sections")
LOGD  <- file.path(STUDY, "probes", "logs")
dir.create(LOGD, recursive = TRUE, showWarnings = FALSE)

# Output that is technically a value and substantively a hole. Anything matching
# renders into the document as a gap the eye slides over.
.suspect <- function(s) {
  if (!length(s) || !nzchar(trimws(s))) return("empty")
  t <- trimws(s)
  if (t %in% c("%", "NA", "NA%", "NaN", "NULL", "Inf", "-Inf", "NA.NA")) return(t)
  if (grepl("^(NA|NaN|Inf|-Inf)", t)) return(t)
  ""
}

out <- utils::capture.output({
  cat("probe_wiring.R  |  ", format(Sys.time()), "\n\n", sep = "")

  # The Rmd sources these at knit time with the working directory at narrative/.
  # Here we are at the repo root; .STUDY_ROOT probes for that.
  source(file.path(STUDY, "scripts", "article_helpers.R"))
  source(file.path(STUDY, "scripts", "exhibit_helpers_tables.R"))
  OJ <- file.path(STUDY, "narrative", "article_objects.json")
  objs <- if (file.exists(OJ)) jsonlite::fromJSON(OJ) else NULL
  if (is.null(objs)) {
    cat("NOTE: article_objects.json absent; objs$ lookups will fail.\n\n")
  } else {
    cat("article_objects.json  written ", format(file.mtime(OJ)),
        "   top-level keys: ", paste(names(objs), collapse = ", "), "\n\n", sep = "")
  }

  files <- list.files(SECD, pattern = "\\.Rmd$", full.names = TRUE)
  ok <- 0L; fail <- 0L; frozen <- 0L; susp <- 0L
  problems <- character(0)

  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # Strip HTML comments FIRST. Section banners document the conventions with
    # worked examples, and those are not call sites. (?s) is load-bearing: without
    # it `.` stops at the first newline and multi-line banners survive intact.
    txt <- gsub("(?s)<!--.*?-->", "", txt, perl = TRUE)
    exprs <- regmatches(txt, gregexpr("`r [^`]+`", txt))[[1]]
    if (!length(exprs)) next
    cat("---- ", basename(f), "  (", length(exprs), " expressions)\n", sep = "")
    for (e in exprs) {
      code <- sub("^`r\\s*", "", sub("`$", "", e))
      if (grepl('^N\\(', code)) { frozen <- frozen + 1L; next }
      v <- tryCatch(eval(parse(text = code)),
                    error = function(err) structure(conditionMessage(err),
                                                    class = "probe_err"))
      if (inherits(v, "probe_err")) {
        fail <- fail + 1L
        cat("  FAIL  ", code, "\n        ", gsub("\n", "\n        ", v), "\n", sep = "")
        problems <- c(problems, paste(basename(f), code, sep = " :: "))
        next
      }
      shown <- paste(utils::head(as.character(v), 1), collapse = "")
      bad   <- .suspect(shown)
      if (nzchar(bad)) {
        susp <- susp + 1L
        cat(sprintf("  BLANK %-70s -> [%s]\n", substr(code, 1, 70), bad))
        problems <- c(problems, paste0(basename(f), " :: ", code, "   (renders as '", bad, "')"))
      } else {
        ok <- ok + 1L
        cat(sprintf("  ok    %-70s -> %s\n", substr(code, 1, 70), shown))
      }
    }
    cat("\n")
  }

  cat(strrep("=", 70), "\n", sep = "")
  cat(sprintf("resolved: %d    failed: %d    blank: %d    still frozen: %d\n",
              ok, fail, susp, frozen))
  if (fail || susp) {
    cat("\nproblems:\n"); for (p in problems) cat("  ", p, "\n", sep = "")
    if (susp)
      cat("\nBLANK means the expression evaluated without error and produced\n",
          "nothing a reader can use. It will not stop the knit. It will print a\n",
          "gap in the sentence. Treat it as a failure.\n", sep = "")
  } else {
    cat("\nEvery wired expression resolves to a value. A render will not stop on a\n",
        "lookup, and no sentence will contain a hole.\n", sep = "")
  }
  if (frozen) cat(sprintf("\n%d literals remain frozen -- grep the sections for N(\n", frozen))
})

writeLines(out, file.path(LOGD, "probe_wiring.log"))
cat(out, sep = "\n")
cat("\nwrote ", file.path(LOGD, "probe_wiring.log"), "\n", sep = "")
