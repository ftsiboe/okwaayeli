# 102_exhibit_table_workbook.R  (10x = compute/emit; see scripts/README.md)
# Emit every manuscript table AS PRINTED to
# output/tables/financial_inclusion_tables.xlsx, one sheet per table -- a
# deliverable for co-authors and supplementary material.
#
# "As printed" means the flextable objects the Rmd renders, not the data behind
# them: stars, jackknife SEs in parentheses, sprintf rounding, "-" placeholders,
# headers, spanners and footnotes. Same ft_*() call as the paper, so the two
# cannot disagree.
#
# NOT A ROUND TRIP -- nothing reads this back, and nothing should. Excel is fine
# as a final output; an exhibit that reads its numbers out of one is not.
#
# Run from the repo root, AFTER 100 (descriptive cache) and 004 (estimations).
#
# ==============================================================================
# WHY THE CACHE IS CLEARED FIRST
# ==============================================================================
# exhibit_helpers_tables.R memoises every estimation object it reads (.CACHE,
# session-lived). run_article.R sources the stages into one session, so if
# DESCRIPTIVE or FIGURES ran earlier in the same call, this script would build
# its workbook from whatever those stages cached -- which is correct right up
# until the run that re-estimates something, and then silently is not.
#
# The workbook is the artefact people forward to co-authors without the paper
# beside it. It gets a cold read.
# ==============================================================================

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name <- "financial_inclusion"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name, "_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

# The builders. Sourced, not attached: exhibit_helpers_tables.R is a library and
# resolves its own paths via .STUDY_ROOT, so it does not care that we are at the
# repo root here and at narrative/ during a knit.
source("studies/financial_inclusion/scripts/exhibit_helpers_tables.R")

if (!requireNamespace("flextable", quietly = TRUE))
  stop("102: package 'flextable' is required.", call. = FALSE)
if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("102: package 'openxlsx' is required.", call. = FALSE)

exhibit_cache_clear()   # see the header -- always a cold read

# Flatten a flextable to the character grid it PRINTS: header, body, footer.
# Hand-written because this flextable version does not export save_as_xlsx().
#
# ==============================================================================
# WHY THE HEADER IS READ FROM $content AND THE REST FROM $dataset
# ==============================================================================
# The obvious implementation reads $dataset for all three parts. It is what
# land_tenure's 102 does, and for land_tenure it happens to work.
#
# It is wrong for a single-header-row table. set_header_labels() rewrites what
# flextable DISPLAYS -- $header$content -- and leaves $header$dataset holding the
# original column names. add_header_row() writes its values into the dataset, so
# any table carrying a spanner comes out right and masks the bug. Every
# land_tenure table has a spanner (bar Table S0, whose header genuinely is its
# column names), so nothing there ever exposed it.
#
# Table 5 here has no spanner. Read from $dataset its sheet is headed
#
#     label | c1 | c2 | c3
#
# instead of
#
#     | No credit [A] | Some credit [B] | Difference [B-A]
#
# -- a sheet of correct numbers in unlabelled columns, in a workbook whose whole
# purpose is being read away from the paper. Every cell right, the table
# unusable.
#
# So: the header comes from $content (what is printed), the body and footer from
# $dataset (proven, and they carry no set_header_labels() equivalent). If the
# content walk fails or comes back empty, it falls back to $dataset rather than
# writing nothing -- and .warn_raw_header() below says so out loud.
# ==============================================================================

# Text of one part as printed. flextable stores each cell as a data.frame of
# chunks with a `txt` column; concatenating them yields the rendered string
# including stars and parenthetical SEs.
.part_content <- function(ft, p) {
  ct <- tryCatch(ft[[p]]$content$data, error = function(e) NULL)
  if (is.null(ct) || !is.matrix(ct) || !nrow(ct)) return(NULL)
  m <- matrix("", nrow(ct), ncol(ct))
  for (i in seq_len(nrow(ct))) for (j in seq_len(ncol(ct))) {
    ch <- ct[[i, j]]
    if (is.data.frame(ch) && "txt" %in% names(ch)) {
      v <- ch$txt[!is.na(ch$txt)]
      m[i, j] <- if (length(v)) paste0(v, collapse = "") else ""
    }
  }
  if (!any(nzchar(m))) return(NULL)
  unname(m)
}

.part_dataset <- function(ft, p) {
  d <- tryCatch(ft[[p]]$dataset, error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(NULL)
  unname(as.matrix(as.data.frame(lapply(d, as.character),
                                 stringsAsFactors = FALSE)))
}

# A header row identical to the body's column names means the content walk fell
# through and set_header_labels() has been lost. Not fatal -- the numbers are
# still right -- but the sheet ships unlabelled, so it must not pass silently.
.warn_raw_header <- function(h, ft, nm) {
  if (is.null(h)) return(invisible(NULL))
  raw <- names(ft$body$dataset)
  if (any(apply(h, 1, function(r) identical(as.character(r), as.character(raw)))))
    warning("102: '", nm, "' header reads as raw column names (",
            paste(utils::head(raw, 4), collapse = ", "),
            "...). flextable's internals have changed -- .part_content() needs ",
            "updating, or this sheet ships without column titles.",
            call. = FALSE, immediate. = TRUE)
  invisible(NULL)
}

.ft_grid <- function(ft, nm) {
  b <- .part_dataset(ft, "body")
  if (is.null(b))
    stop("102: could not read the body of '", nm, "'. flextable's internals ",
         "have changed -- .ft_grid() needs updating.", call. = FALSE)
  h <- .part_content(ft, "header")
  if (is.null(h)) h <- .part_dataset(ft, "header")
  .warn_raw_header(h, ft, nm)
  f <- .part_dataset(ft, "footer")
  pad <- function(m) {
    if (is.null(m)) return(NULL)
    if (ncol(m) == ncol(b)) return(m)
    if (ncol(m) >  ncol(b)) return(m[, seq_len(ncol(b)), drop = FALSE])
    cbind(m, matrix("", nrow(m), ncol(b) - ncol(m)))   # footer lines span
  }
  as.data.frame(rbind(pad(h), b, pad(f)), stringsAsFactors = FALSE)
}

# Sheet name -> builder. Sheet names are the manuscript's numbering because this
# workbook is read by humans holding the paper; everything upstream is named by
# functionality instead, on the reasoning that table numbers move.
# NB not `TABLES` -- article_helpers.R defines that as the output/tables path.
#
# This study has ten tables against land_tenure's twelve, and no Table S0.
# The order here is the order they appear in the manuscript: main body first,
# then the appendix.
#
# Held as NAMES, not as the functions themselves. Writing `= ft_table1` would
# resolve at list-construction time, so a builder that had been renamed would
# die with "object 'ft_table1' not found" and no indication of which sheet or
# what else was wrong -- and the validity check below could never run, because
# the line defining what to check would be the line that failed.
TABLE_BUILDERS <- c(
  "Table 1"  = "ft_table1",  "Table 2"  = "ft_table2",  "Table 3"  = "ft_table3",
  "Table 4"  = "ft_table4",  "Table 5"  = "ft_table5",  "Table 6"  = "ft_table6",
  "Table S1" = "ft_tableS1", "Table S2" = "ft_tableS2",
  "Table S3" = "ft_tableS3", "Table S4" = "ft_tableS4"
)

# Validate every name BEFORE building anything. Table 1 is the expensive one, so
# a typo in "Table S4" would otherwise surface minutes in -- which is exactly how
# 100_exhibit_descriptive_stats.R lost a run to a missing CONT_ROWS: the symbol
# was checked after the slow part rather than before it.
.absent <- TABLE_BUILDERS[!vapply(TABLE_BUILDERS, exists,
                                  FUN.VALUE = logical(1), mode = "function")]
if (length(.absent))
  stop("102: no builder function named: ", paste(.absent, collapse = ", "),
       "\n  Defined in exhibit_helpers_tables.R: ",
       paste(sort(grep("^ft_", ls(), value = TRUE)), collapse = ", "),
       call. = FALSE)

# Build every table before writing any of it, so a failure cannot leave a
# workbook that silently omits one.
built <- list()
for (nm in names(TABLE_BUILDERS)) {
  message("  building ", nm, " ...")
  built[[nm]] <- get(TABLE_BUILDERS[[nm]], mode = "function")()
}

grids <- stats::setNames(
  lapply(names(built), function(nm) .ft_grid(built[[nm]], nm)), names(built))

out <- file.path(study_dir_tables(study_environment),
                 "financial_inclusion_tables.xlsx")
openxlsx::write.xlsx(grids, file = out, colNames = FALSE, overwrite = TRUE)

message("Wrote ", out, "  (", length(grids), " sheets)")
invisible(TRUE)
