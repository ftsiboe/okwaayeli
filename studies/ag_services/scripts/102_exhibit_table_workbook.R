# 102_exhibit_table_workbook.R  (1## = exhibits; see scripts/README.md)
# Emit every manuscript table AS PRINTED to output/tables/ag_services_tables.xlsx,
# one sheet per table -- a deliverable for co-authors and supplementary material.
#
# "As printed" means the flextable objects the Rmd renders, not the data behind
# them: stars, jackknife SEs in parentheses, sprintf rounding, "-" placeholders,
# headers, spanners and footnotes. Same ft_*() call as the paper, so the two
# cannot disagree.
#
# NOT A ROUND TRIP -- nothing reads this back, and nothing should. Excel is fine
# as a final output; an exhibit that reads its numbers out of one is not. This
# replaces the three retired workbooks (ag_services_results.xlsx,
# -msf.xlsx, -summary-statistics.xlsx) with a single build-time deliverable.
#
# Run from the repo root, AFTER 100 (descriptive cache) and 101 (figure data).
#
# PARTIAL BUILDS ARE EXPECTED, FOR NOW.
# Twelve of the fifteen builders in exhibit_helpers_tables.R are still
# .not_yet() stubs. This script builds what exists, records what does not, and
# says so -- on the console AND on a "Contents" sheet inside the workbook, so a
# co-author opening the file cannot mistake an incomplete workbook for a
# complete one. It does NOT stop on a stub: refusing to emit Table 7 because
# Table 3 is unwritten would make the finished builders unusable for months.
#
# What it will never do is emit a placeholder. AG_PREVIEW is forced off below;
# a preview box carries no data and has no business in a deliverable.

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name <- "ag_services"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name, "_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

# ---- AG_PREVIEW off, unconditionally ----------------------------------------
# .not_yet() renders a yellow PLACEHOLDER box when AG_PREVIEW=1. That is for
# checking the document layout, never for a file anyone receives. Restored on
# exit so a preview session survives running this.
.preview_was <- Sys.getenv("AG_PREVIEW", unset = NA)
Sys.setenv(AG_PREVIEW = "0")
on.exit({
  if (is.na(.preview_was)) Sys.unsetenv("AG_PREVIEW")
  else Sys.setenv(AG_PREVIEW = .preview_was)
}, add = TRUE)

# The builders. Sourced, not attached: exhibit_helpers_tables.R is a library and
# resolves its own paths via .STUDY_ROOT, so it does not care that we are at the
# repo root here and at narrative/ during a knit.
source("studies/ag_services/scripts/exhibit_helpers_tables.R")

if (!requireNamespace("flextable", quietly = TRUE))
  stop("102: package 'flextable' is required.", call. = FALSE)
if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("102: package 'openxlsx' is required.", call. = FALSE)

# Flatten a flextable to the character grid it prints: header, body, footer.
# Hand-written because this flextable version does not export save_as_xlsx().
#
# Reads flextable internals ($header/$body/$footer $dataset). The alternative was
# a duplicate copy of every column title, which is how exhibits drift. If a
# flextable upgrade breaks this, the stop() below says so rather than writing
# junk.
.ft_grid <- function(ft, nm) {
  part <- function(p) {
    d <- ft[[p]]$dataset
    if (is.null(d) || !nrow(d)) return(NULL)
    m <- as.matrix(as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE))
    unname(m)
  }
  b <- part("body")
  if (is.null(b))
    stop("102: could not read the body of '", nm, "'. flextable's internals ",
         "have changed -- .ft_grid() needs updating.", call. = FALSE)
  h <- part("header")
  f <- part("footer")
  pad <- function(m) {
    if (is.null(m)) return(NULL)
    if (ncol(m) == ncol(b)) return(m)
    cbind(m, matrix("", nrow(m), ncol(b) - ncol(m)))   # footer lines span
  }
  as.data.frame(rbind(pad(h), b, pad(f)), stringsAsFactors = FALSE)
}

# Sheet name -> builder. Sheet names are the manuscript's numbering because this
# workbook is read by humans holding the paper; everything upstream is named by
# functionality instead, on the reasoning that table numbers move.
# NB not `TABLES` -- article_helpers.R defines that as the output/tables path.
#
# THE MANIFEST IS THE FULL FIFTEEN, including the unwritten ones. A manifest
# that listed only what works would report a complete workbook every time.
TABLE_BUILDERS <- list(
  "Table 1"  = ft_table1,  "Table 2"  = ft_table2,  "Table 3"  = ft_table3,
  "Table 4"  = ft_table4,  "Table 5"  = ft_table5,  "Table 6"  = ft_table6,
  "Table 7"  = ft_table7,
  "Table S1" = ft_tableS1, "Table S2" = ft_tableS2, "Table S3" = ft_tableS3,
  "Table S4" = ft_tableS4, "Table S5" = ft_tableS5, "Table S6" = ft_tableS6,
  "Table S7" = ft_tableS7, "Table S8" = ft_tableS8
)

# Build every table before writing any of it, so a failure cannot leave a
# workbook that silently omits one.
#
# Two failure modes, told apart on purpose:
#   PENDING - the builder is a .not_yet() stub. Expected; recorded, not fatal.
#   FAILED  - a written builder errored. That is a real break in the pipeline,
#             and it is loud.
built  <- list()
status <- data.frame(sheet = names(TABLE_BUILDERS), state = NA_character_,
                     detail = NA_character_, stringsAsFactors = FALSE)

for (i in seq_along(TABLE_BUILDERS)) {
  nm <- names(TABLE_BUILDERS)[i]
  message("  building ", nm, " ...")
  r <- tryCatch(TABLE_BUILDERS[[i]](), error = function(e) e)
  if (inherits(r, "error")) {
    msg <- conditionMessage(r)
    if (grepl("is not written yet", msg, fixed = TRUE)) {
      status$state[i]  <- "PENDING"
      status$detail[i] <- sub("^.*?\\n\\s*", "", strsplit(msg, "\n  Verify")[[1]][1])
      message("    PENDING - builder not written")
    } else {
      status$state[i]  <- "FAILED"
      status$detail[i] <- msg
      message("    FAILED  - ", msg)
    }
    next
  }
  built[[nm]]      <- r
  status$state[i]  <- "built"
  status$detail[i] <- ""
}

.failed <- status$sheet[status$state %in% "FAILED"]
if (length(.failed))
  stop("102: ", length(.failed), " written builder(s) errored: ",
       paste(.failed, collapse = ", "),
       "\n  A written builder that fails is a break, not a gap. Fix it before ",
       "shipping a workbook\n  that omits its table. Details are on the console ",
       "above.", call. = FALSE)

if (!length(built))
  stop("102: no table built. Nothing to write.", call. = FALSE)

grids <- stats::setNames(
  lapply(names(built), function(nm) .ft_grid(built[[nm]], nm)), names(built))

# ---- Contents sheet ----------------------------------------------------------
# First sheet, so the coverage of this file is the first thing anyone sees.
# "No silent caps": a workbook that quietly contains 3 of 15 tables reads as a
# workbook of 3 tables.
contents <- data.frame(
  c(paste0(project_name, " -- manuscript tables as printed"),
    paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"),
           " by scripts/102_exhibit_table_workbook.R"),
    paste0(sum(status$state == "built"), " of ", nrow(status),
           " tables in this file."),
    "",
    "Every sheet is the same flextable the manuscript renders. Nothing reads",
    "this workbook back into the pipeline.",
    ""),
  stringsAsFactors = FALSE)
names(contents) <- " "
contents <- rbind(contents,
                  stats::setNames(data.frame(
                    sprintf("%-10s %-8s %s", status$sheet, status$state,
                            ifelse(is.na(status$detail), "", status$detail)),
                    stringsAsFactors = FALSE), " "))

out <- file.path(study_dir_tables(study_environment),
                 paste0(project_name, "_tables.xlsx"))
openxlsx::write.xlsx(c(list(Contents = contents), grids), file = out,
                     colNames = FALSE, overwrite = TRUE)

.pending <- status$sheet[status$state %in% "PENDING"]
message("Wrote ", out, "  (", length(grids), " table sheets + Contents)")
if (length(.pending))
  message("102: ", length(.pending), " table(s) NOT in this workbook -- their ",
          "builders are unwritten:\n  ", paste(.pending, collapse = ", "),
          "\n  This is a PARTIAL deliverable. Say so when you send it.")

invisible(TRUE)
