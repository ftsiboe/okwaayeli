# 102_exhibit_table_workbook.R  (10x = compute/emit; see scripts/README.md)
# Emit every manuscript table AS PRINTED to
# output/tables/resource_extraction_tables.xlsx, one sheet per table -- a
# deliverable for co-authors and supplementary material.
#
# "As printed" means the flextable objects the Rmd renders, not the data behind
# them: stars, jackknife SEs in parentheses, sprintf rounding, "-" placeholders,
# headers, spanners and footnotes. Same ft_*() call as the paper, so the two
# cannot disagree.
#
# NOT A ROUND TRIP -- nothing reads this back, and nothing should. Excel is fine
# as a final output; an exhibit that reads its numbers out of one is not. (The
# thing being retired in this study is exactly that: data/tables/*.csv, extracted
# from output/resource_extraction_results.xlsx. See ../AGENT_PROMPT.md.)
#
# CAVEAT WHILE THE PORT IS UNFINISHED: ft_tableA1() and ft_tableA4()-ft_tableA9()
# still read the frozen data/tables/*.csv rather than the estimation objects, so
# for now this workbook faithfully reproduces frozen values for those sheets. It
# is honest about what the paper currently prints; it is not yet evidence that
# the numbers came from the pipeline. Tables 1, 2, A2 and A3 become live once
# 100_exhibit_descriptive_stats.R's cache is wired into the builders.
#
# Run from the repo root, AFTER 100 (descriptive cache) and 101 (figure data).

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name <- "resource_extraction"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name, "_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

# The builders. Sourced, not attached: exhibit_helpers_tables.R is a library and
# resolves its own paths via .STUDY_ROOT, so it does not care that we are at the
# repo root here and at narrative/ during a knit.
source("studies/resource_extraction/scripts/exhibit_helpers_tables.R")

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
#
# EVERY table the manuscript prints must be here. An omission is a sheet the
# co-author silently does not get, and there is nothing to flag it -- so this
# list is checked against the ft_* functions the library actually defines,
# below, rather than trusted.
TABLE_BUILDERS <- list(
  "Table 1"  = ft_table1,  "Table 2"  = ft_table2,
  "Table 3"  = ft_table3,  "Table 4"  = ft_table4,
  "Table A1" = ft_tableA1, "Table A2" = ft_tableA2, "Table A3" = ft_tableA3,
  "Table A4" = ft_tableA4, "Table A5" = ft_tableA5, "Table A6" = ft_tableA6,
  "Table A7" = ft_tableA7, "Table A8" = ft_tableA8, "Table A9" = ft_tableA9
)

# Coverage check: any exported ft_table*() the library defines but this list
# omits. Cheap, and it fails at the top of the run rather than after the builds.
.defined <- grep("^ft_table", ls(), value = TRUE)
.wired   <- vapply(TABLE_BUILDERS, function(f) {
  nm <- names(which(vapply(mget(.defined, inherits = TRUE), identical, logical(1), f)))
  if (length(nm)) nm[1] else NA_character_
}, character(1))
.missing <- setdiff(.defined, .wired)
if (length(.missing))
  stop("102: exhibit_helpers_tables.R defines table builders that are not in ",
       "TABLE_BUILDERS: ", paste(.missing, collapse = ", "),
       "\n  Add them, or the workbook silently omits a table the paper prints.",
       call. = FALSE)

# Build every table before writing any of it, so a failure cannot leave a
# workbook that silently omits one.
built <- list()
for (nm in names(TABLE_BUILDERS)) {
  message("  building ", nm, " ...")
  built[[nm]] <- TABLE_BUILDERS[[nm]]()
}

grids <- stats::setNames(
  lapply(names(built), function(nm) .ft_grid(built[[nm]], nm)), names(built))

out <- file.path(study_dir_tables(study_environment),
                 "resource_extraction_tables.xlsx")
openxlsx::write.xlsx(grids, file = out, colNames = FALSE, overwrite = TRUE)

message("Wrote ", out, "  (", length(grids), " sheets)")
invisible(TRUE)
