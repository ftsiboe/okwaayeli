# 102_exhibit_table_workbook.R  (10x = compute/emit; see scripts/README.md)
# Emit every manuscript table AS PRINTED to output/tables/land_tenure_tables.xlsx,
# one sheet per table -- a deliverable for co-authors and supplementary material.
#
# "As printed" means the flextable objects the Rmd renders, not the data behind
# them: stars, jackknife SEs in parentheses, sprintf rounding, "-" placeholders,
# headers, spanners and footnotes. Same ft_*() call as the paper, so the two
# cannot disagree.
#
# NOT A ROUND TRIP -- nothing reads this back. Excel as an output is fine; Excel
# in the middle of the pipeline is what 101 was cleared of.
#
# Run from the repo root, AFTER 100 (descriptive cache) and 101 (figure data:
# fig1_range() and trend_gap() read output/figures/).

tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){
  rm(list = ls(all = TRUE)); gc()
})

devtools::document()

project_name <- "land_tenure"
study_environment <- readRDS(
  file.path(paste0("studies/", project_name, "/data"),
            paste0(project_name, "_study_environment.rds")))
study_environment <- study_dirs(study_environment, layout = "v2")

# The builders. Sourced, not attached: exhibit_helpers_tables.R is a library and
# resolves its own paths via .STUDY_ROOT, so it does not care that we are at the
# repo root here and at narrative/ during a knit.
source("studies/land_tenure/scripts/exhibit_helpers_tables.R")

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
# NB not `TABLES`: article_helpers.R defines that as the output/tables path.
TABLE_BUILDERS <- list(
  "Table 1"  = ft_table1,  "Table 2"  = ft_table2,
  "Table 3"  = ft_table3,  "Table 4"  = ft_table4,
  "Table S0" = ft_tableS0, "Table S1" = ft_tableS1, "Table S2" = ft_tableS2,
  "Table S3" = ft_tableS3, "Table S4" = ft_tableS4, "Table S5" = ft_tableS5,
  "Table S6" = ft_tableS6, "Table S7" = ft_tableS7
)

# Build every table before writing any of it. A partial workbook that silently
# omits the table that failed is exactly the failure mode the Table 3 fallback
# used to produce.
built <- list()
for (nm in names(TABLE_BUILDERS)) {
  message("  building ", nm, " ...")
  built[[nm]] <- TABLE_BUILDERS[[nm]]()
}

grids <- stats::setNames(
  lapply(names(built), function(nm) .ft_grid(built[[nm]], nm)), names(built))

out <- file.path(study_dir_tables(study_environment), "land_tenure_tables.xlsx")
openxlsx::write.xlsx(grids, file = out, colNames = FALSE, overwrite = TRUE)

message("Wrote ", out, "  (", length(grids), " sheets)")
invisible(TRUE)
