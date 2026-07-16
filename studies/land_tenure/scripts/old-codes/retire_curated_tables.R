# retire_curated_tables.R  --  one-shot cleanup, NOT part of the pipeline.
#
# Removes what the 2026-07-16 migration orphaned, now that every exhibit except
# Table S0 is built from the pipeline:
#
#   1. data/tables/*.csv          the v001 draft's frozen table values
#   2. output/land_tenure_results.xlsx   the Stata/Excel workbook
#   3. output/figure/, output/figure_data/   the pre-v2 layout's empty folders
#
# RUN AFTER A CLEAN RENDER. 102_exhibit_table_workbook.R already proves every
# ft_*() builds -- it called all twelve -- but the render is where the ~90
# tbl_num() calls in the Rmd actually execute. Render, then run this.
#
#   source("studies/land_tenure/scripts/old-codes/retire_curated_tables.R")
#
# Everything here is in git. If a claim in this file turns out to be wrong, the
# files come back with `git checkout`.
#
# WHAT SURVIVES, and why:
#
#   tableS0.csv   Curated by design and permanently. The GLSS construction table
#                 -- question wording transcribed from the instruments, response
#                 options, per-wave mappings. Not computable from any object. A
#                 genuine input, not a cached output.
#
#   R/exhibits-workbook.R    NOT deleted, despite being test-only now. It is the
#                 parity harness: tests/testthat/golden/_freeze.R uses
#                 read_exhibit_sheet() to regenerate the goldens behind the
#                 14,405-assertion resource_extraction test and the 678-assertion
#                 land test. Delete it and the evidence that the R engine
#                 reproduces Stata can never be re-derived.
#
#   data-raw/scripts/figures_and_tables.R   Six sibling studies still source() it.

STUDY  <- "studies/land_tenure"
TABLES <- file.path(STUDY, "data", "tables")
OUTPUT <- file.path(STUDY, "output")

# ---- 1. Orphaned CSVs --------------------------------------------------------

# Orphaned by the descriptive engine (100_exhibit_descriptive_stats.R ->
# .LIVE_IDS -> .tbl1_live / .tbl2_live / .tblS_live).
descriptive <- c("table1.csv", "table1_header.csv",
                 "table2.csv", "table2_header.csv",
                 "tableS1.csv", "tableS1_header.csv",
                 "tableS2.csv", "tableS2_header.csv",
                 "tableS3.csv", "tableS3_header.csv",
                 "tableS4.csv", "tableS4_header.csv")

# Orphaned when the tryCatch fallbacks came out of ft_table3()/ft_table4() and
# table4 joined .LIVE_IDS -- which cut the last reader of table4.csv, the 27
# tbl_num("table4.csv", ...) calls in sections 1, 5 and 6.
frontier <- c("table3.csv", "table3_header.csv",
              "table4.csv", "table4_header.csv")

# Orphaned when S5/S6/S7 went live off sf_estm / ef_mean.
supplementary <- c("tableS5.csv", "tableS5_header.csv",
                   "tableS6.csv", "tableS6_header.csv",
                   "tableS7.csv", "tableS7_header.csv")

csvs <- c(descriptive, frontier, supplementary)

# Refuse to run if anything still reads one. Cheap insurance against deleting a
# file some Rmd picked up since this list was written.
src <- unlist(lapply(c(file.path(STUDY, "scripts"), file.path(STUDY, "narrative")),
                     list.files, pattern = "\\.(R|Rmd)$",
                     recursive = TRUE, full.names = TRUE))
src <- src[!grepl("old-codes", src)]
txt <- unlist(lapply(src, readLines, warn = FALSE))

still_read <- Filter(function(f) {
  # Builders read via .read_tbl("x.csv"). Prose reads via tbl_num("x.csv", ...),
  # but that goes through .live_table() and never touches disk for a .LIVE_IDS
  # id -- so a tbl_num() hit is not evidence the file is needed. Only flag
  # .read_tbl / .read_hdr / .ft_csv.
  any(grepl(paste0("(\\.ft_csv|\\.read_tbl|\\.read_hdr)\\(\\s*\"", f, "\""), txt))
}, csvs)

if (length(still_read))
  stop("retire_curated_tables.R: these are still read by a builder:\n  ",
       paste(still_read, collapse = "\n  "),
       "\nFix the reader before deleting the file.", call. = FALSE)

p <- file.path(TABLES, csvs)
p <- p[file.exists(p)]
if (length(p)) {
  message("Deleting ", length(p), " orphaned CSVs from ", TABLES, " ...")
  file.remove(p)
} else {
  message("data/tables/: already retired.")
}

# ---- 2. The workbook ---------------------------------------------------------
# Written by nothing since the openxlsx round trips came out of 101 (they were
# write-only: the display sheets that consumed them cache #N/A). Read by nothing
# since Tables 1 and 2 went live.
xlsx <- file.path(OUTPUT, "land_tenure_results.xlsx")
if (file.exists(xlsx)) {
  message("Deleting the Stata/Excel workbook: ", xlsx,
          " (", round(file.size(xlsx) / 1e6, 1), " MB)")
  file.remove(xlsx)
}

# ---- 3. Pre-v2 layout folders ------------------------------------------------
# study_dirs(layout = "v2") no longer creates these; figures and their data now
# share output/figures/. Delete only if empty -- a non-empty one means something
# is still writing to the old names, which is worth knowing rather than erasing.
for (d in file.path(OUTPUT, c("figure", "figure_data"))) {
  if (!dir.exists(d)) next
  left <- list.files(d, all.files = TRUE, no.. = TRUE)
  if (length(left)) {
    warning("retire_curated_tables.R: ", d, " is NOT empty (", length(left),
            " file(s)). Something still writes to the pre-v2 layout. Left in ",
            "place.", call. = FALSE, immediate. = TRUE)
  } else {
    message("Removing empty pre-v2 folder: ", d)
    unlink(d, recursive = TRUE)
  }
}

# ---- Report ------------------------------------------------------------------
left <- list.files(TABLES)
message("\nDone.")
message("data/tables/ now holds ", length(left), " file(s): ",
        paste(left, collapse = ", "))
if (!identical(left, "tableS0.csv"))
  warning("Expected exactly tableS0.csv to remain. Check the list above.",
          call. = FALSE)
invisible(TRUE)
