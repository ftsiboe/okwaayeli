# Freeze the parity goldens from resource_extraction's Stata workbook.
# Run from the repo root:  source("tests/testthat/golden/_freeze.R")
#
# WHY resource_extraction: it is the only study whose Engine A sheets were never
# affected by the `mat roweq A = Female` bug (fixed repo-wide 2026-07-15). Every
# other study's means sheet has the OwnLnd0/OwnLnd1 (or disagCat0/1) prevalence
# rows mis-tagged Equ=="Female", colliding with the real Female outcome rows.
# Freezing those would enshrine the collision as the parity target.
#
# WHEN TO RE-FREEZE: only after re-running
# studies/resource_extraction/scripts/100_exhibits.do. Re-freezing to silence a
# failing test defeats the purpose -- the golden is the reference, not the output.

XL <- "studies/resource_extraction/output/resource_extraction_results.xlsx"
DIR <- "tests/testthat/golden"

stopifnot(file.exists(XL))
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("okwaayeli", quietly = TRUE)) devtools::load_all(".")

TREATMENTS <- c("extraction_any", "mining_any", "mining_comm", "mining_gala",
                "quarrying", "sand", "salt")

# Engine A: one sheet per treatment.
for (tr in TREATMENTS) {
  sheet <- paste0("Means_", tr)
  m <- okwaayeli::read_exhibit_sheet(XL, sheet)
  out <- file.path(DIR, paste0("re_means_", tr, ".csv"))
  data.table::fwrite(m, out)
  message(sprintf("%-28s -> %-40s %6d rows", sheet, basename(out), nrow(m)))
}

# Engine B.
e <- okwaayeli::read_exhibit_sheet(XL, "extraction")
data.table::fwrite(e, file.path(DIR, "resource_extraction_extraction.csv"))
message(sprintf("%-28s -> %-40s %6d rows", "extraction",
                "resource_extraction_extraction.csv", nrow(e)))

message("\nFrozen from: ", normalizePath(XL))
message("Workbook mtime: ", format(file.info(XL)$mtime))
