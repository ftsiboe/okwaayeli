# Changelog

## okwaayeli (development version)

### Documentation

- Major documentation overhaul:
  - Added project-level `pkgdown` site config (`_pkgdown.yml`) and a
    GitHub Actions workflow to publish it to GitHub Pages.
  - Added a methodology vignette (`vignettes/msf-and-matching.Rmd`)
    explaining the meta-stochastic-frontier and matching pipeline.
  - Added a harmonized-data codebook stub
    (`data-raw/releases/harmonized_data/codebook.md`) covering every
    released dataset.
  - Greatly expanded `CONTRIBUTING.md` with the standard study pipeline
    (001_DATA → 100_FIGTAB), “How to add a new study” instructions, and
    coding/style conventions.
  - Rewrote `SECURITY.md` with a real reporting process.
  - Added `inst/CITATION` so `citation("okwaayeli")` returns proper
    BibTeX.
  - Added GitHub issue templates (bug, study proposal, data request), a
    pull-request template, and a `CODEOWNERS` file.
  - Added `.gitattributes` to normalise line endings.
- Top-level README: added a table of contents, a pipeline-at-a-glance
  diagram, a citation block, and corrected stale references to the
  legacy `GHAgricProductivityLab` repository name.
- Per-study replication READMEs: filled in missing abstracts and removed
  empty `[here]()` working-paper links.

## okwaayeli 0.0.0.9000

- Initial development version of the package (renamed from
  `GHAgricProductivityLab`). Re-exports the meta-stochastic-frontier and
  statistical-matching engine used across the legacy studies. See README
  for the canonical list of published outputs.
