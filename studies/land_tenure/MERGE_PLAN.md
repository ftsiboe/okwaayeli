# Plan: Consolidate land_tenure study into `studies/land_tenure`

Study-specific application of `studies/consolidate_study_prompt.md`.
Two phases, both in scope: Phase 1 consolidates the folders; Phase 2
migrates the manuscript to the automated narrative setup.

- **STUDY**: `land_tenure`
- **STUDY_DIR**: `studies/land_tenure` (target)
- **MANUSCRIPT_DIR**: `manuscripts/land_tenure` (read-only source)
- **REFERENCE_DIR**: `studies/resource_extraction` (canonical layout)

**Mode:** COPY from MANUSCRIPT_DIR — nothing under `manuscripts/` is deleted
or modified. Moves happen only WITHIN `studies/land_tenure` (via `git mv`
where tracked).

## Target structure (mirrors resource_extraction)

```
studies/land_tenure/
├── README.md                  (stays)
├── land_tenure.Rmd            (stays)
├── scripts/                   (NEW — moved from root)
├── data/                      (NEW — moved from output/)
├── literature/                (NEW — copied)
├── narrative/                 (NEW — Phase 1: copied Word drafts;
│                               Phase 2: full automated article system)
│   └── old/
├── output/                    (stays as is)
└── submissions/               (NEW — copied)
    └── conferences/
```

## Phase 1: consolidation procedure

0. **Inventory** — full recursive listings of STUDY_DIR, MANUSCRIPT_DIR, and
   REFERENCE_DIR; verify the mapping below against the actual files (the
   reference layout is the run-time source of truth, not this document).
   **Stop and get user approval of the final mapping before executing.**
1. **scripts/** — move all code files currently at STUDY_DIR root:
   the numbered pipeline scripts (DATA, MATCHING, TREATMENT, MSF), the
   exhibits scripts (.R and .do), and the sbatch job file. No renumbering.
2. **literature/** — copy every PDF from `manuscripts/land_tenure/literature/`.
3. **submissions/conferences/** — copy `manuscripts/land_tenure/conference/`,
   including the `gaae2025/` versions subfolder.
4. **narrative/** — copy from `manuscripts/land_tenure/writeup/`:
   the current draft (`v001_LandTenureAgricProdGapGhana.docx`) and the
   outline; `writeup/old/` → `narrative/old/`.
5. **data/** — move `output/land_tenure_study_environment.rds` here.
6. **output/** — unchanged (estimations, figure, figure_data, matching,
   treatment_effects, results workbook).
7. **Path fixes** — scan moved scripts for relative paths assuming the old
   layout (scripts assume the repo root as working directory) and update.
8. **Verify** — compare file counts/sizes source vs. destination; confirm
   `manuscripts/land_tenure` is byte-for-byte unchanged.
9. **Report** — what was moved, copied, excluded, flagged, and any path
   edits made; then proceed to Phase 2.

## Hard constraints

- **MANUSCRIPT_DIR is read-only** — copy from it; never delete, move, or
  modify anything under `manuscripts/`.
- **No silent overwrites** — if a destination file already exists, report
  the conflict instead of replacing it.
- Moves use `git mv` where files are tracked, to preserve history.

## Explicitly excluded

- `writeup/sample/` — reference drafts from other studies; not copied.
- `output/disability_study_data.dta` — appears to belong to another study;
  left in place and flagged for review.
- System files (`desktop.ini`) — not copied.
- Everything under `manuscripts/land_tenure` — kept intact, untouched.

## Phase 2 (in scope): migrate to the automated narrative setup

The study currently lives in Word drafts (`v001_LandTenureAgricProdGapGhana.docx`).
Migration means rebuilding it in the reproducible article system used by
resource_extraction, so manuscript text, cited numbers, and exhibits all
derive from the same estimation objects and cannot drift apart.

### A. Scaffold machinery (copy from REFERENCE_DIR, adapt, empty of content)

1. `narrative/csl/` — copy both citation styles (elsevier-harvard, ieee).
2. `narrative/css/tables.css` — copy for html output.
3. `narrative/reference.docx` — copy the Word style template.
4. `narrative/references.bib` — create empty; populate in step C.
5. `narrative/diagnostics/` — create empty folder.
6. `scripts/300_article_helpers.R` — copy and adapt: set
   `STUDY <- "studies/land_tenure"`; keep the path constants (DATA, OUTPUT,
   EXHIBITS, NARRATIVE, OBJECTS_JSON), formatting helpers (`fmt_num`,
   `fmt_pct`, ...), and `assert_present()` knit guards.
7. `scripts/301_article_objects.R` — copy the skeleton, strip
   resource_extraction extraction logic. Rebuild against land_tenure
   estimation objects (`output/estimations/*.rds`,
   `data/land_tenure_study_environment.rds`, `te_summary.rds`): pull every
   number the text will cite (sample sizes, elasticities, returns-to-scale
   delta, TGR/TE means by tenure form, matching diagnostics) and write
   `narrative/article_objects.json`. Key it to the same objects
   `100_exhibits.R` uses so text and exhibits share one
   source. Document the frontier/sample keying in header comments, as
   resource_extraction does.
8. `scripts/302_render_article.R` — copy and repoint to
   `narrative/land-tenure.Rmd`; keep officedown (landscape exhibits) and
   the `ARTICLE_CSL` env-var style switch.
9. `scripts/305_tables.R` — copy the flextable builder pattern; rebuild
   table functions from `output/land_tenure_results.xlsx` /
   `output/figure_data/`.
10. `scripts/run_article.R` — copy and repoint: helpers → objects → render,
    with the citation-style toggle at top.

### B. Master Rmd and sections

1. `narrative/land-tenure.Rmd` — master file modeled on the reference:
   YAML with `officedown::rdocx_document` (reference_docx, fig sizes),
   `html_document` (toc, css), `bibliography: references.bib`,
   `csl:` read from `ARTICLE_CSL`; setup chunk loads
   `article_objects.json` and sources `../scripts/305_tables.R`; body is
   a sequence of `child=` includes of `sections/`.
2. `narrative/sections/` — split the v001 draft into prefix-numbered
   child Rmds matching the reference set: abstract, introduction, data,
   methods, results, discussion, conclusion, declarations, references,
   tables_and_figures, appendix. (Add/drop sections to fit the paper —
   e.g. no `state_of_extraction` analogue unless a tenure-context section
   is wanted.)
3. Content migration: transfer text from
   `v001_LandTenureAgricProdGapGhana.docx` section by section; replace
   every hard-typed number with an inline `objs$...` reference guarded by
   `assert_present()`; replace figure/table references with the builders
   from 305 and images from `output/figure/`.

### C. References

1. Extract the bibliography from the v001 draft into
   `narrative/references.bib` (BibTeX).
2. Convert in-text citations to pandoc `[@key]` syntax.
3. Cross-check every entry against the PDFs in `literature/`.

### D. Verify

1. `run_article.R` knits clean to .docx and .html with zero
   `assert_present()` failures.
2. Diff rendered .docx against the v001 draft: all numbers match the
   estimation objects (where they differ, the objects win — flag the
   discrepancy in `narrative/diagnostics/`).
3. Both citation styles render.
4. Word draft lineage stays in `narrative/old/`; from then on the Rmd
   system is the single source of the manuscript.
