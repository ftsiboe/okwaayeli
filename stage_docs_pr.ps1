# stage_docs_pr.ps1
#
# Stages the "docs/improve-github-documentation" branch with the files
# Claude added/modified, leaving any other in-flight working-tree changes
# in place on `main`.
#
# Run from PowerShell at the repo root:
#     cd "C:\Users\ftsib\Dropbox (Personal)\GitHub\labs\okwaayeli"
#     powershell -ExecutionPolicy Bypass -File .\stage_docs_pr.ps1
#
# After it finishes, open the printed PR URL in your browser to file the PR.

$ErrorActionPreference = "Stop"

# 1. Sanity check
git status --short | Out-Null
$branch = git rev-parse --abbrev-ref HEAD
Write-Host "Current branch: $branch"

# 2. Stash everything that isn't mine so the branch is clean.
Write-Host ""
Write-Host "Stashing your in-flight changes as 'pre-docs-pr-WIP'..."
git stash push --include-untracked -m "pre-docs-pr-WIP"

# 3. Create the docs branch from current HEAD.
Write-Host ""
Write-Host "Creating branch docs/improve-github-documentation..."
git checkout -B docs/improve-github-documentation

# 4. Restore Claude's docs files from the stash (without touching your other edits).
#    The stash holds *all* working-tree changes; we apply it and then keep
#    only the doc files; everything else gets reset.
git stash apply --index

# These are the paths Claude touched. We keep exactly these.
$keep = @(
    ".github/ISSUE_TEMPLATE/config.yml",
    ".github/ISSUE_TEMPLATE/bug_report.yml",
    ".github/ISSUE_TEMPLATE/study_proposal.yml",
    ".github/ISSUE_TEMPLATE/data_request.yml",
    ".github/pull_request_template.md",
    ".github/CODEOWNERS",
    ".github/workflows/pkgdown.yaml",
    "CONTRIBUTING.md",
    "SECURITY.md",
    "inst/CITATION",
    "NEWS.md",
    "_pkgdown.yml",
    "vignettes/msf-and-matching.Rmd",
    "data-raw/releases/harmonized_data/codebook.md",
    "README.md",
    "README.Rmd",
    ".gitignore",
    ".gitattributes",
    "replications/legacy_studies/README.md",
    "replications/income_transfer/README.md"
)

# Plus the per-study README pairs.
$studies = "ag_services","disability","education","financial_inclusion","input_dealers","land_tenure","resource_extraction","societal_peace_and_cohesion","time_poverty"
$rmds   = @{
    "ag_services"="ag_services.Rmd"; "disability"="disability_study.Rmd";
    "education"="education.Rmd"; "financial_inclusion"="financial_inclusion.Rmd";
    "input_dealers"="input_dealers.Rmd"; "land_tenure"="land_tenure.Rmd";
    "resource_extraction"="resource_extraction.Rmd";
    "societal_peace_and_cohesion"="societal_peace_and_cohesion.Rmd";
    "time_poverty"="time_poverty_study.Rmd"
}
foreach ($s in $studies) {
    $keep += "replications/$s/README.md"
    $keep += "replications/$s/$($rmds[$s])"
}

# 5. Reset every tracked file EXCEPT the ones in $keep back to HEAD.
$tracked = git ls-files | ForEach-Object { $_ }
foreach ($f in $tracked) {
    if ($keep -notcontains $f) {
        git checkout HEAD -- $f 2>$null
    }
}

# 6. Untracked files Claude created (need to be added).
$newFiles = @(
    ".github/ISSUE_TEMPLATE/config.yml",
    ".github/ISSUE_TEMPLATE/bug_report.yml",
    ".github/ISSUE_TEMPLATE/study_proposal.yml",
    ".github/ISSUE_TEMPLATE/data_request.yml",
    ".github/pull_request_template.md",
    ".github/CODEOWNERS",
    ".github/workflows/pkgdown.yaml",
    "CONTRIBUTING.md",
    "inst/CITATION",
    "NEWS.md",
    "_pkgdown.yml",
    "vignettes/msf-and-matching.Rmd",
    "data-raw/releases/harmonized_data/codebook.md",
    ".gitattributes",
    "replications/income_transfer/README.md"
)

# 7. Stage everything.
git add $keep $newFiles 2>$null
git add stage_docs_pr.ps1 2>$null  # the script itself (optional)

# 8. Commit.
$msg = @"
docs: large documentation refresh

- Add pkgdown site + GitHub Pages workflow.
- Add inst/CITATION so citation('okwaayeli') returns BibTeX.
- Add NEWS.md and start tracking documentation changes.
- Add methodology vignette (vignettes/msf-and-matching.Rmd) walking
  through the 001 -> 100 pipeline.
- Add harmonized-data codebook stub.
- Expand CONTRIBUTING with the standard study pipeline, 'how to add a
  new study' instructions, and coding/style conventions.
- Rewrite SECURITY.md with a real disclosure process.
- Add GitHub issue templates (bug, study proposal, data request), a
  pull-request template, and CODEOWNERS.
- Add .gitattributes to normalise line endings.
- Add ~`$*, *.tmp, docs/, *.Rcheck/ to .gitignore; allow the harmonized
  data README and codebook through.
- Top-level README: TOC, at-a-glance table, Mermaid pipeline diagram,
  citation block, status tags per study, install + quickstart, fix
  stale GHAgricProductivityLab links.
- Replication READMEs (8 active studies + legacy_studies +
  income_transfer): correct titles (several were copy-paste of the
  wrong paper), status badges, contributors, abstracts, JEL codes,
  keywords; fix empty [here]() working-paper links; clean up the
  garbled '## ## Outputs' in legacy_studies.
"@

git commit -m $msg

# 9. Push.
Write-Host ""
Write-Host "Pushing branch to origin..."
git push -u origin docs/improve-github-documentation

# 10. Restore your prior in-flight work on main.
Write-Host ""
Write-Host "Switching back to main and re-applying your stashed in-flight changes..."
git checkout main
git stash pop

# 11. Print the PR URL.
$repo = (git config --get remote.origin.url) -replace '\.git$',''
$url  = "$repo/compare/main...docs/improve-github-documentation?expand=1"
Write-Host ""
Write-Host "Done. Open the PR here:"
Write-Host "  $url"
