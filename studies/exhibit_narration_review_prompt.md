# Agent prompt: Developmental review of exhibit narration

Copy this prompt to an agent, filling in the inputs. Contains no study-specific
file names, exhibit numbers or counts — the agent must discover everything by
inventorying the study.

---

## Inputs

- **STUDY**: `<study_name>`
- **STUDY_DIR**: `studies/<study_name>`
- **SECTIONS_DIR**: `STUDY_DIR/narrative/sections` — the prose, split into prefix-numbered child Rmds
- **EXHIBIT_SECTIONS**: the tables-and-figures and appendix child Rmds inside SECTIONS_DIR — the authoritative exhibit list
- **RENDERED**: `STUDY_DIR/narrative/<study>.html` (and `.docx`) — what a reader actually receives
- **FIGURE_DATA**: `STUDY_DIR/output/figures` — `.csv`/`.rds` saved beside each `.png`, if the study follows the figure-data persistence convention

## Task

Judge whether the manuscript discusses every figure and table adequately and in
an engaging way. Deliver a diagnosis first; apply changes only after approval.

This is a **narration** review. The exhibits are final: never propose changing a
figure, a table, an estimate, or anything upstream of them, and never re-run the
pipeline. The only artefacts you may edit are the prose child Rmds — and only
after the user approves a listed set of changes.

## Scope boundary

Read the data behind the exhibits, but review only the writing about them. When
the data contradicts the prose, that is a narration finding: the sentence is
wrong, not the estimate. Say so, propose the corrected sentence, and let the
user confirm before you change it.

## Procedure

1. **Inventory from EXHIBIT_SECTIONS, not from the render.** Those files are the
   definitive list — every `####` heading with an `ft_*()` chunk or an
   `![](...)` include. List main-text and supplementary exhibits separately, and
   include prose-only appendix items (notes, boxes): they are exhibits for this
   purpose and are the ones most often left empty.

2. **Build the data-side view.** For each figure, read its persisted `.csv`/
   `.rds` and reduce it to the shape a reader sees: the ranked values per metric,
   which are significant, which are extreme, which reverse sign. For each table,
   read the builder's source (usually the estimation objects) or the rendered
   table itself. This step is where the real findings come from — a review that
   only reads prose against prose cannot tell whether the prose is true. **If a
   study does not persist figure data, say so explicitly in the report** and
   review those figures from the rendered image and prose alone, marking the
   coverage judgements as unverified.

3. **Audit cross-references programmatically.** Regex every `Table N` / `Figure
   N` / `Note N` mention across the prose child Rmds and diff against the
   inventory. Anything never named is orphaned.

4. **Read the rendered output.** Some defects exist only after knitting: a
   heading with no body, a percentage printed unscaled, an exhibit whose builder
   silently emitted a table of dashes. Strip tags from RENDERED and read the
   passages that narrate each exhibit as a reader receives them.

5. **Assess each exhibit** on the axes below.

6. **Deliver the review** in the format below. Stop there.

7. **On approval, list every proposed change for sign-off before editing** —
   file, location, current text, replacement, and which findings each resolves.
   Ask about anything you inferred rather than confirmed.

8. **Apply, then verify**: re-run the cross-reference audit (orphan count should
   be zero), and parse-check every inline `` `r ... ` `` expression in the
   manuscript. Hand the re-render to the user; never claim you tested a knit you
   did not run.

## Assessment axes

- **Discussed at all?** — yes / partial / no. Named in prose, and actually
  explained rather than merely cited.
- **Notable patterns covered?** — compare the prose against the data-side view
  from step 2. Flag the biggest contrast, the outlier, the sign reversal, the
  monotone gradient, the one non-significant cell in an otherwise significant
  row — anything visible in the exhibit and absent from the text.
- **Depth** — does the text interpret what the pattern means and why it matters,
  or restate the number? Flag purely descriptive narration.
- **Engagement** — flag flat, mechanical framings ("The following table
  presents…") and propose openers that lead with the finding. Name the strong
  passages too, so the author knows what to preserve.
- **Placement and signposting** — exhibits discussed far from where they appear,
  referenced by the wrong number, or introduced before the reader is told why to
  care. **Check what each section heading claims about the content beneath it**:
  a main result filed under a "robustness" heading is mis-signposted, and the
  paper's most distinctive finding is the one most likely to be buried this way.

Additionally, always check:

- **Promises.** Every forward reference ("discussed in the appendix", "reported
  below") must be delivered. Grep each one to its destination.
- **Self-consistency.** Where the paper restricts its own scope — a subsample, a
  comparable window, a set of rounds — verify the surrounding prose honours that
  restriction. A carefully argued limitation violated two paragraphs later costs
  more credibility than never stating it.
- **Sign conventions.** Establish each exhibit's direction empirically: find the
  assignment under which the prose is true, and check every other claim in the
  same passage against it. State the convention where it is missing.

## Deliverable format

An inventory list, then a table keyed by exhibit — exhibit → discussed?
(yes/partial/no) → notable patterns missed → depth & engagement verdict →
specific suggestion — then a short prioritized summary leading with the exhibits
that most need attention. **Quote the relevant sentences** so the author can
find them. Diagnose and suggest; do not rewrite whole sections in the review.

Separate what you **confirmed** from what needs the author to **verify**. A
finding resting on an inferred sign convention is not the same claim as an empty
heading, and conflating them costs the reader trust in both.

## Pitfalls — check for each

- **`Table 1` does not match "Tables 1 and 2".** A naive fixed-string grep
  reports exhibits as orphaned when they are cited in a group, and misses range
  citations entirely. Match `Tables?\s+S?\d+`, expand `and`/comma lists, and
  expand `S1--S4`-style ranges before concluding anything is orphaned. Exclude
  commented-out lines and the exhibit-section headings themselves.

- **A group citation is not individual coverage.** Exhibits cited only as a
  block ("Tables S1–S4") leave the reader unable to tell which table holds
  which result. Flag it, even though the audit shows them referenced.

- **An empty heading renders as a heading.** A section stub with a `TODO`
  comment and no body produces a title followed immediately by the next
  exhibit. It will not appear in any reference audit, because the problem is
  absence of content, not absence of citation. Check the render for headings
  with nothing under them.

- **Proportions printed with a literal `%`.** Where the house pattern is
  `fmt_num(100 * x, d)` followed by `%`, a call missing the `100 *` renders
  "0.69%" for 68.5% — plausible enough to survive proofreading, and it will
  contradict the sentence's own conclusion. Grep for the formatter immediately
  followed by a literal `%` and check each one carries its scaling.

- **A sign error survives a rewrite when only one clause is wrong.** Where a
  paragraph makes several directional claims from one exhibit, test every claim
  against a single convention. The one that fails is usually a finding carried
  over from an earlier, unmatched table. Do not flip it on inference alone: ask.

- **"Robustness checks confirm our results" is a claim, not a caption.** Read
  the robustness data before accepting it. Count sign flips and compare the
  extreme specifications against the main estimate. Where one dimension
  dominates the sensitivity, that is a finding worth reporting honestly — a
  result robust to four dimensions and fragile in the fifth is more informative
  than a blanket claim of consistency, and far more defensible in review.

- **The richest exhibits get the thinnest coverage.** A panel with dozens of
  cells is harder to narrate than a two-column table, so it tends to receive
  three sentences of assertion. Count roughly how many cells the prose actually
  touches, and check whether a structure recurring across panels — the same
  trade-off appearing by crop and again by region — is named as a pattern or
  left as a list. Naming it usually converts a list into the paper's argument.

- **Do not hard-code numbers into prose you write.** Where the study's rule is
  that every measured quantity renders live, added text must use the study's
  lookup helpers or cite the exhibit without a figure. Before promising live
  numbers, check the table is registered in the lookup switch — an unregistered
  table falls through to a stored-CSV read that may not exist. A qualitative
  note citing the exhibit is the safe default and needs no code change.

- **Verify file freshness before trusting a copy.** On cloud-synced or mounted
  repositories, a staged snapshot can lag the file on disk — with the stale copy
  reporting the *new* size in the transfer log. Compare byte counts, and read
  through the shell on the machine that owns the file when they disagree.
  Reviewing a stale render means reviewing defects the author already fixed.

- **Distinguish an author's deliberate silence from an omission.** A pattern
  excluded for a stated methodological reason is handled, not missed — but if it
  is the most visible feature of the exhibit, the reader still needs one
  sentence saying what it is and why it is not interpreted. Recommend the
  sentence, not the interpretation.

- **Statistical significance is part of the pattern.** A subgroup that is the
  only non-significant cell in its row, or an exhibit where every cell is
  significant, is worth a clause. Do not describe an effect as absent when it is
  merely imprecisely estimated — say which, and check whether the standard
  errors support the null the prose asserts.

## Hard constraints

- **The exhibits are final.** No changes to figures, tables, estimates, builders,
  or any pipeline stage. If a defect lies upstream, report it and stop.
- **No edits before approval.** Deliver the review, list the changes, wait.
- **Prose files only**, unless the user explicitly approves a helper-library
  change — and then flag it as the one item requiring a re-render to verify.
- **Leave a marked comment, not a silent fix**, wherever you inferred a
  convention rather than confirming it.
- Report what you could not verify as plainly as what you could.
