# article_helpers.R
# Article layer: repo-root-relative paths, formatting helpers, presence assertions.
#
# A LIBRARY, NOT A STEP: sourced by 301, 302 and narrative/ag-services.Rmd, so it
# has no position in a sequence and carries no number. See scripts/README.md.
#
# NB the paths below are REPO-ROOT-relative. knitr sets the working directory to
# narrative/ during a render, so anything sourced by the Rmd must resolve its own
# paths -- see .STUDY_ROOT in exhibit_helpers_tables.R. Do not reach for OUTPUT
# from a file the Rmd sources.

STUDY        <- "studies/ag_services"
DATA         <- file.path(STUDY, "data")
OUTPUT       <- file.path(STUDY, "output")
FIGURE       <- file.path(OUTPUT, "figures")   # v2 layout; see ?study_dirs
TABLES       <- file.path(OUTPUT, "tables")
NARRATIVE    <- file.path(STUDY, "narrative")
OBJECTS_JSON <- file.path(NARRATIVE, "article_objects.json")

# sprintf-based rounding so inline text matches the tables' sprintf("%.3f", ...)
# cells exactly (formatC and sprintf can disagree on halfway values, e.g.
# 0.0875 -> formatC "0.087" vs sprintf "0.088").
fmt_num <- function(x, digits = 2)
  formatC(as.numeric(sprintf(paste0("%.", digits, "f"), x)),
          format = "f", digits = digits, big.mark = ",")
fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_abs_pct <- function(x, digits = 1) paste0(formatC(abs(100 * x), format = "f", digits = digits), "%")

# Stop if any requested object is missing / NA (guards the knit against silent gaps).
assert_present <- function(x, name = deparse(substitute(x))) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    stop(sprintf("assert_present(): '%s' is missing or NA.", name), call. = FALSE)
  }
  invisible(x)
}

# =============================================================================
#  Placeholder markers, rendered in red
# =============================================================================
# The outline sections carry [[OUTLINE ...]], [[CITE: ...]] and [[WIRE: ...]]
# markers. They are deliberately plain text in the .Rmd so they stay greppable,
# and they are deliberately NOT hidden in comments, because a placeholder that
# does not print is a placeholder that ships.
#
# This colours them at render time instead of rewriting the sources. Nothing in
# sections/*.Rmd changes, so `grep -rn "\[\[OUTLINE" sections/` keeps working and
# a marker can still be converted to prose by deleting two brackets.
#
# FOUR THINGS THIS HAS TO GET RIGHT, each of which breaks it if missed.
#   1. `[[` also appears as ordinary R indexing inside inline code, for example
#      objs$heterogeneity$female[["1"]]$tgr in Section 5. Matching bare `[[`
#      would wrap live results in red. Only the keywords above are matched.
#   2. Markers nest: paragraph 3 of the introduction holds a [[CITE:]] inside an
#      [[OUTLINE]], closing with four brackets. A non-greedy match stops at the
#      inner close and leaves a stray `]]` in the document, so the scan counts
#      bracket depth rather than pattern-matching.
#   3. The same keywords appear inside HTML comments that document the
#      convention. Splicing a raw block into a comment corrupts it, so matches
#      inside <!-- --> are skipped.
#   4. The hook sees every marker TWICE. knitr fires the `document` hook once
#      per child AND again for the assembled parent, and the first pass leaves
#      the brackets sitting inside the raw run - deliberately, so a marker is
#      still greppable in the rendered file. The scan therefore has to be
#      idempotent, which means skipping anything already inside a raw-attribute
#      fence. Without that guard, pass two splices a new fence INSIDE an open
#      <w:t>, and officedown's post-processor dies on the malformed part with
#      "Opening and ending tag mismatch: t line N and body". Nothing upstream
#      reports a problem: pandoc completes and writes the .docx, and only
#      officer's read_xml() on word/document.xml finds the damage.
#
# ONE BEHAVIOUR CHANGE WORTH KNOWING. Marker text becomes a raw run, so citation
# keys written inside a marker are printed literally instead of being resolved
# and added to the reference list. That is the right outcome (a work named in an
# outline note has not been cited yet) but it does mean the bibliography can
# shrink the first time this is switched on.

.ph_xml_escape <- function(x)
  gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE),
                         fixed = TRUE), fixed = TRUE)

# Character spans the scan must not reach into: HTML comments, and raw
# attribute fences (```{=openxml} / ```{=html}) emitted by an earlier pass.
# Returns 1-based inclusive [from, to]; an unterminated region runs to the end.
.ph_protected <- function(s) {
  n <- nchar(s); from <- integer(0); to <- integer(0)

  cs <- gregexpr("<!--", s, fixed = TRUE)[[1L]]
  ce <- gregexpr("-->",  s, fixed = TRUE)[[1L]]
  if (cs[[1L]] != -1L) for (o in cs) {
    e    <- ce[ce > o]
    from <- c(from, o)
    to   <- c(to, if (length(e)) e[[1L]] + 2L else n)
  }

  # Fences are found line-wise, so `off` has to track character offsets, not
  # bytes: markers hold non-ASCII (the paragraph sign in the outline notes) and
  # gregexpr/substr elsewhere here are character-indexed. Keep them consistent.
  ln   <- strsplit(s, "\n", fixed = TRUE)[[1L]]
  off  <- cumsum(c(1L, nchar(ln) + 1L))
  open <- NA_integer_
  for (i in seq_along(ln)) {
    if (is.na(open)) {
      if (grepl("^[ \t]*`{3,}\\{=", ln[[i]])) open <- i
    } else if (grepl("^[ \t]*`{3,}[ \t]*$", ln[[i]])) {
      from <- c(from, off[[open]])
      to   <- c(to,   off[[i]] + nchar(ln[[i]]))
      open <- NA_integer_
    }
  }
  if (!is.na(open)) { from <- c(from, off[[open]]); to <- c(to, n) }

  list(from = from, to = to)
}

# Balanced [[ ]] blocks that open with one of `keys`, outside protected spans.
.ph_blocks <- function(s, keys) {
  st <- gregexpr(sprintf("\\[\\[(%s)", paste(keys, collapse = "|")), s)[[1L]]
  if (st[[1L]] == -1L) return(list())
  n    <- nchar(s)
  prot <- .ph_protected(s)
  # any(logical(0)) is FALSE, so this is also the no-protected-spans case.
  .guarded <- function(p) any(p >= prot$from & p <= prot$to)
  out <- list(); i <- 1L
  while (i <= length(st)) {
    p <- st[[i]]
    if (.guarded(p)) { i <- i + 1L; next }
    depth <- 0L; j <- p
    while (j <= n) {
      two <- substr(s, j, j + 1L)
      if (identical(two, "[[")) { depth <- depth + 1L; j <- j + 2L
      } else if (identical(two, "]]")) {
        depth <- depth - 1L; j <- j + 2L
        if (depth == 0L) break
      } else j <- j + 1L
    }
    if (depth != 0L)
      stop("placeholders_in_red(): unclosed marker starting at character ", p,
           ".\n  Every [[ needs a matching ]]; count them before rendering.",
           call. = FALSE)
    out[[length(out) + 1L]] <- c(p, j - 1L)
    i <- i + 1L
    while (i <= length(st) && st[[i]] < j) i <- i + 1L   # skip nested starts
  }
  out
}

.ph_paragraphs <- function(txt) {
  ps <- strsplit(txt, "\n[ \t]*\n")[[1L]]
  ps <- trimws(gsub("[ \t]*\n[ \t]*", " ", ps))
  ps[nzchar(ps)]
}

.ph_render <- function(txt, to, colour, italic) {
  ps <- .ph_paragraphs(txt)
  if (!length(ps)) return("")
  if (identical(to, "docx")) {
    runs <- vapply(ps, function(p) sprintf(
      paste0('<w:p><w:pPr><w:spacing w:after="80"/></w:pPr><w:r><w:rPr>',
             '<w:color w:val="%s"/>%s</w:rPr>',
             '<w:t xml:space="preserve">%s</w:t></w:r></w:p>'),
      colour, if (italic) "<w:i/>" else "", .ph_xml_escape(p)), character(1L))
    return(paste0("\n\n```{=openxml}\n", paste(runs, collapse = "\n"),
                  "\n```\n\n"))
  }
  sty <- sprintf("color:#%s;%s", colour,
                 if (italic) "font-style:italic;" else "")
  # Fenced as raw html rather than left as bare tags, purely so .ph_protected()
  # can see it. Pandoc treats the two identically; an unfenced <p> would be
  # re-matched on the parent pass exactly like the openxml branch was.
  paste0("\n\n```{=html}\n",
         paste(sprintf('<p style="%s">%s</p>', sty, .ph_xml_escape(ps)),
               collapse = "\n"),
         "\n```\n\n")
}

#' Print placeholder markers in red for the rest of this knit.
#'
#' Call once from the master document's setup chunk. Installs a knitr `document`
#' hook, so it sees the whole knitted document and needs no change to any
#' section file. Safe to call when there are no markers.
#'
#' @param keys Marker keywords to colour.
#' @param colour Hex colour without the leading hash. Word's standard dark red
#'   is C00000.
#' @param italic Italicise as well as colour.
#' @param verbose Report how many blocks were coloured.
placeholders_in_red <- function(keys = c("OUTLINE", "CITE", "WIRE", "TODO"),
                                colour = "C00000", italic = TRUE,
                                verbose = TRUE) {
  knitr::knit_hooks$set(document = function(x) {
    s  <- paste(x, collapse = "\n")
    bl <- .ph_blocks(s, keys)
    if (!length(bl)) {
      if (verbose) message("placeholders_in_red(): no markers found.")
      return(x)
    }
    to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (is.null(to)) to <- "docx"
    for (b in rev(bl)) {                       # back to front: offsets hold
      rep <- .ph_render(substr(s, b[[1L]], b[[2L]]), to, colour, italic)
      s <- paste0(substr(s, 1L, b[[1L]] - 1L), rep,
                  substr(s, b[[2L]] + 1L, nchar(s)))
    }
    if (verbose)
      message("placeholders_in_red(): ", length(bl),
              " placeholder block(s) rendered in red (#", colour, ", ", to, ").")
    strsplit(s, "\n", fixed = TRUE)[[1L]]
  })
  invisible(TRUE)
}
