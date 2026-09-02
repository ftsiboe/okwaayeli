# exhibits-figures-modern001.R ------------------------------------------------
#
# A reusable figure system for okwaayeli studies. ADDITIVE: nothing here
# replaces or modifies the existing fig_*() builders in exhibits-figures.R, and
# no existing study changes behaviour because this file exists. A study opts in
# by calling these builders instead of the old ones.
#
# WHY "modern001". The suffix is a style generation, not a version number. If a
# later study wants a different look, it gets exhibits-figures-modern002.R with
# its own fig_modern002_*() namespace, and both remain callable from the same
# package. That is the point of tagging rather than overwriting: two studies
# submitted to two journals can carry two looks without either being broken to
# make room for the other.
#
# THE ONE RULE THAT MAKES THE SET COHERENT. Colour encodes WHICH MEASURE a value
# belongs to, and nothing else. It never encodes whether a number is good or
# bad, large or small, or above or below zero. Sign is carried by position
# against the reference line; statistical significance is carried by whether a
# marker is filled or hollow. A reader who learns three colours once can read
# every figure in a paper built on this file, including one they have not seen.
# Any builder added here later must obey that rule or it does not belong here.
#
# THE PALETTE IS VALIDATED, NOT CHOSEN. See fig_modern001_check() for the
# recorded report and its provenance. Do not substitute hues by eye: the obvious
# warm/cool pair for this kind of work (a clay red against a teal) fails the
# colourblind separation gate outright, at a normal-vision distance of 8.7
# against a floor of 15.
#
# LAYERS. Two of them, deliberately.
#   Primitives  style, palette, theme, scales, reference rule, mark spec,
#               significance test, number formatting, save. Use these to compose
#               a figure this file does not already build.
#   Builders    decomposition, slope, dumbbell, grid, balance, speccurve. Each
#               takes a tidy data frame and returns a ggplot. Use these first.
#
# EVERY BUILDER IS COLUMN-AGNOSTIC. Column names are arguments, defaulting to
# the repository's own conventions (Estimate, Estimate.sd, jack_pv). A study
# with different names passes them; no study needs to rename its data to suit
# this file.

utils::globalVariables(c(".est", ".se", ".p", ".sig", ".grp", ".msr", ".blk",
                         ".lo", ".hi", ".x", ".y", ".lab", ".xend", ".yend",
                         ".a", ".b", ".on", ".idx"))

# =============================================================================
#  Primitives
# =============================================================================

#' Style constants for the modern001 figure system
#'
#' Single source of truth for every colour, size and weight the modern001
#' builders use. Change a value here and every figure in every study built on
#' this system changes with it, which is the entire reason the constants are not
#' inlined into the builders.
#'
#' The three categorical hues are positional slots, not semantic ones. A study
#' maps its own measures onto them in order, so a study with three measures that
#' are not technology, efficiency and their product still gets a coherent
#' figure set.
#'
#' @return A named list with elements `hue` (three validated categorical hues),
#'   `ink`, `secondary`, `muted`, `grid`, `surface`, and the numeric mark
#'   constants `point_size`, `point_stroke`, `line_width`, `rule_width`,
#'   `grid_width`.
#' @family modern001 figure helpers
#' @export
#' @examples
#' fig_modern001_style()$hue
fig_modern001_style <- function() {
  list(
    hue         = c(slot1 = "#eb6834", slot2 = "#2a78d6", slot3 = "#4a3aa7"),
    ink         = "#14140F",
    secondary   = "#4A4A44",
    muted       = "#7C7C74",
    grid        = "#E4E2DC",
    surface     = "#FCFCFB",
    point_size  = 2.6,
    point_stroke= 1.1,
    line_width  = 0.7,
    rule_width  = 0.5,
    grid_width  = 0.3
  )
}

#' Categorical palette for the modern001 figure system
#'
#' Returns the validated hues, optionally named for the measures they will
#' encode. Slots are assigned in order and are never cycled: a fourth measure is
#' not given a generated fourth hue, because a generated hue is indistinguishable
#' from an existing one under colour vision deficiency. Ask for more than three
#' and this errors rather than silently producing an unreadable figure.
#'
#' @param n Number of slots required. Maximum 3.
#' @param labels Optional character vector of measure labels, used to name the
#'   returned vector so it can be passed straight to a manual scale.
#' @return A named character vector of hex colours.
#' @family modern001 figure helpers
#' @export
#' @examples
#' fig_modern001_palette(3, c("Technology gap ratio", "Technical efficiency",
#'                            "Net performance"))
fig_modern001_palette <- function(n = 3, labels = NULL) {
  hue <- fig_modern001_style()$hue
  if (!is.numeric(n) || length(n) != 1L || n < 1L)
    stop("fig_modern001_palette(): n must be a single positive integer.",
         call. = FALSE)
  if (n > length(hue))
    stop("fig_modern001_palette(): the validated palette holds ", length(hue),
         " slots and ", n, " were requested.\n",
         "  Hues are never generated to fill a gap. Fold the tail into an ",
         "'Other' level, facet into small multiples, or split the figure.",
         call. = FALSE)
  out <- unname(hue[seq_len(n)])
  if (!is.null(labels)) {
    if (length(labels) != n)
      stop("fig_modern001_palette(): labels must have length n.", call. = FALSE)
    names(out) <- labels
  }
  out
}

#' Report the palette validation, and where it came from
#'
#' The modern001 hues were not selected by eye. They were run through a
#' colourblind-separation validator and this function records the result, so a
#' co-author or referee asking "are these accessible" gets an answer rather than
#' an assurance. Nothing is recomputed here; the report is the recorded output of
#' the check that gated the palette into the file.
#'
#' @param quiet If `TRUE`, return the report invisibly instead of printing it.
#' @return A list with the hues, the recorded check results and the provenance
#'   note, returned invisibly when printed.
#' @family modern001 figure helpers
#' @export
fig_modern001_check <- function(quiet = FALSE) {
  rep <- list(
    hues = fig_modern001_style()$hue,
    surface = fig_modern001_style()$surface,
    checks = c(
      "lightness band"      = "PASS  all 3 inside L 0.43-0.77",
      "chroma floor"        = "PASS  all 3 at or above 0.1",
      "CVD separation"      = "PASS  worst all-pairs slot3 vs slot2, dE 13.0 deutan, 17.4 tritan",
      "normal-vision floor" = "PASS  worst all-pairs slot3 vs slot2, dE 16.3, floor 15",
      "contrast vs surface" = "PASS  all 3 at or above 3:1"),
    pairlist = "all-pairs, the strict list required by dot and small-multiple forms",
    greyscale = paste("The three hues sit at different lightness, so they",
                      "separate on a monochrome printer. Filled against hollow",
                      "carries significance and position carries sign, so",
                      "neither depends on colour."),
    rejected = paste("A clay red against a teal was tested first and failed:",
                     "normal-vision separation 8.7 against a floor of 15, and",
                     "both below the chroma floor. Do not reintroduce it."))
  if (!quiet) {
    cat("modern001 palette, surface ", rep$surface, "\n", sep = "")
    for (i in seq_along(rep$hues))
      cat("  ", names(rep$hues)[i], "  ", rep$hues[i], "\n", sep = "")
    cat("\n")
    for (i in seq_along(rep$checks))
      cat("  ", formatC(names(rep$checks)[i], width = -20), rep$checks[i],
          "\n", sep = "")
    cat("\n  pairlist: ", rep$pairlist, "\n", sep = "")
  }
  invisible(rep)
}

#' Theme for the modern001 figure system
#'
#' A print theme: no panel border, no panel fill, hairline grid on one axis at
#' most, and text in ink tokens rather than series colours. The existing
#' `ers_theme()` is untouched and remains available; this is a separate look, not
#' a replacement.
#'
#' @param base_size Base font size in points. 10 suits a single-column journal
#'   figure at 6 inches wide; 8 suits a two-panel supplementary exhibit.
#' @param grid One of `"none"`, `"x"` or `"y"`. Which axis gets hairline
#'   gridlines. Solid, never dashed: dashing reads as a projection or a
#'   threshold when it is only a grid.
#' @param legend Legend position, passed to `ggplot2::theme()`.
#' @param facet_bg Draw a filled strip behind facet labels. Defaults to `FALSE`,
#'   which is what makes small multiples read as one figure rather than as
#'   several boxed ones.
#' @return A `ggplot2` theme object.
#' @family modern001 figure helpers
#' @export
fig_modern001_theme <- function(base_size = 10, grid = c("none", "x", "y"),
                                legend = "top", facet_bg = FALSE) {
  grid <- match.arg(grid)
  s <- fig_modern001_style()
  gl <- ggplot2::element_line(colour = s$grid, linewidth = s$grid_width)
  th <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text              = ggplot2::element_text(colour = s$secondary),
      plot.title        = ggplot2::element_text(colour = s$ink, size = base_size * 1.15,
                                                face = "plain", hjust = 0),
      plot.subtitle     = ggplot2::element_text(colour = s$muted, size = base_size * 0.95, hjust = 0),
      plot.caption      = ggplot2::element_text(colour = s$muted, size = base_size * 0.85,
                                                hjust = 0, face = "italic"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text         = ggplot2::element_text(colour = s$muted, size = base_size * 0.9),
      axis.text.y       = ggplot2::element_text(colour = s$ink, hjust = 0),
      axis.title        = ggplot2::element_text(colour = s$muted, size = base_size * 0.9),
      axis.ticks        = ggplot2::element_blank(),
      panel.border      = ggplot2::element_blank(),
      panel.background  = ggplot2::element_blank(),
      plot.background   = ggplot2::element_rect(fill = s$surface, colour = NA),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major.x = if (grid == "x") gl else ggplot2::element_blank(),
      panel.grid.major.y = if (grid == "y") gl else ggplot2::element_blank(),
      legend.position   = legend,
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(colour = s$secondary, size = base_size * 0.95),
      legend.key        = ggplot2::element_blank(),
      legend.justification = if (identical(legend, "top")) "left" else "center",
      strip.text        = ggplot2::element_text(colour = s$ink, size = base_size * 0.95,
                                                hjust = 0, face = "plain"),
      # Group headers. Three things have to be set together or a switched strip
      # renders rotated and tucked between the axis labels and the panel, which
      # reads as a stray caption rather than a heading:
      #   placement outside  puts it beyond the axis text, where a heading belongs
      #   angle 0            keeps it horizontal and therefore readable
      #   hjust 0            aligns it to the left edge of its group
      # Bold, because it is a heading and the rows beneath it are not.
      strip.placement   = "outside",
      strip.text.y.left = ggplot2::element_text(colour = s$ink, size = base_size * 0.95,
                                                angle = 0, hjust = 0, vjust = 1,
                                                face = "bold",
                                                margin = ggplot2::margin(r = 6)),
      panel.spacing     = ggplot2::unit(1.1, "lines"))
  if (facet_bg)
    th <- th + ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = s$grid, colour = NA))
  th
}

#' Colour and fill scales bound to the modern001 palette
#'
#' Both scales are returned together and both carry the same `limits`, so a
#' measure keeps its hue even when a subset of the data does not contain it.
#' That is what stops a filtered figure from repainting its survivors, which is
#' the single most misleading thing a chart can do to a reader who has already
#' learned the legend.
#'
#' @param measures Character vector of measure labels, in the order they should
#'   take palette slots. Maximum 3.
#' @param guide Passed to the colour scale. The fill scale is always hidden,
#'   since colour and fill encode the same thing and two legends for one
#'   variable is noise.
#' @return A list of two `ggplot2` scales, addable to a plot.
#' @family modern001 figure helpers
#' @export
fig_modern001_scales <- function(measures, guide = "legend") {
  pal <- fig_modern001_palette(length(measures), measures)
  list(
    ggplot2::scale_colour_manual(values = pal, limits = measures,
                                 drop = FALSE, guide = guide),
    ggplot2::scale_fill_manual(values = pal, limits = measures,
                               drop = FALSE, guide = "none"))
}

#' Is an estimate distinguishable from zero
#'
#' Prefers an explicit p-value when one is supplied and falls back to a normal
#' approximation from the standard error. Returns `FALSE` where the inputs do not
#' support a judgement, so a marker with unknown significance is drawn hollow.
#' Erring toward hollow is deliberate: a hollow marker understates confidence,
#' and understating it is the cheaper error.
#'
#' @param estimate Numeric vector of point estimates.
#' @param se Numeric vector of standard errors, or `NULL`.
#' @param p Numeric vector of p-values, or `NULL`. Takes precedence over `se`.
#' @param level Significance level. Default 0.05.
#' @return A logical vector, `TRUE` where the estimate is distinguishable from
#'   zero at `level`.
#' @family modern001 figure helpers
#' @export
#' @examples
#' fig_modern001_significant(c(-0.088, 0.010), se = c(0.0089, 0.0109))
fig_modern001_significant <- function(estimate, se = NULL, p = NULL,
                                      level = 0.05) {
  n <- length(estimate)
  if (!is.null(p)) {
    out <- !is.na(p) & p < level
    if (length(out) == n) return(out)
  }
  if (!is.null(se)) {
    z <- stats::qnorm(1 - level / 2)
    return(!is.na(estimate) & !is.na(se) & se > 0 & abs(estimate / se) > z)
  }
  rep(FALSE, n)
}

#' Format a number for a figure label
#'
#' Uses a true minus sign rather than a hyphen, because a hyphen at label size
#' reads as a dash and is easy to lose. Set `minus = "hyphen"` where a downstream
#' font lacks the glyph.
#'
#' @param x Numeric vector.
#' @param digits Decimal places.
#' @param signed Prefix positive values with a plus sign. Useful when a figure
#'   reports differences and the sign is the point.
#' @param pct Multiply by 100 and append a percent sign.
#' @param minus One of `"true"` (U+2212) or `"hyphen"`.
#' @return A character vector.
#' @family modern001 figure helpers
#' @export
fig_modern001_fmt <- function(x, digits = 3, signed = TRUE, pct = FALSE,
                              minus = c("true", "hyphen")) {
  minus <- match.arg(minus)
  v <- if (pct) x * 100 else x
  out <- formatC(v, format = "f", digits = digits,
                 flag = if (signed) "+" else "")
  if (pct) out <- paste0(out, "%")
  if (minus == "true") out <- gsub("-", "−", out, fixed = TRUE)
  out
}

#' The reference rule at zero
#'
#' One solid hairline in ink, drawn once. Every modern001 builder that reports a
#' difference includes it, because a difference figure without a visible zero
#' asks the reader to locate the only value that matters.
#'
#' @param orientation `"v"` for a vertical rule (values on x) or `"h"` for a
#'   horizontal one (values on y).
#' @param at Where to draw it. Default 0.
#' @return A `ggplot2` layer.
#' @family modern001 figure helpers
#' @export
fig_modern001_reference <- function(orientation = c("v", "h"), at = 0) {
  orientation <- match.arg(orientation)
  s <- fig_modern001_style()
  if (orientation == "v")
    ggplot2::geom_vline(xintercept = at, colour = s$ink, linewidth = s$rule_width)
  else
    ggplot2::geom_hline(yintercept = at, colour = s$ink, linewidth = s$rule_width)
}

#' The modern001 mark specification for point estimates
#'
#' Two layers, drawn in one call: hollow markers for estimates not
#' distinguishable from zero, filled markers for those that are. Both carry a
#' surface-coloured ring so that overlapping markers stay separable without a
#' border being drawn around them.
#'
#' Expects the plot's data to carry a logical column `.sig` and the global
#' mapping to set both `colour` and `fill` to the measure. Both are satisfied
#' automatically when the data comes from `fig_modern001_prepare()`.
#'
#' @param size Marker radius in ggplot point units.
#' @param stroke Outline weight.
#' @return A list of two `ggplot2` layers.
#' @family modern001 figure helpers
#' @export
fig_modern001_points <- function(size = NULL, stroke = NULL) {
  s <- fig_modern001_style()
  size <- if (is.null(size)) s$point_size else size
  stroke <- if (is.null(stroke)) s$point_stroke else stroke
  list(
    ggplot2::geom_point(data = function(d) d[!d$.sig, , drop = FALSE],
                        shape = 21, fill = s$surface, size = size,
                        stroke = stroke),
    ggplot2::geom_point(data = function(d) d[d$.sig, , drop = FALSE],
                        shape = 21, size = size, stroke = stroke))
}

#' Normalise a study's estimate table for the modern001 builders
#'
#' Renames whichever columns a study happens to use into the fixed internal
#' names every builder expects, computes the interval and the significance flag,
#' and fixes the factor order of the measures so palette slots are assigned in
#' the order given rather than alphabetically.
#'
#' Call this directly only when composing a figure by hand from the primitives.
#' The builders call it themselves.
#'
#' @param data A data frame of estimates.
#' @param estimate,se,p Column names holding the point estimate, its standard
#'   error and its p-value. `p` may be `NULL`.
#' @param group Column name holding the row category, for example the treatment
#'   arm, the survey round or the crop.
#' @param measure Column name holding the measure. `NULL` for a single-measure
#'   figure, in which case one slot is used.
#' @param block Optional column name holding a panel grouping, used by
#'   `fig_modern001_grid()`.
#' @param measures Optional character vector fixing the order of measure levels.
#'   Defaults to order of appearance.
#' @param groups Optional character vector fixing the order of group levels.
#'   Defaults to order of appearance. Use `order_by` instead to sort by value.
#' @param order_by Optional measure label. When supplied, groups are ordered by
#'   that measure's estimate, which is how a ranked figure is produced without
#'   the caller reshaping anything.
#' @param level Significance level for the filled or hollow decision.
#' @param ci Interval multiplier. Default 1.96, a 95 percent normal interval.
#'   Note that the older `fig_*()` builders in this package draw plus or minus
#'   one standard error; this system draws a 95 percent interval, which is what
#'   a reader assumes an error bar means.
#' @param scale Multiply estimates and errors by this before plotting. Use 100
#'   to report percentage points.
#' @return A data frame carrying `.est`, `.se`, `.p`, `.sig`, `.lo`, `.hi`,
#'   `.grp`, `.msr` and, where requested, `.blk`.
#' @family modern001 figure helpers
#' @export
fig_modern001_prepare <- function(data, estimate = "Estimate",
                                  se = "Estimate.sd", p = "jack_pv",
                                  group = NULL, measure = NULL, block = NULL,
                                  measures = NULL, groups = NULL,
                                  order_by = NULL, level = 0.05, ci = 1.96,
                                  scale = 1) {
  if (!is.data.frame(data) || !nrow(data))
    stop("fig_modern001_prepare(): data must be a non-empty data frame.",
         call. = FALSE)
  need <- c(estimate = estimate, group = group, measure = measure, block = block)
  need <- need[!vapply(need, is.null, logical(1))]
  miss <- setdiff(unlist(need), names(data))
  if (length(miss))
    stop("fig_modern001_prepare(): column(s) not found in data: ",
         paste(miss, collapse = ", "),
         "\n  available: ", paste(names(data), collapse = ", "), call. = FALSE)

  d <- data.frame(.est = as.numeric(data[[estimate]]) * scale,
                  stringsAsFactors = FALSE)
  d$.se <- if (!is.null(se) && se %in% names(data))
    as.numeric(data[[se]]) * scale else NA_real_
  d$.p  <- if (!is.null(p) && p %in% names(data))
    as.numeric(data[[p]]) else NA_real_
  d$.grp <- if (is.null(group)) "" else as.character(data[[group]])
  d$.msr <- if (is.null(measure)) "Estimate" else as.character(data[[measure]])
  if (!is.null(block)) d$.blk <- as.character(data[[block]])

  d$.sig <- fig_modern001_significant(d$.est, d$.se,
                                      if (all(is.na(d$.p))) NULL else d$.p,
                                      level = level)
  d$.lo <- d$.est - ci * d$.se
  d$.hi <- d$.est + ci * d$.se

  lv <- if (is.null(measures)) unique(d$.msr) else measures
  if (length(setdiff(unique(d$.msr), lv)))
    stop("fig_modern001_prepare(): measures does not cover every level present: ",
         paste(setdiff(unique(d$.msr), lv), collapse = ", "), call. = FALSE)
  d$.msr <- factor(d$.msr, levels = lv)

  if (!is.null(order_by)) {
    if (!order_by %in% lv)
      stop("fig_modern001_prepare(): order_by '", order_by,
           "' is not one of the measures.", call. = FALSE)
    key <- d[d$.msr %in% order_by, c(".grp", ".est")]
    key <- key[order(key$.est), ]
    d$.grp <- factor(d$.grp, levels = unique(key$.grp))
  } else {
    d$.grp <- factor(d$.grp, levels = if (is.null(groups)) unique(d$.grp) else groups)
  }
  if (!is.null(block)) d$.blk <- factor(d$.blk, levels = unique(d$.blk))
  d
}

#' Save a modern001 figure at consistent dimensions
#'
#' A thin wrapper over `ggplot2::ggsave()` that fixes the resolution and the
#' background so that figures from different studies, saved on different days,
#' still look like one set when they land in a manuscript.
#'
#' @param plot A ggplot object.
#' @param file Output path.
#' @param width,height Inches. Defaults suit a single-column journal figure.
#' @param dpi Resolution. 600 matches the existing exhibit scripts.
#' @return The path, invisibly.
#' @family modern001 figure helpers
#' @export
fig_modern001_save <- function(plot, file, width = 6.5, height = 4.6,
                               dpi = 600) {
  ggplot2::ggsave(filename = file, plot = plot, width = width, height = height,
                  dpi = dpi, bg = fig_modern001_style()$surface)
  invisible(file)
}

# =============================================================================
#  Builders
# =============================================================================

#' Decomposition figure: several measures, several groups, one scale
#'
#' The form that replaces a table of estimates whose point is that two of its
#' columns carry opposite signs. Groups run down the y axis, measures are offset
#' within each group, and every value sits on one common x scale so the
#' comparison is made by the eye rather than by arithmetic.
#'
#' Use it wherever a study reports the same set of measures across several
#' treatment arms, sources, or subsamples.
#'
#' @param data A tidy data frame, one row per group and measure.
#' @param estimate,se,p,group,measure Column names. See
#'   `fig_modern001_prepare()`.
#' @param measures Character vector fixing measure order and therefore palette
#'   slots. Maximum 3.
#' @param groups Character vector fixing group order.
#' @param label Print the estimate beside each marker. Defaults to `TRUE`, which
#'   is right when this figure replaces a table: the values are the reason a
#'   referee trusts the picture. Set `FALSE` where the figure is illustrative and
#'   a table carries the numbers.
#' @param digits Decimal places on the labels.
#' @param x_title Axis title. The default states the sign convention, which is
#'   the one thing a reader must know before reading anything else.
#' @param base_size Base font size.
#' @param ci Interval multiplier.
#' @param scale Multiplier applied to estimates.
#' @return A ggplot object.
#' @family modern001 figure helpers
#' @export
fig_modern001_decomposition <- function(data, estimate = "Estimate",
                                        se = "Estimate.sd", p = "jack_pv",
                                        group = "group", measure = "measure",
                                        measures = NULL, groups = NULL,
                                        label = TRUE, digits = 3,
                                        x_title = "Treated minus untreated",
                                        base_size = 10, ci = 1.96, scale = 1) {
  d <- fig_modern001_prepare(data, estimate, se, p, group, measure,
                             measures = measures, groups = groups,
                             ci = ci, scale = scale)
  s <- fig_modern001_style()
  lv <- levels(d$.msr)
  d$.y <- as.numeric(d$.grp) + (as.numeric(d$.msr) - (length(lv) + 1) / 2) * 0.22

  g <- ggplot2::ggplot(d, ggplot2::aes(x = .est, y = .y, colour = .msr,
                                       fill = .msr)) +
    fig_modern001_reference("v") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .lo, xmax = .hi), height = 0,
                            linewidth = s$line_width, na.rm = TRUE) +
    fig_modern001_points() +
    fig_modern001_scales(lv) +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(d$.grp)),
                                labels = levels(d$.grp),
                                expand = ggplot2::expansion(add = 0.55)) +
    ggplot2::labs(x = x_title, y = NULL) +
    fig_modern001_theme(base_size = base_size, grid = "x")

  if (label) {
    d$.lab <- fig_modern001_fmt(d$.est, digits = digits)
    g <- g + ggplot2::geom_text(
      data = d, ggplot2::aes(label = .lab),
      hjust = ifelse(d$.est >= 0, -0.35, 1.35), colour = s$secondary,
      size = base_size * 0.26, show.legend = FALSE, na.rm = TRUE) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.14))
  }
  g
}

#' Slope figure: the same measures followed across an ordered sequence
#'
#' For survey rounds, years, or any ordered sequence where the question is which
#' measures are moving and which are not. Lines rather than grouped bars: a bar
#' chart makes each period a separate comparison and hides the trajectory, which
#' is usually the entire finding.
#'
#' @param data A tidy data frame, one row per point and measure.
#' @param estimate,se,p,measure Column names.
#' @param time Column name holding the ordered sequence.
#' @param measures Character vector fixing measure order.
#' @param times Character vector fixing the sequence order. Defaults to order of
#'   appearance, which is rarely what you want for factors read from disk.
#' @param label_last Print the final value on each line. Selective labelling: a
#'   value on every point is unreadable and goes unread.
#' @param digits Decimal places.
#' @param y_title Axis title.
#' @param base_size Base font size.
#' @param ci Interval multiplier.
#' @param scale Multiplier applied to estimates.
#' @return A ggplot object.
#' @family modern001 figure helpers
#' @export
fig_modern001_slope <- function(data, estimate = "Estimate",
                                se = "Estimate.sd", p = "jack_pv",
                                time = "Survey", measure = "measure",
                                measures = NULL, times = NULL,
                                label_last = TRUE, digits = 3,
                                y_title = "Treated minus untreated",
                                base_size = 10, ci = 1.96, scale = 1) {
  d <- fig_modern001_prepare(data, estimate, se, p, group = time,
                             measure = measure, measures = measures,
                             groups = times, ci = ci, scale = scale)
  s <- fig_modern001_style()
  lv <- levels(d$.msr)
  d$.x <- as.numeric(d$.grp)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .est, colour = .msr,
                                       fill = .msr, group = .msr)) +
    fig_modern001_reference("h") +
    ggplot2::geom_line(linewidth = s$line_width, na.rm = TRUE) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .lo, ymax = .hi),
                            linewidth = s$line_width, alpha = 0.55,
                            na.rm = TRUE) +
    fig_modern001_points() +
    fig_modern001_scales(lv) +
    ggplot2::scale_x_continuous(breaks = seq_along(levels(d$.grp)),
                                labels = levels(d$.grp),
                                expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::labs(x = NULL, y = y_title) +
    fig_modern001_theme(base_size = base_size, grid = "y")

  if (label_last) {
    last <- d[d$.x == max(d$.x, na.rm = TRUE), , drop = FALSE]
    last$.lab <- fig_modern001_fmt(last$.est, digits = digits)
    g <- g + ggplot2::geom_text(data = last, ggplot2::aes(label = .lab),
                                hjust = -0.35, colour = s$secondary,
                                size = base_size * 0.26, show.legend = FALSE,
                                na.rm = TRUE)
  }
  g
}

#' Dumbbell figure: two measures per category, and the distance between them
#'
#' The right form when the finding is that one measure varies across categories
#' while the other does not. A connector makes the gap itself the mark, and an
#' optional shaded band marks the span of the measure that is meant to look
#' constant, so the comparison is visible rather than arithmetic.
#'
#' @param data A tidy data frame, one row per category and measure. Exactly two
#'   measures.
#' @param estimate,se,p,group,measure Column names.
#' @param measures Character vector of length 2 fixing measure order.
#' @param order_by Measure label to sort categories by. `NULL` keeps input order.
#' @param band Measure label whose span should be shaded, or `NULL`. Use it for
#'   the measure whose constancy is the point.
#' @param third Optional column name holding a further value printed as a right
#'   hand text column, for example a net or combined measure that would clutter
#'   the plot if drawn.
#' @param third_title Header for that column.
#' @param digits Decimal places.
#' @param x_title Axis title.
#' @param base_size Base font size.
#' @param scale Multiplier applied to estimates.
#' @return A ggplot object.
#' @family modern001 figure helpers
#' @export
fig_modern001_dumbbell <- function(data, estimate = "Estimate",
                                   se = "Estimate.sd", p = "jack_pv",
                                   group = "group", measure = "measure",
                                   measures = NULL, order_by = NULL,
                                   band = NULL, third = NULL,
                                   third_title = NULL, digits = 3,
                                   x_title = "Treated minus untreated",
                                   base_size = 10, scale = 1) {
  d <- fig_modern001_prepare(data, estimate, se, p, group, measure,
                             measures = measures, order_by = order_by,
                             scale = scale)
  lv <- levels(d$.msr)
  if (length(lv) != 2L)
    stop("fig_modern001_dumbbell(): expects exactly two measures, got ",
         length(lv), ". Use fig_modern001_decomposition() for three.",
         call. = FALSE)
  s <- fig_modern001_style()

  wide <- stats::reshape(d[, c(".grp", ".msr", ".est")], idvar = ".grp",
                         timevar = ".msr", direction = "wide")
  names(wide) <- c(".grp", ".a", ".b")

  g <- ggplot2::ggplot()
  if (!is.null(band)) {
    bv <- d$.est[d$.msr %in% band]
    g <- g + ggplot2::annotate("rect", xmin = min(bv, na.rm = TRUE),
                               xmax = max(bv, na.rm = TRUE), ymin = -Inf,
                               ymax = Inf,
                               fill = fig_modern001_palette(2, lv)[[which(lv == band)]],
                               alpha = 0.08)
  }
  g <- g +
    fig_modern001_reference("v") +
    ggplot2::geom_segment(data = wide,
                          ggplot2::aes(x = .a, xend = .b, y = .grp, yend = .grp),
                          colour = s$muted, linewidth = s$line_width,
                          alpha = 0.55, na.rm = TRUE) +
    ggplot2::geom_point(data = d,
                        ggplot2::aes(x = .est, y = .grp, colour = .msr,
                                     fill = .msr),
                        shape = 21, size = s$point_size * 1.15,
                        stroke = s$point_stroke, na.rm = TRUE) +
    fig_modern001_scales(lv) +
    ggplot2::labs(x = x_title, y = NULL) +
    fig_modern001_theme(base_size = base_size, grid = "x")

  if (!is.null(third) && third %in% names(data)) {
    tv <- unique(data.frame(.grp = as.character(data[[group]]),
                            .lab = fig_modern001_fmt(as.numeric(data[[third]]) * scale,
                                                     digits = digits),
                            stringsAsFactors = FALSE))
    tv$.grp <- factor(tv$.grp, levels = levels(d$.grp))
    tv <- tv[!is.na(tv$.grp), , drop = FALSE]
    g <- g + ggplot2::geom_text(data = tv,
                                ggplot2::aes(x = Inf, y = .grp, label = .lab),
                                hjust = -0.25, colour = s$secondary,
                                size = base_size * 0.26, na.rm = TRUE) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::unit(c(6, 46, 6, 6), "pt"))
    if (!is.null(third_title))
      g <- g + ggplot2::labs(caption = third_title)
  }
  g
}

#' Small-multiple grid: categories down, measures across
#'
#' For heterogeneity, where the categories are too many for colour to carry
#' identity. Identity moves to the row label and colour is freed to encode the
#' measure, one per column. Past roughly seven categories this is the honest
#' form; adding hues is not.
#'
#' @param data A tidy data frame, one row per category and measure.
#' @param estimate,se,p,group,measure Column names.
#' @param block Optional column name grouping categories into labelled panels,
#'   for example gender, age and schooling in one figure.
#' @param measures Character vector fixing measure order and column order.
#' @param order_by Measure label to sort categories by within each block.
#' @param free_y Give each block only its own categories. Almost always `TRUE`
#'   when `block` is used.
#' @param strip_position Where the block heading sits. `"left"` places it
#'   outside the axis, horizontal, left-aligned and bold, alongside its rows.
#'   `"top"` stacks the blocks and puts the heading above each one, which suits
#'   long category labels that would otherwise push a left heading far off the
#'   plot. `"top"` needs cowplot and falls back to `"left"` with a message if it
#'   is unavailable.
#' @param x_title Axis title.
#' @param base_size Base font size.
#' @param ci Interval multiplier. Set to `0` to draw points alone, which is
#'   right when the underlying estimates carry no standard error.
#' @param scale Multiplier applied to estimates.
#' @return A ggplot object.
#' @family modern001 figure helpers
#' @export
fig_modern001_grid <- function(data, estimate = "Estimate",
                               se = "Estimate.sd", p = "jack_pv",
                               group = "group", measure = "measure",
                               block = NULL, measures = NULL, order_by = NULL,
                               free_y = TRUE,
                               strip_position = c("left", "top"),
                               x_title = "Treated minus untreated",
                               base_size = 9, ci = 1.96, scale = 1) {
  strip_position <- match.arg(strip_position)
  d <- fig_modern001_prepare(data, estimate, se, p, group, measure, block,
                             measures = measures, order_by = order_by,
                             ci = ci, scale = scale)
  s <- fig_modern001_style()
  lv <- levels(d$.msr)

  # The panel body, shared by both strip positions so the two layouts cannot
  # drift apart in anything other than where the heading sits.
  .body <- function(dd, xlab = x_title) {
    g <- ggplot2::ggplot(dd, ggplot2::aes(x = .est, y = .grp, colour = .msr,
                                          fill = .msr)) +
      fig_modern001_reference("v")
    if (ci > 0)
      g <- g + ggplot2::geom_errorbarh(ggplot2::aes(xmin = .lo, xmax = .hi),
                                       height = 0, linewidth = s$line_width,
                                       alpha = 0.6, na.rm = TRUE)
    g + fig_modern001_points(size = s$point_size * 0.85) +
      fig_modern001_scales(lv, guide = "none") +
      ggplot2::labs(x = xlab, y = NULL) +
      fig_modern001_theme(base_size = base_size, grid = "x", legend = "none")
  }

  if (is.null(block))
    return(.body(d) + ggplot2::facet_wrap(~ .msr, nrow = 1))

  if (strip_position == "top" &&
      !requireNamespace("cowplot", quietly = TRUE)) {
    message("fig_modern001_grid(): strip_position = 'top' needs cowplot, which ",
            "is not installed. Falling back to 'left'.")
    strip_position <- "left"
  }

  if (strip_position == "left")
    return(.body(d) +
             ggplot2::facet_grid(.blk ~ .msr,
                                 scales = if (free_y) "free_y" else "fixed",
                                 space  = if (free_y) "free_y" else "fixed",
                                 switch = "y"))

  # strip_position == "top". One sub-plot per block, stacked, each headed by its
  # own bold left-aligned title. The measure strips print once at the top and the
  # x axis once at the bottom, so the stack reads as a single figure rather than
  # as several charts that happen to share a page.
  blks <- levels(d$.blk)
  rows <- vapply(blks, function(b)
    length(unique(as.character(d$.grp[d$.blk %in% b]))), integer(1))
  plots <- lapply(seq_along(blks), function(i) {
    dd <- droplevels(d[d$.blk %in% blks[[i]], , drop = FALSE])
    pp <- .body(dd, xlab = if (i == length(blks)) x_title else NULL) +
      ggplot2::facet_wrap(~ .msr, nrow = 1) +
      ggplot2::ggtitle(blks[[i]]) +
      ggplot2::theme(plot.title = ggplot2::element_text(
        face = "bold", hjust = 0, colour = s$ink, size = base_size * 1.0,
        margin = ggplot2::margin(b = 4)))
    if (i > 1L)
      pp <- pp + ggplot2::theme(strip.text = ggplot2::element_blank())
    if (i < length(blks))
      pp <- pp + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    pp
  })
  cowplot::plot_grid(plotlist = plots, ncol = 1, align = "v", axis = "lr",
                     rel_heights = unname(rows) + 1.4)
}

#' Covariate balance figure, in the Love plot form
#'
#' The standard exhibit for a matched design, and much easier to audit than a
#' variance chart: one row per covariate, hollow marker before matching, filled
#' marker after, and a shaded band at the conventional threshold so the only
#' question a referee is asking gets answered without reading a number.
#'
#' @param data A data frame, one row per covariate.
#' @param covariate,before,after Column names holding the covariate label and
#'   the absolute standardized mean difference in each sample.
#' @param threshold Where to draw the tolerance band. Default 0.1.
#' @param order_by One of `"before"`, `"after"` or `"none"`.
#' @param labels Length-2 character vector naming the two samples.
#' @param x_title Axis title.
#' @param base_size Base font size.
#' @return A ggplot object.
#' @family modern001 figure helpers
#' @export
fig_modern001_balance <- function(data, covariate = "covariate",
                                  before = "before", after = "after",
                                  threshold = 0.1,
                                  order_by = c("before", "after", "none"),
                                  ncol = 1,
                                  labels = c("Before matching", "After matching"),
                                  x_title = "Absolute standardized mean difference",
                                  base_size = 9) {
  order_by <- match.arg(order_by)
  miss <- setdiff(c(covariate, before, after), names(data))
  if (length(miss))
    stop("fig_modern001_balance(): column(s) not found: ",
         paste(miss, collapse = ", "), call. = FALSE)
  s <- fig_modern001_style()
  d <- data.frame(.grp = as.character(data[[covariate]]),
                  .a = abs(as.numeric(data[[before]])),
                  .b = abs(as.numeric(data[[after]])),
                  stringsAsFactors = FALSE)
  ord <- switch(order_by, before = order(d$.a), after = order(d$.b),
                none = seq_len(nrow(d)))
  d$.grp <- factor(d$.grp, levels = d$.grp[ord])
  hue <- fig_modern001_palette(2)[2]

  if (ncol > 1L) {
    lev <- levels(d$.grp)
    per <- ceiling(length(lev) / ncol)
    idx <- match(as.character(d$.grp), lev)
    d$.blk <- factor(ceiling((length(lev) - idx + 1L) / per))
  }

  g <- ggplot2::ggplot(d) +
    ggplot2::annotate("rect", xmin = 0, xmax = threshold, ymin = -Inf,
                      ymax = Inf, fill = hue, alpha = 0.07) +
    ggplot2::geom_segment(ggplot2::aes(x = .a, xend = .b, y = .grp, yend = .grp),
                          colour = s$muted, linewidth = s$line_width,
                          alpha = 0.45, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = .a, y = .grp, shape = labels[1]),
                        colour = s$muted, fill = s$surface,
                        size = s$point_size, stroke = s$point_stroke,
                        na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = .b, y = .grp, shape = labels[2]),
                        colour = hue, fill = hue, size = s$point_size,
                        stroke = s$point_stroke, na.rm = TRUE) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(21, 21), labels),
                                limits = labels) +
    ggplot2::guides(shape = ggplot2::guide_legend(
      override.aes = list(colour = c(s$muted, hue),
                          fill = c(s$surface, hue)))) +
    ggplot2::labs(x = x_title, y = NULL) +
    fig_modern001_theme(base_size = base_size, grid = "x")

  if (ncol > 1L)
    g <- g + ggplot2::facet_wrap(~ .blk, ncol = ncol, scales = "free_y") +
      ggplot2::theme(strip.text = ggplot2::element_blank())
  g
}

#' Specification curve
#'
#' Every specification a robustness arm estimates, sorted by result, over a
#' panel showing which analytical choices produced each one. It answers the only
#' question a robustness exhibit should: does the finding survive the whole space
#' of defensible choices, and how many estimates cross zero.
#'
#' Returns the two panels separately as well as an assembled plot, so a caller
#' who wants to restyle one panel can.
#'
#' @param estimates A data frame, one row per specification.
#' @param estimate,se,p Column names in `estimates`.
#' @param id Column name holding the specification identifier, used to join to
#'   `choices`.
#' @param choices Optional long data frame with one row per specification and
#'   active choice, holding `id`, a `choice` label and optionally a `family`
#'   grouping. `NULL` returns the upper panel alone.
#' @param strip_position Where the family heading sits, on the same terms as
#'   `fig_modern001_grid()`. `"left"` places it outside the axis, horizontal,
#'   left-aligned and bold. `"top"` gives each family its own sub-panel headed
#'   above, stacked under the curve. Both keep the lower panel column-aligned
#'   with the curve, because every panel is drawn on one explicit shared x scale
#'   rather than on whatever range its own rows happen to span.
#' @param choice,family Column names within `choices`.
#' @param reference Optional value to mark with a horizontal rule, typically the
#'   preferred specification's estimate.
#' @param reference_label Label for that rule.
#' @param y_title Axis title for the upper panel.
#' @param base_size Base font size.
#' @param ci Interval multiplier.
#' @param heights Relative heights of the two panels.
#' @return A list with `estimates`, `choices` and `plot`. `choices` is a single
#'   ggplot under `strip_position = "left"` and a named list of one ggplot per
#'   family under `"top"`, so either can be restyled before assembly. `plot` is
#'   assembled with `cowplot` when it is available and is `NULL` otherwise.
#' @family modern001 figure helpers
#' @export
fig_modern001_speccurve <- function(estimates, estimate = "Estimate",
                                    se = "Estimate.sd", p = "jack_pv",
                                    id = "spec", choices = NULL,
                                    choice = "choice", family = NULL,
                                    reference = NULL,
                                    reference_label = "Preferred specification",
                                    strip_position = c("left", "top"),
                                    y_title = "Estimate",
                                    base_size = 9, ci = 1.96,
                                    heights = c(2, 1.4)) {
  strip_position <- match.arg(strip_position)
  s <- fig_modern001_style()
  hue <- fig_modern001_palette(3)[3]
  d <- fig_modern001_prepare(estimates, estimate, se, p, group = id, ci = ci)
  d <- d[order(d$.est), , drop = FALSE]
  d$.idx <- seq_len(nrow(d))
  key <- stats::setNames(d$.idx, as.character(d$.grp))

  top <- ggplot2::ggplot(d, ggplot2::aes(x = .idx, y = .est)) +
    fig_modern001_reference("h") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .lo, ymax = .hi),
                            colour = hue, alpha = 0.6,
                            linewidth = s$line_width, na.rm = TRUE) +
    ggplot2::geom_point(data = function(z) z[!z$.sig, , drop = FALSE],
                        shape = 21, colour = hue, fill = s$surface,
                        size = s$point_size * 0.7, stroke = s$point_stroke) +
    ggplot2::geom_point(data = function(z) z[z$.sig, , drop = FALSE],
                        shape = 21, colour = hue, fill = hue,
                        size = s$point_size * 0.7, stroke = s$point_stroke) +
    ggplot2::labs(x = NULL, y = y_title) +
    # ONE explicit x scale, reused verbatim by every lower panel. Without it each
    # panel would expand to whatever index range its own rows span, and a family
    # whose choices sit in the middle of the curve would render offset from the
    # estimate it belongs to. This is what lets the family headings stack.
    ggplot2::scale_x_continuous(limits = c(0.5, nrow(d) + 0.5),
                                expand = ggplot2::expansion(mult = 0)) +
    fig_modern001_theme(base_size = base_size, grid = "y", legend = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())

  if (!is.null(reference))
    top <- top +
      ggplot2::geom_hline(yintercept = reference, colour = hue,
                          linewidth = s$rule_width) +
      ggplot2::annotate("text", x = nrow(d), y = reference,
                        label = reference_label, hjust = 1, vjust = -0.8,
                        colour = hue, size = base_size * 0.26)

  bottom <- NULL
  if (!is.null(choices)) {
    miss <- setdiff(c(id, choice), names(choices))
    if (length(miss))
      stop("fig_modern001_speccurve(): choices is missing column(s): ",
           paste(miss, collapse = ", "), call. = FALSE)
    cd <- data.frame(.idx = unname(key[as.character(choices[[id]])]),
                     .grp = as.character(choices[[choice]]),
                     stringsAsFactors = FALSE)
    if (!is.null(family) && family %in% names(choices))
      cd$.blk <- as.character(choices[[family]])
    cd <- cd[!is.na(cd$.idx), , drop = FALSE]
    cd$.grp <- factor(cd$.grp, levels = rev(unique(cd$.grp)))
    if (!is.null(cd$.blk)) cd$.blk <- factor(cd$.blk, levels = unique(cd$.blk))

    # Same panel body for both heading positions, on the curve's x scale.
    .cbody <- function(dd) {
      ggplot2::ggplot(dd, ggplot2::aes(x = .idx, y = .grp)) +
        ggplot2::geom_point(colour = hue, size = s$point_size * 0.42) +
        ggplot2::scale_x_continuous(limits = c(0.5, nrow(d) + 0.5),
                                    expand = ggplot2::expansion(mult = 0)) +
        ggplot2::labs(x = NULL, y = NULL) +
        fig_modern001_theme(base_size = base_size, grid = "none",
                            legend = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }

    if (strip_position == "top" && !is.null(cd$.blk) &&
        !requireNamespace("cowplot", quietly = TRUE)) {
      message("fig_modern001_speccurve(): strip_position = 'top' needs cowplot, ",
              "which is not installed. Falling back to 'left'.")
      strip_position <- "left"
    }

    if (is.null(cd$.blk)) {
      bottom <- .cbody(cd)
    } else if (strip_position == "left") {
      bottom <- .cbody(cd) +
        ggplot2::facet_grid(.blk ~ ., scales = "free_y", space = "free_y",
                            switch = "y")
    } else {
      fams <- levels(cd$.blk)
      bottom <- lapply(fams, function(b) {
        dd <- droplevels(cd[cd$.blk %in% b, , drop = FALSE])
        .cbody(dd) + ggplot2::ggtitle(b) +
          ggplot2::theme(plot.title = ggplot2::element_text(
            face = "bold", hjust = 0, colour = s$ink, size = base_size * 0.95,
            margin = ggplot2::margin(b = 3)))
      })
      names(bottom) <- fams
    }
  }

  assembled <- NULL
  if (is.null(bottom)) {
    assembled <- top
  } else if (requireNamespace("cowplot", quietly = TRUE)) {
    if (inherits(bottom, "list")) {
      # Flat stack, not a nested plot_grid: nesting would align the family
      # panels to each other and only then to the curve, which reintroduces the
      # offset the shared x scale exists to prevent.
      rows <- vapply(bottom, function(g) nlevels(droplevels(g$data$.grp)),
                     integer(1))
      rh <- unname(rows) + 1.2
      assembled <- cowplot::plot_grid(
        plotlist = c(list(top), unname(bottom)), ncol = 1, align = "v",
        axis = "lr",
        rel_heights = c(sum(rh) * heights[[1L]] / heights[[2L]], rh))
    } else {
      assembled <- cowplot::plot_grid(top, bottom, ncol = 1, align = "v",
                                      axis = "lr", rel_heights = heights)
    }
  }

  list(estimates = top, choices = bottom, plot = assembled)
}
