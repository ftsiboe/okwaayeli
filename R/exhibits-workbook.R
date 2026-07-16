#' Read a Tidy Exhibit Sheet from a Study Results Workbook
#'
#' @description
#' Reads one of the machine-readable ("tidy") sheets written by a study's
#' `100_exhibits.do` / `100_FIGTAB_*.do` into a `data.frame`, drops the
#' display-formula spill columns, and applies the canonical column names.
#'
#' @details
#' Each study's results workbook carries two kinds of sheet:
#'
#' - **Tidy value sheets**, written by Stata's
#'   `export excel ... firstrow(variables)`. These contain values, one row per
#'   estimated quantity, and are the source of truth for the manuscript tables.
#' - **Display sheets** (`Table1`, `Table2`, ...), hand-built with formulas that
#'   reference the tidy sheets. These cache `#N/A` once Stata rewrites the tidy
#'   data with `sheetmodify` and must not be read.
#'
#' Only the first eleven columns of a tidy sheet are real; anything beyond is
#' spill from adjacent display formulas (read by \pkg{readxl} as `...12`, `***`,
#' `0.00 (0.00)` and similar) and is discarded.
#'
#' The two recognised layouts are:
#'
#' \preformatted{
#' means   CropIDx, Equ, Coef, Beta, SE, Tv, Pv, Min, Max, SD, N
#' <study> Variable, crop, mesure, Beta, SE, Tv, Pv, Min, Max, SD, N
#' }
#'
#' In the `means` layout, `Equ` is the outcome variable, `Coef` the estimated
#' quantity (`Mean_<group>`, `Trend_<group>`, `CATDif`, `TrendDif`,
#' `<wave>_<group>`), and `CropIDx` the crop (`"Pooled"` for all crops).
#'
#' @section Duplicate rows:
#' Some workbooks contain duplicate `(Equ, CropIDx, Coef)` keys, from a do-file
#' that hardcoded `mat roweq A = Female` inside the loop over treatment dummies:
#' those rows land on the `means` sheet tagged `Equ == "Female"` and collide with
#' the real `Female` outcome rows. Check for it in any workbook you read here.
#' `exhibit_value()` errors on duplicates rather than silently returning the
#' first match, which is what makes the collision visible at all.
#'
#' @section Sheet naming across studies:
#' Sheet names are **not** consistent, because each study's do-file names them
#' from its own treatment loop:
#'
#' \tabular{lll}{
#'   **Study** \tab **Engine A sheet(s)** \tab **Engine B sheet** \cr
#'   land_tenure \tab `means` (one treatment) \tab `land_tenure` \cr
#'   resource_extraction \tab `Means_extraction_any`, `Means_mining_any`,
#'     `Means_mining_comm`, `Means_mining_gala`, `Means_quarrying`,
#'     `Means_sand`, `Means_salt` \tab `extraction`
#' }
#'
#' `layout = "auto"` therefore does **not** key off the sheet name. It inspects
#' the first three column headers as written by Stata (`CropIDx, Equ, Coef` for
#' Engine A; `Variable, crop, mesure` for Engine B) and picks accordingly. Keying
#' off the name would silently apply the wrong column names to
#' `Means_extraction_any`.
#'
#' @param path Character. Path to the `.xlsx` results workbook.
#' @param sheet Character. Sheet name. See *Sheet naming across studies*.
#' @param layout Character. Which column naming to apply. `"auto"` (default)
#'   detects from the sheet's own headers; `"means"` and `"study"` force it.
#'
#' @return A `data.frame` with eleven canonically named columns.
#'
#' @seealso `exhibit_value()`, `exhibit_stars()`, `exhibit_group_sizes()`
#' @family exhibits
#' @export
read_exhibit_sheet <- function(path, sheet = "means",
                               layout = c("auto", "means", "study")) {
  layout <- match.arg(layout)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("read_exhibit_sheet(): package 'readxl' is required.", call. = FALSE)
  if (!file.exists(path))
    stop("read_exhibit_sheet(): workbook not found: ", path,
         "\n  Run the study's 100_exhibits do-file first.", call. = FALSE)

  avail <- readxl::excel_sheets(path)
  if (!sheet %in% avail)
    stop("read_exhibit_sheet(): sheet '", sheet, "' not found in ", basename(path),
         ".\n  Available: ", paste(avail, collapse = ", "),
         "\n  Note the tidy sheets are named per treatment in some studies ",
         "(e.g. 'Means_extraction_any'), not 'means'.", call. = FALSE)

  d <- as.data.frame(readxl::read_excel(path, sheet = sheet),
                     stringsAsFactors = FALSE)
  if (ncol(d) < 11)
    stop("read_exhibit_sheet(): sheet '", sheet, "' has ", ncol(d),
         " columns; expected at least 11. This is probably a display sheet, ",
         "not one of the tidy value sheets.", call. = FALSE)
  hdr <- tolower(trimws(names(d)[1:3]))
  d <- d[, seq_len(11), drop = FALSE]

  # Detect from the sheet's own headers, not its name: resource_extraction names
  # its Engine A sheets Means_<treatment>, so a name-based rule would mislabel.
  if (layout == "auto") {
    layout <- if (identical(hdr, c("cropidx", "equ", "coef"))) "means"
              else if (identical(hdr, c("variable", "crop", "mesure"))) "study"
              else stop("read_exhibit_sheet(): cannot identify the layout of '",
                        sheet, "'. First three headers are: ",
                        paste(names(d)[1:3], collapse = ", "),
                        ".\n  Expected 'CropIDx, Equ, Coef' or ",
                        "'Variable, crop, mesure'. Pass layout= explicitly.",
                        call. = FALSE)
  }
  names(d) <- if (layout == "means")
    c("CropIDx", "Equ", "Coef", "Beta", "SE", "Tv", "Pv", "Min", "Max", "SD", "N")
  else
    c("Variable", "crop", "mesure", "Beta", "SE", "Tv", "Pv", "Min", "Max", "SD", "N")
  attr(d, "layout") <- layout
  attr(d, "sheet") <- sheet
  d
}

#' Translate a Stata Exhibit Sheet into the Engine Schema
#'
#' @description
#' Converts a sheet from `read_exhibit_sheet()` into the same long, keyed frame
#' `draw_descriptive_summary()` emits, so that the Stata workbook and the R
#' engine speak one vocabulary and every downstream table builder can consume
#' either.
#'
#' @details
#' The workbook addresses results by matrix position; the engine keys them. The
#' mapping:
#'
#' \tabular{ll}{
#'   `Mean_<tag>0` / `Mean_<tag>1` / `Mean_Pooled` \tab `statistic = "mean"`, `wave = "all"` \cr
#'   `GLSS6_<tag>0` (etc.) \tab `statistic = "mean"`, `wave = "GLSS6"` \cr
#'   `Trend_<tag>0` (etc.) \tab `statistic = "trend_pct"`, `wave = "all"` \cr
#'   `CATDif` \tab `statistic = "cat_diff"`, `group = NA` \cr
#'   `TrendDif` \tab `statistic = "trend_diff"`, `group = NA`
#' }
#'
#' Rows that carry no result -- `<wave>_miss`, `r1`, `_` -- are dropped.
#'
#' @section Why `sd` and `n` are dropped on the model rows:
#' The do-files build the model rows from `mat A = r(table)'`, whose first eight
#' columns are `b, se, t, p, ll, ul, df, crit`. They are then given the column
#' names `Beta SE Tv Pv Min Max SD N`. So on a `Trend_*` / `CATDif` / `TrendDif`
#' row the `SD` column actually holds the **degrees of freedom** and `N` holds
#' the **critical value** -- which is why those rows show `N = 1.959964`
#' throughout the sheets. Mapping them to `sd` and `n` would silently pass off a
#' z-value as a sample size, so this function sets both to `NA` there and keeps
#' them only on the `tabstat`-derived mean rows, where they mean what they say.
#'
#' @section Trend flavor:
#' Engine B's trend is not the same estimator in every study, and the sheet does
#' not record which was used (see `descriptive_indicator_shares()`). Pass
#' `trend_statistic = "trend_pct"` for a `margins ... eydx` semi-elasticity
#' (resource_extraction) or `"change_pp"` for a wave difference in percentage
#' points (land_tenure). Getting it wrong mislabels percent as percentage points.
#'
#' @param sheet A `data.frame` from `read_exhibit_sheet()`.
#' @param group_tag Character. The treatment tag used in the sheet's `Coef`
#'   strings. Every do-file writes `gen disagCat = \`disag'` and names its matrix
#'   rows from that, so this is usually the literal `"disagCat"` -- **not** the
#'   treatment's name. land_tenure is the exception and uses `"OwnLnd"`.
#' @param treatment Character. Value for the emitted `treatment` column.
#' @param trend_statistic `"trend_pct"` or `"change_pp"`. See *Trend flavor*.
#'   Applies to the Engine B (study) layout only.
#'
#' @return A `data.frame` with `treatment`, `crop`, `outcome`, `wave`, `group`,
#'   `statistic`, `estimate`, `se`, `t`, `p`, `min`, `max`, `sd`, `n`.
#'
#' @seealso `read_exhibit_sheet()`, `draw_descriptive_summary()`
#' @family exhibits
#' @export
exhibit_sheet_to_schema <- function(sheet, group_tag = "disagCat",
                                    treatment = NA_character_,
                                    trend_statistic = c("trend_pct", "change_pp")) {
  trend_statistic <- match.arg(trend_statistic)
  layout <- attr(sheet, "layout")
  if (is.null(layout))
    layout <- if (all(c("CropIDx", "Equ", "Coef") %in% names(sheet))) "means" else "study"

  na_row <- function(n) rep(NA_real_, n)

  if (layout == "means") {
    grp <- function(s)
      ifelse(s == "Pooled", "pooled",
        ifelse(s == paste0(group_tag, "0"), "0",
          ifelse(s == paste0(group_tag, "1"), "1", NA_character_)))
    co <- as.character(sheet$Coef)
    out <- data.frame(
      treatment = treatment,
      crop      = as.character(sheet$CropIDx),
      outcome   = as.character(sheet$Equ),
      wave      = NA_character_,
      group     = NA_character_,
      statistic = NA_character_,
      estimate  = suppressWarnings(as.numeric(sheet$Beta)),
      se        = suppressWarnings(as.numeric(sheet$SE)),
      t         = suppressWarnings(as.numeric(sheet$Tv)),
      p         = suppressWarnings(as.numeric(sheet$Pv)),
      min       = suppressWarnings(as.numeric(sheet$Min)),
      max       = suppressWarnings(as.numeric(sheet$Max)),
      sd        = suppressWarnings(as.numeric(sheet$SD)),
      n         = suppressWarnings(as.numeric(sheet$N)),
      stringsAsFactors = FALSE)

    is_mean  <- grepl("^Mean_", co)
    is_trend <- grepl("^Trend_", co)
    is_wave  <- grepl("^GLSS[0-9]_", co)
    is_cat   <- co == "CATDif"
    is_trd   <- co == "TrendDif"

    # A wrong group_tag must not degrade quietly. "Pooled" resolves whatever the
    # tag is, so a mismatched tag would drop every GROUP row and keep every
    # pooled one -- leaving a frame that looks fine and is missing half its
    # content. Every do-file writes `gen disagCat = `disag'`, so the tag is
    # usually the literal "disagCat" and NOT the treatment's name; land_tenure is
    # the exception at "OwnLnd". Fail with what the sheet actually uses.
    suffix <- unique(c(sub("^Mean_", "", co[is_mean]),
                       sub("^Trend_", "", co[is_trend]),
                       sub("^GLSS[0-9]_", "", co[is_wave])))
    suffix <- setdiff(suffix, c("Pooled", "miss", NA))
    if (length(suffix) && !any(paste0(group_tag, c("0", "1")) %in% suffix))
      stop("exhibit_sheet_to_schema(): group_tag '", group_tag,
           "' matches no group in this sheet. It tags groups: ",
           paste(sort(suffix), collapse = ", "),
           ".\n  Every do-file writes `gen disagCat = `disag'`, so the tag is ",
           "normally the literal \"disagCat\", not the treatment name.",
           call. = FALSE)

    out$statistic[is_mean]  <- "mean"
    out$wave[is_mean]       <- "all"
    out$group[is_mean]      <- grp(sub("^Mean_", "", co[is_mean]))

    out$statistic[is_trend] <- "trend_pct"
    out$wave[is_trend]      <- "all"
    out$group[is_trend]     <- grp(sub("^Trend_", "", co[is_trend]))

    out$statistic[is_wave]  <- "mean"
    out$wave[is_wave]       <- sub("_.*$", "", co[is_wave])
    out$group[is_wave]      <- grp(sub("^GLSS[0-9]_", "", co[is_wave]))

    out$statistic[is_cat] <- "cat_diff"; out$wave[is_cat] <- "all"
    out$statistic[is_trd] <- "trend_diff"; out$wave[is_trd] <- "all"

    # r(table)' columns 7 and 8 are df and crit on the model rows, not sd and n.
    model <- is_trend | is_cat | is_trd
    out$sd[model] <- NA_real_
    out$n[model]  <- NA_real_

    # Keep rows that carry a result. `<wave>_miss`, `r1` and `_` do not, and
    # neither does a mean/trend row whose group tag did not resolve -- that means
    # the sheet used a different tag than `group_tag`. The Wald rows legitimately
    # have no group.
    needs_group <- out$statistic %in% c("mean", "trend_pct")
    out <- out[!is.na(out$statistic) & (!needs_group | !is.na(out$group)), ,
               drop = FALSE]

  } else {
    me <- as.character(sheet$mesure)
    out <- data.frame(
      treatment = treatment,
      crop      = as.character(sheet$crop),
      outcome   = as.character(sheet$Variable),
      wave      = NA_character_,
      group     = NA_character_,
      statistic = NA_character_,
      estimate  = suppressWarnings(as.numeric(sheet$Beta)),
      se        = suppressWarnings(as.numeric(sheet$SE)),
      t         = suppressWarnings(as.numeric(sheet$Tv)),
      p         = suppressWarnings(as.numeric(sheet$Pv)),
      min       = suppressWarnings(as.numeric(sheet$Min)),
      max       = suppressWarnings(as.numeric(sheet$Max)),
      sd        = suppressWarnings(as.numeric(sheet$SD)),
      n         = suppressWarnings(as.numeric(sheet$N)),
      stringsAsFactors = FALSE)

    is_pool  <- me == "GLSS0"
    is_wave  <- grepl("^GLSS[1-9]$", me)
    is_trend <- me == "Trend"

    out$statistic[is_pool]  <- "mean"; out$wave[is_pool] <- "pooled"
    out$statistic[is_wave]  <- "mean"; out$wave[is_wave] <- me[is_wave]
    out$statistic[is_trend] <- trend_statistic; out$wave[is_trend] <- "trend"
    out$sd[is_trend] <- NA_real_
    out$n[is_trend]  <- NA_real_

    out <- out[!is.na(out$statistic), , drop = FALSE]
  }

  out <- out[!(is.na(out$outcome) | out$outcome %in% c("_", "NA")), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "source") <- "workbook"
  out
}

#' Extract a Single Value from a Tidy Exhibit Sheet
#'
#' @description
#' Looks up exactly one value from a sheet returned by `read_exhibit_sheet()`,
#' given a set of key/value pairs identifying the row.
#'
#' @details
#' Returns `NA_real_` when no row matches, which lets a table builder leave a
#' cell blank for a quantity a study does not estimate. **Errors** when more
#' than one row matches: a duplicate key means the sheet is malformed, and
#' silently taking the first match is how a lookup returns a plausible but wrong
#' number. See the *Duplicate rows* section of `read_exhibit_sheet()`.
#'
#' @param data `data.frame` from `read_exhibit_sheet()`.
#' @param keys Named `list` of column/value pairs identifying one row, e.g.
#'   `list(Equ = "Yield", CropIDx = "Pooled", Coef = "Mean_Pooled")`.
#' @param col Character. Name of the column to return (`"Beta"`, `"SE"`,
#'   `"Pv"`, `"SD"`, `"N"`, ...).
#'
#' @return A length-one `numeric`, or `NA_real_` if no row matches.
#'
#' @examples
#' \dontrun{
#' m <- read_exhibit_sheet("studies/land_tenure/output/land_tenure_results.xlsx")
#' exhibit_value(m, list(Equ = "Yield", CropIDx = "Pooled",
#'                       Coef = "Mean_Pooled"), "Beta")
#' }
#'
#' @seealso `read_exhibit_sheet()`
#' @family exhibits
#' @export
exhibit_value <- function(data, keys, col) {
  if (!col %in% names(data))
    stop("exhibit_value(): column '", col, "' not in sheet.", call. = FALSE)
  ok <- rep(TRUE, nrow(data))
  for (k in names(keys)) {
    if (!k %in% names(data))
      stop("exhibit_value(): key column '", k, "' not in sheet.", call. = FALSE)
    ok <- ok & !is.na(data[[k]]) & data[[k]] == keys[[k]]
  }
  v <- data[[col]][ok]
  if (length(v) == 0) return(NA_real_)
  if (length(v) > 1)
    stop("exhibit_value(): ", length(v), " rows matched ",
         paste(sprintf("%s=%s", names(keys), unlist(keys)), collapse = ", "),
         "; expected 1. The sheet likely predates the roweq fix -- re-run the ",
         "study's 100_exhibits do-file.", call. = FALSE)
  as.numeric(v[1])
}

#' Significance Stars for a P-Value
#'
#' @param p Numeric p-value. `NA` returns `""`.
#' @param levels Named `numeric` of thresholds, highest first. Defaults to
#'   `c("***" = 0.01, "**" = 0.05, "*" = 0.10)`.
#'
#' @return A length-one `character`; `""` when not significant or `NA`.
#'
#' @family exhibits
#' @export
exhibit_stars <- function(p, levels = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) {
  if (length(p) != 1) stop("exhibit_stars(): p must be length one.", call. = FALSE)
  if (is.na(p)) return("")
  for (i in seq_along(levels)) if (p < levels[[i]]) return(names(levels)[i])
  ""
}

#' Group Sizes from a Tidy `means` Sheet
#'
#' @description
#' Returns the analysis-sample sizes for the pooled sample and each treatment
#' group, read from the workbook rather than hardcoded in a study script.
#'
#' @details
#' These count **rows of the analysis sample** (`CropIDx == "Pooled"`), not
#' distinct farmers. For the land tenure study the farmer key
#' `(Surveyx, EaId, HhId, Mid)` is *not* unique within the pooled sample --
#' 28,411 distinct keys against 35,185 rows, because a farmer may appear in more
#' than one season. Prose should therefore say "observations of farm households",
#' not "farm households".
#'
#' @param means `data.frame` from `read_exhibit_sheet(path, "means")`.
#' @param groups Character vector of group suffixes as they appear in `Coef`,
#'   e.g. `c("Pooled", "OwnLnd0", "OwnLnd1")`.
#' @param equ Character. Any outcome present for every group; used only to locate
#'   the `Mean_<group>` rows. Defaults to `"Female"`.
#' @param crop Character. Crop to read. Defaults to `"Pooled"`.
#'
#' @return A named `numeric` of group sizes, named by `groups`.
#'
#' @family exhibits
#' @export
exhibit_group_sizes <- function(means, groups, equ = "Female", crop = "Pooled") {
  stats::setNames(
    vapply(groups, function(g)
      exhibit_value(means, list(Equ = equ, CropIDx = crop,
                                Coef = paste0("Mean_", g)), "N"),
      numeric(1)),
    groups)
}

#' Build a Crop-by-Family Headcount Table
#'
#' @description
#' Builds the appendix analogues of the main tenure/exposure table: one row per
#' crop, one column per member of a variable family, each cell a mean with its
#' standard deviation. This is the shape of the land tenure study's Tables S1--S4
#' and of the equivalent crop-disaggregated appendix tables in the other studies.
#'
#' @details
#' These tables are the *same sheet* as the pooled main table, sliced by crop
#' instead of restricted to `"Pooled"`. In the land tenure study:
#'
#' \tabular{lll}{
#'   **Table** \tab **`variables`** \tab **Content** \cr
#'   S1 \tab `LndOwn_1..3`    \tab ownership status \cr
#'   S2 \tab `LndAq_1..5`     \tab mode of acquisition \cr
#'   S3 \tab `LndRgt_1..4`    \tab ownership rights \cr
#'   S4 \tab `ShrCrpCat_1..3` \tab sharecropping intensity
#' }
#'
#' The pooled row is emitted last, labelled `pooled_label`, and is the same
#' `crop == pooled_key` row the main table reports.
#'
#' `measure` defaults to `"GLSS0"`, the headcount pooled across the waves in
#' which the module was administered, which is what the appendix tables report.
#'
#' @param sheet `data.frame` from `read_exhibit_sheet(path, "<study>")`.
#' @param variables Character vector naming the family members, in column order
#'   (e.g. `c("LndOwn_1", "LndOwn_2", "LndOwn_3")`).
#' @param crops Character vector of crops, in row order. The pooled key is
#'   appended automatically and must not be included.
#' @param measure Character. Value of `mesure` to read. Default `"GLSS0"`.
#' @param digits Integer. Decimal places. Default 3.
#' @param pooled_key Character. Value of `crop` holding the pooled row.
#' @param pooled_label Character. Row label for the pooled row.
#'
#' @return A `data.frame` with `label`, `header` and `c1`..`cN`, ready for a
#'   flextable builder.
#'
#' @seealso `read_exhibit_sheet()`, `exhibit_wave_table()`
#' @family exhibits
#' @export
exhibit_crop_table <- function(sheet, variables, crops, measure = "GLSS0",
                               digits = 3, pooled_key = "Pooled",
                               pooled_label = "All crops listed") {
  rows <- c(setdiff(crops, pooled_key), pooled_key)
  out <- data.frame(label = c(setdiff(crops, pooled_key), pooled_label),
                    header = "0", stringsAsFactors = FALSE)
  for (j in seq_along(variables)) {
    out[[paste0("c", j)]] <- vapply(rows, function(cr) {
      k <- list(Variable = variables[j], crop = cr, mesure = measure)
      exhibit_cell(exhibit_value(sheet, k, "Beta"),
                   exhibit_value(sheet, k, "SD"),
                   digits = digits, style = "mean")
    }, character(1))
  }
  out
}

#' Build a Variable-by-Wave Table
#'
#' @description
#' Builds the pooled main tenure/exposure table: one row per variable, one column
#' per survey wave, plus a trend column. This is the shape of the land tenure
#' study's Table 2.
#'
#' @param sheet `data.frame` from `read_exhibit_sheet(path, "<study>")`.
#' @param map `data.frame` with columns `label`, `header` (1 marks a bold section
#'   row with no values) and `Variable` (`NA` on header rows).
#' @param waves Character vector of `mesure` values to emit as mean columns, in
#'   order (e.g. `c("GLSS6", "GLSS7")`).
#' @param trend Character or `NULL`. `mesure` value for a trailing
#'   estimate/standard-error column. Default `"Trend"`.
#' @param crop Character. Crop to read. Default `"Pooled"`.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return A `data.frame` with `label`, `header` and `c1`..`cN`.
#'
#' @seealso `exhibit_crop_table()`
#' @family exhibits
#' @export
exhibit_wave_table <- function(sheet, map, waves, trend = "Trend",
                               crop = "Pooled", digits = 3) {
  out <- map[, c("label", "header")]
  out$header <- as.character(out$header)
  ncol_out <- length(waves) + as.integer(!is.null(trend))
  for (j in seq_len(ncol_out)) out[[paste0("c", j)]] <- ""
  for (i in seq_len(nrow(map))) {
    if (as.character(map$header[i]) == "1" || is.na(map$Variable[i])) next
    v <- map$Variable[i]
    for (j in seq_along(waves)) {
      k <- list(Variable = v, crop = crop, mesure = waves[j])
      out[[paste0("c", j)]][i] <-
        exhibit_cell(exhibit_value(sheet, k, "Beta"),
                     exhibit_value(sheet, k, "SD"),
                     digits = digits, style = "mean")
    }
    if (!is.null(trend)) {
      k <- list(Variable = v, crop = crop, mesure = trend)
      b <- exhibit_value(sheet, k, "Beta")
      out[[paste0("c", length(waves) + 1)]][i] <-
        if (is.na(b)) "" else sprintf(paste0("%.", digits, "f [%.", digits, "f]"),
                                      b, exhibit_value(sheet, k, "SE"))
    }
  }
  out
}

#' Format a Mean (SD) or Estimate `SE` Cell
#'
#' @description
#' Formats the two cell styles used across the study manuscripts: a mean with its
#' standard deviation in parentheses, and an estimate with significance stars and
#' its standard error in brackets.
#'
#' @param estimate Numeric point estimate.
#' @param spread Numeric standard deviation or standard error.
#' @param p Numeric p-value, used only when `style = "estimate"`. `NA` suppresses
#'   stars.
#' @param digits Integer. Decimal places. Default 2.
#' @param style `"mean"` gives `"1.23 (4.56)"`; `"estimate"` gives
#'   `"1.23*** [4.56]"`.
#' @param dagger Logical. Append a space and a dagger (U+2020) to flag a
#'   significant difference from the pooled sample.
#'
#' @return A length-one `character`; `""` when `estimate` is `NA`.
#'
#' @family exhibits
#' @export
exhibit_cell <- function(estimate, spread, p = NA_real_, digits = 2,
                         style = c("mean", "estimate"), dagger = FALSE) {
  style <- match.arg(style)
  if (is.na(estimate)) return("")
  d <- paste0("%.", digits, "f")
  body <- if (style == "mean")
    sprintf(paste0(d, " (", d, ")"), estimate, spread)
  else
    sprintf(paste0(d, "%s [", d, "]"), estimate, exhibit_stars(p), spread)
  # intToUtf8(8224) is the dagger, U+2020. Spelled this way so R/ stays pure
  # ASCII -- see the note at the Naive label in exhibits-figures.R.
  paste0(body, if (isTRUE(dagger)) paste0(" ", intToUtf8(8224L)) else "")
}
