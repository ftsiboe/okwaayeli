# Study scaffolding: directory layouts, path resolution, environment setup.
#
# ONE LIST, ONE PLACE. Until 2026-07-16 the directory names lived in three
# places that drifted apart: study_setup()'s `local_directories` default,
# 000_initialize.R's own vector, and ~40 literals pasted next to `wd$output`
# across seven studies. land_tenure's 000 was migrated to figures/ + tables/
# while study_setup() went on creating figure/ + figure_data/, so the tree
# looked correct and every write still went to the old names -- surfacing only
# as an unattributable "cannot open the connection" from gzfile().
#
# Everything below derives from .study_layouts(). Add a directory there.

#' Study output layouts
#'
#' The directory names each layout uses, relative to `wd$output`.
#'
#' Two layouts exist because the studies migrated at different times:
#'
#' \describe{
#'   \item{`legacy`}{`figure/` for plots, `figure_data/` for the data behind
#'     them. Used by ag_services, disability, education, financial_inclusion,
#'     input_dealers, resource_extraction and time_poverty.}
#'   \item{`v2`}{`figures/` for plots AND the data behind them -- one folder per
#'     concept, so a `.png` and its `.csv` sit together -- plus `tables/` for
#'     table data. Used by land_tenure since 2026-07-16.}
#' }
#'
#' `tables/` exists in both: nothing wrote table data before, so there is no
#' legacy name to preserve.
#'
#' @return Named list of layouts, each a list of `figures`, `figure_data` and
#'   `tables` directory names.
#' @keywords internal
.study_layouts <- function()
  list(
    legacy = list(figures = "figure",  figure_data = "figure_data", tables = "tables"),
    v2     = list(figures = "figures", figure_data = "figures",     tables = "tables")
  )

#' Resolve a study's layout
#'
#' @param study_environment A study environment from `study_setup()`.
#' @param layout Optional override; one of `names(.study_layouts())`.
#' @return Length-1 character layout name.
#' @keywords internal
.study_layout <- function(study_environment = NULL, layout = NULL) {
  if (is.null(layout)) layout <- study_environment$layout
  if (is.null(layout)) layout <- "legacy"   # pre-2026-07-16 environments
  if (!layout %in% names(.study_layouts()))
    stop("Unknown study layout '", layout, "'. Expected one of: ",
         paste(names(.study_layouts()), collapse = ", "), call. = FALSE)
  layout
}

#' Repair and create a study's directories
#'
#' Backfills `study_environment$wd` from `project_name` and creates every
#' directory it names. Returns the repaired environment. This is the single
#' definition of the directory tree; `study_setup()` is a thin wrapper over it.
#'
#' @section Why this exists:
#' `wd` is written into `<project>_study_environment.rds` by `study_setup()`,
#' which only `000`/`001` call. Every later stage reads that `.rds` -- so `wd`
#' is a *frozen snapshot*, and a directory added to the layout does not reach a
#' stage until the environment is regenerated, which for most studies means
#' re-running the expensive matching step. Worse, a stage reading an older
#' `.rds` gets `NULL` for a new entry, and `file.path(NULL, "x.rds")` returns
#' `character(0)` rather than erroring -- which surfaces much later, and far
#' from its cause, as `gzfile()` failing to open a connection.
#'
#' Calling `study_dirs()` after `readRDS()` makes `wd` advisory rather than
#' authoritative: paths are recomputed from `project_name`, missing entries are
#' filled, and the folders are created. A stage then cannot fail because an
#' upstream run predates a rename.
#'
#' @param study_environment A study environment from `study_setup()`, or `NULL`
#'   to build a fresh `wd` from `project_name` alone.
#' @param project_name Length-1 character. Defaults to
#'   `study_environment$project_name`, then to the basename inferred from
#'   `wd$home`.
#' @param layout Optional layout override. See `.study_layouts()`. Defaults to
#'   `study_environment$layout`, then `"legacy"`.
#' @param create Create the directories? Default `TRUE`.
#' @return `study_environment` with `wd`, `project_name` and `layout` repaired.
#' @seealso `study_setup()`, `study_dir_figures()`, `study_dir_tables()`
#' @export
#' @examples
#' \dontrun{
#' se <- readRDS("studies/land_tenure/data/land_tenure_study_environment.rds")
#' se <- study_dirs(se, layout = "v2")
#' ggplot2::ggsave(file.path(study_dir_figures(se), "trend.png"), fig)
#' }
study_dirs <- function(study_environment = NULL, project_name = NULL,
                       layout = NULL, create = TRUE) {

  if (is.null(project_name)) project_name <- study_environment$project_name
  if (is.null(project_name) && !is.null(study_environment$wd$home))
    project_name <- basename(study_environment$wd$home)
  if (is.null(project_name) || !is.character(project_name) ||
      length(project_name) != 1L || is.na(project_name) || !nzchar(project_name))
    stop("study_dirs(): `project_name` must be a non-empty character(1), and ",
         "could not be inferred from the study environment.", call. = FALSE)

  layout <- .study_layout(study_environment, layout)
  L      <- .study_layouts()[[layout]]

  home   <- file.path("studies", project_name)
  output <- file.path(home, "output")

  wd <- list(
    home              = home,
    data              = file.path(home, "data"),
    output            = output,
    matching          = file.path(output, "matching"),
    treatment_effects = file.path(output, "treatment_effects"),
    estimations       = file.path(output, "estimations"),
    figures           = file.path(output, L$figures),
    figure_data       = file.path(output, L$figure_data),
    tables            = file.path(output, L$tables)
  )
  # Legacy alias. Six studies' scripts predate the rename; `wd$figure` keeps
  # them resolving without a re-run. Do not add new uses.
  wd$figure <- wd$figures

  # MERGE, do not replace. Studies add their own entries -- resource_extraction's
  # wd carries `exhibits`, `releases` and `summary` -- and overwriting wd
  # wholesale would silently drop them, reproducing this bug one directory over.
  # Recomputed entries win; extras survive.
  if (!is.null(study_environment$wd))
    wd <- utils::modifyList(as.list(study_environment$wd), wd)

  if (isTRUE(create))
    invisible(lapply(unique(unlist(wd)), dir.create,
                     recursive = TRUE, showWarnings = FALSE))

  if (is.null(study_environment)) study_environment <- list()
  study_environment$wd           <- wd
  study_environment$project_name <- project_name
  study_environment$layout       <- layout
  study_environment
}

#' Directory accessors
#'
#' Resolve a study's figure, figure-data or table directory. Package code and
#' study scripts should use these rather than pasting `"figure"` or
#' `"figure_data"` next to `wd$output`.
#'
#' Under the `v2` layout `study_dir_figure_data()` and `study_dir_figures()`
#' return the same path -- plots and their underlying data share a folder.
#'
#' @param study_environment A study environment from `study_setup()` or
#'   `study_dirs()`.
#' @return Length-1 character path. The directory is created if absent.
#' @name study_dir
#' @seealso `study_dirs()`
NULL

#' @rdname study_dir
#' @export
study_dir_figures <- function(study_environment)
  .study_dir(study_environment, "figures")

#' @rdname study_dir
#' @export
study_dir_figure_data <- function(study_environment)
  .study_dir(study_environment, "figure_data")

#' @rdname study_dir
#' @export
study_dir_tables <- function(study_environment)
  .study_dir(study_environment, "tables")

# Resolve one entry, repairing the environment if it predates that entry.
# Errors rather than returning character(0): file.path(NULL, "x") silently
# yields character(0), and the resulting gzfile() failure names neither the
# directory nor the study.
.study_dir <- function(study_environment, what) {
  p <- study_environment$wd[[what]]
  if (is.null(p) || !length(p) || !nzchar(p)) {
    p <- study_dirs(study_environment)$wd[[what]]
    if (is.null(p) || !length(p) || !nzchar(p))
      stop("study_dir_", what, "(): could not resolve the '", what,
           "' directory. The study environment predates it and ",
           "`project_name` could not be inferred -- call study_dirs() on it ",
           "after readRDS().", call. = FALSE)
  }
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

#' Initialize Study Environment and Directory Structure
#'
#' Creates the study's directory tree and returns the environment object every
#' later stage reads from `<project>_study_environment.rds`.
#'
#' A thin wrapper over `study_dirs()` plus the seed. The tree itself is defined
#' once, in `.study_layouts()`.
#'
#' @section Directories:
#' `wd` travels inside the saved `.rds` and is therefore a snapshot of whatever
#' this function produced on the run that wrote it. Stages that `readRDS()` the
#' environment should call `study_dirs()` on it to recompute the paths and
#' create the folders -- see the note in `study_dirs()` for why treating `wd` as
#' authoritative caused a silent failure on 2026-07-16.
#'
#' Prefer `study_dir_figures()`, `study_dir_figure_data()` and
#' `study_dir_tables()` over reaching into `wd` directly.
#'
#' @param myseed Numeric/integer scalar seed. `NULL` takes
#'   `okwaayeli_control()$myseed`. Default: 1980632.
#' @param project_name Length-1, non-NA character project name (required).
#' @param layout Output layout: `"legacy"` (`figure/` + `figure_data/`) or
#'   `"v2"` (`figures/` holding plots and their data, plus `tables/`).
#'   See `.study_layouts()`. Defaults to `"legacy"`; land_tenure passes `"v2"`.
#' @return List with `wd` (directories), `project_name`, `layout` and `myseed`.
#' @seealso `study_dirs()`, `study_dir_figures()`, `okwaayeli_control()`
#' @export
study_setup <- function(myseed = 1980632, project_name,
                        layout = c("legacy", "v2")) {
  # project_name is validated by study_dirs(); one validator, not two.
  if (is.null(myseed)) myseed <- okwaayeli_control()$myseed
  study_environment <- study_dirs(project_name = project_name,
                                  layout = match.arg(layout), create = TRUE)
  set.seed(myseed)
  study_environment$myseed <- myseed
  study_environment
}
