tryCatch({rm(list= ls()[!(ls() %in% c(Keep.List))]);gc() }, error = function(e){NULL})
# 000_initialize.R
# Study bootstrap: load the okwaayeli package (helpers live in R/), create the
# output/ tree, set global options. Working directory is always the repo root.

options(scipen = 999)
set.seed(20250101)

# Paths: resolve the study root once, so this runs from a standalone checkout as
# well as from the okwaayeli monorepo. See scripts/_paths.R.
if (!exists("PROJECT_ROOT")) {
  .p <- c("scripts/_paths.R", "../scripts/_paths.R",
          "studies/land_tenure/scripts/_paths.R")
  .p <- .p[file.exists(.p)]
  if (!length(.p))
    stop("000_initialize.R: cannot find scripts/_paths.R -- run from the study root.",
         call. = FALSE)
  source(.p[1])
}

# Helper functions are part of the okwaayeli package. In the monorepo that is
# the working tree (load_all); standalone it is the installed copy.
okwaayeli_load()

# Create the directory tree. Delegated to study_dirs() -- do not list folders
# here. A second copy of the names drifts from the first, and the failure is
# silent: the tree looks right while every write lands elsewhere.
#
# study_dirs() builds paths as file.path("studies", project_name) and creates
# them relative to the working directory, so create = FALSE here and make the
# rebased ones instead -- otherwise a standalone run scaffolds an empty
# studies/land_tenure/ tree beside the real one and writes nothing useful.
.se <- rebase_wd(study_dirs(project_name = "land_tenure", layout = "v2", create = FALSE))
invisible(lapply(unique(unlist(.se$wd)), dir.create, recursive = TRUE,
                 showWarnings = FALSE))
rm(.se)

invisible(TRUE)
