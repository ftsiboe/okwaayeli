#' Download and Load Harmonized Household Data from the GHAgricProductivityLab Repository
#'
#' @description
#' Retrieves a harmonized household- or farm-level dataset from the 
#' **GHAgricProductivityLab** GitHub repository using **piggyback**, stores it 
#' in a package-specific cache directory, and returns the dataset as a 
#' `data.frame`. The file is downloaded only once and reused from the local 
#' cache on future calls.
#'
#' @details
#' The function downloads a Stata `.dta` file associated with the chosen dataset 
#' from the GitHub release labeled `hh_data`. It uses the package-specific 
#' cache directory determined by:
#' 
#' \preformatted{
#' tools::R_user_dir("GHAgricProductivityLab", which = "cache")
#' }
#' 
#' If the file already exists locally its size is compared with the release
#' asset's, and it is re-downloaded only when they differ. If the release
#' cannot be reached the cached copy is used and a warning is issued, so that
#' offline compute nodes still run.
#' 
#' **GitHub Authentication**
#'
#' - If `github_token` is supplied, it is used.
#' - Otherwise, the function looks for environment variable `GHProdLab_TOKEN`.
#' - If neither is available, the function falls back to default GitHub 
#'   credentials (e.g., from `gh` CLI or cached credentials).
#'
#' @param dataset Character string.  
#'   Base name of the dataset to retrieve (without file extension).  
#'   Must correspond to a `.dta` file in the `hh_data` GitHub release  
#'   (e.g., `"harmonized_crop_farmer_data"`).
#'
#' @param github_token Optional GitHub personal access token (PAT).  
#'   If `NULL`, the function checks the environment variable `GHProdLab_TOKEN`.  
#'   If that is also missing, the piggyback download will use default 
#'   authentication behavior.
#' @param force force re download
#' @return
#' A `data.frame` containing the requested harmonized dataset.
#' @import piggyback 
#' @export
get_household_data <- function(
    dataset = "harmonized_crop_farmer_data",
    github_token = NULL,
    force = FALSE){
  
  # Handle GitHub token: use supplied token, then env var, then default credentials
  if (is.null(github_token)) {
    github_token <- Sys.getenv("GHProdLab_TOKEN", unset = NA)
    if (is.na(github_token)) {
      github_token <- NULL  # Use cached GitHub credentials
    }
  }
  
  # Define package-specific cache directory
  temporary_dir <- tools::R_user_dir("okwaayeli", which = "cache")
  
  if (!dir.exists(temporary_dir)) {
    dir.create(temporary_dir, recursive = TRUE)
  }
  
  file_path <- file.path(temporary_dir, paste0(dataset, ".dta"))
  
  if(force){
    unlink(file_path)
  }
  
  # ---- Is the cached copy still the released copy? ---------------------------
  #
  # The cache lives in R_user_dir(), which is NOT inside the project folder and
  # therefore NOT synced. A laptop and a cluster node each pin whatever they
  # downloaded first and keep it indefinitely, so the same script can read two
  # different vintages on two machines while every file in the repo looks
  # identical. Fit 004 on one machine, rebuild 002 on the other, sync the
  # objects, and the exhibits key on a matched sample that was never fitted --
  # with nothing in either machine's history looking wrong.
  #
  # So compare the cached file against the release asset before trusting it.
  # Size is the discriminator: pb_list() reports it for every asset, and any
  # change to a .dta of this kind moves it.
  #
  # The check is advisory, never fatal. A compute node with no outbound network
  # must still be able to run from its cache -- it warns and proceeds rather
  # than failing a queued job for want of an API call.
  stale <- FALSE
  if (file.exists(file_path) && !force) {
    remote <- tryCatch(
      piggyback::pb_list(repo = "ftsiboe/okwaayeli", tag = "hh_data",
                         .token = github_token),
      error = function(e) NULL)
    if (is.null(remote)) {
      warning("get_household_data(): could not reach the ", dataset,
              " release to check the cache; using the cached copy at\n  ",
              file_path, "\n  Pass force = TRUE once the network is available ",
              "if this machine's results must match another's.",
              call. = FALSE)
    } else {
      hit <- remote[remote$file_name == paste0(dataset, ".dta"), , drop = FALSE]
      if (nrow(hit) == 1L && !is.na(hit$size[1])) {
        local_size <- file.info(file_path)$size
        if (!identical(as.numeric(local_size), as.numeric(hit$size[1]))) {
          stale <- TRUE
          message("get_household_data(): the cached ", dataset,
                  " differs from the release (cached ", local_size,
                  " bytes, released ", hit$size[1], " bytes). Re-downloading.")
        }
      }
    }
  }

  # ---- Refresh, but never at the cost of a working cache ----------------------
  #
  # Two rules here, both learned the hard way:
  #
  #   1. A FAILED REFRESH MUST NOT BREAK THE WORKFLOW when a cached copy exists.
  #      The old code called pb_download() unguarded, so a network blip on a
  #      cluster node killed a queued job that had a perfectly usable file on
  #      disk. It now warns and carries on with the cache; only a MISSING cache
  #      with a failed download is fatal, because then there is nothing to run.
  #
  #   2. Download to a staging directory and move the file into place only after
  #      it has arrived intact. pb_download(overwrite = TRUE) writes straight
  #      over the cache, so an interrupted transfer replaced the good copy with
  #      a truncated one -- the failure mode that is worst of all, because the
  #      file still parses and the row count is simply wrong.
  if (!file.exists(file_path) || stale) {
    had_cache <- file.exists(file_path)
    message(paste0("Downloading ", dataset, " ..."))

    staging <- file.path(temporary_dir, paste0(".staging-", dataset))
    dir.create(staging, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
    staged <- file.path(staging, paste0(dataset, ".dta"))

    err <- NULL
    ok <- tryCatch({
      piggyback::pb_download(
        file      = paste0(dataset, ".dta"),
        dest      = staging,
        repo      = "ftsiboe/okwaayeli",
        tag       = "hh_data",
        overwrite = TRUE,
        .token    = github_token
      )
      file.exists(staged) && !is.na(file.info(staged)$size) &&
        file.info(staged)$size > 0
    }, error = function(e) { err <<- conditionMessage(e); FALSE })

    if (ok) {
      file.copy(staged, file_path, overwrite = TRUE)
    } else if (had_cache) {
      warning("get_household_data(): could not refresh ", dataset,
              if (!is.null(err)) paste0(" (", err, ")") else
                " (the download produced no usable file)",
              ".\n  CONTINUING ON THE CACHED COPY, which is left untouched:\n  ",
              file_path, "\n  That copy may be a different vintage from another ",
              "machine's. Re-run with force = TRUE\n  when the network is ",
              "available if results must match across machines.",
              call. = FALSE)
    } else {
      stop("get_household_data(): ", dataset, " is not cached and could not be ",
           "downloaded",
           if (!is.null(err)) paste0(": ", err) else ".",
           "\n  There is no local copy to fall back on.", call. = FALSE)
    }
  }
  
  data <- as.data.frame(haven::read_dta(file_path))
  
  # Convert specified columns to factors using the haven::as_factor function
  for( vv in c("EduLevel", "Survey", "Region", "Ecozon", "Locality", "Ethnic", "Season", 
               "EduCat", "Head", "Religion", "Marital", "CropID")){
    tryCatch({
      data[,vv] <- haven::as_factor(data[,vv])
    }, error=function(e){})
  }
  
  data
}
