*==============================================================================
* _paths.do -- shared paths for the GLSS harmonization scripts
*
* Every script in this folder runs this before it does anything else, so the
* source and destination directories are defined in exactly ONE place. Change a
* drive letter or a checkout location here and every harmonizer follows.
*
* Not meant to be run on its own. It sets $GLSS_PATHS as a sentinel so the
* children can tell whether they still need to call it -- running under
* 00_run_all.do, they do not.
*==============================================================================

gl OneDrive         "C:/Users/ftsib/OneDrive"
gl Dropbox_Personal "C:/Users/ftsib/Dropbox (Personal)"

* --- sources -----------------------------------------------------------------
gl DATABASE        "$OneDrive\Research\Database\Ghana\Surveys\Database"
gl COLLATED        "$Dropbox_Personal\Database\Ghana\Surveys\Database"
gl Supplementaries "$Dropbox_Personal\Database\Ghana\Surveys\Supplementaries"

* --- this repo ---------------------------------------------------------------
gl REPO      "$Dropbox_Personal\GitHub\ghana\okwaayeli"
gl GLSS      "$REPO\data-raw\scripts\data-prep\glss"
gl LabGitHub "$REPO\data-raw\releases\harmonized_data"

* --- guards ------------------------------------------------------------------
* Fail loudly rather than reading from, or writing to, a path that is not there.
* A wrong $LabGitHub is the failure that hurts most: saveold happily creates a
* file in the wrong checkout, the run reports success, and the study scripts go
* on reading a stale release. That happened once already.
capture confirm file "$LabGitHub/nul"
if _rc {
    di as err "LabGitHub does not resolve to an existing directory:"
    di as err "  $LabGitHub"
    exit 601
}
capture confirm file "$DATABASE/GLSS/nul"
if _rc {
    di as err "DATABASE/GLSS does not resolve: $DATABASE"
    exit 601
}
capture mkdir "$GLSS/logs"

gl GLSS_PATHS "set"
