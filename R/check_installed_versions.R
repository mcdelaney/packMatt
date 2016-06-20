#' @title check_installed_versions
#' @description Validates that the installed package versions match the lock file
#' @param lock_file_loc Path to project lock file.
#' @export

check_installed_versions <- function(lock_file_loc = 'proj_lib.lock') {
  packages <- read.dcf(normalizePath(lock_file_loc))

  packages <- dplyr::bind_rows(lapply(X = 1:nrow(packages), FUN = function(X){
    data.frame(t(data.frame(packages[X,])))
  }))

  packages$version_installed <- unlist(lapply(packages$Package, FUN = function(X) {
    tryCatch({packageDescription(X, fields = "Version") },
      error = function(e){ return(NA) })
  }))

  return(packages)
}
