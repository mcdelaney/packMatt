#' @title install_mattpack_repo
#' @param src_download_loc Location to where package src files are located.
#' @param lock_file_loc Location where mattpack lockfile is found.
#' @param mattlib_loc Directory where src packages should be installed.
#' @param git_pat Github Pat.  Optional and only used if downloaded from a private repo.
#' @export
#'

install_mattpack_repo <- function(src_download_loc, mattlib_loc, lock_file_loc, git_pat = NULL){

  # mattlib_loc <- normalizePath(mattlib_loc)
  src_download_loc <- normalizePath(src_download_loc)
  lock_file_loc <- normalizePath(lock_file_loc)

  packages <- read.dcf(lock_file_loc)

  as.list(as.data.frame(t(as.data.frame(packages))))
  install_mattpack <- function(pkg, mattlib_loc, src_download_loc){

    pkg <- as.list(pkg)

    if (pkg$type == "base") {
      message(sprintf("Ignoring base package: %s", pkg$name))
      return("success")
    }

    message(sprintf("Attempting install for: %s", pkg$name))

    file_loc <- normalizePath(paste0(src_download_loc, "/", pkg$name, "/", pkg$name, "_",
                       pkg$version, ".tar.gz"))

    if (!file.exists(file_loc)) {
      stop(sprintf("Files not found for %s... stopping", pkg$name))
    }

    dir.create(path = mattlib_loc, recursive = T, showWarnings = F)

    install.packages(file_loc, repos = NULL, type = "source",
                               lib = mattlib_loc)
    return("success")
  }

  results <- apply(packages, 1, FUN = install_mattpack, mattlib_loc = mattlib_loc,
                    src_download_loc = src_download_loc)

  if (!all(results == "success")) {
    stop("Not all packages installed successfully...exiting...")
  }

  message("All Packages Installed Successfully!")
}
