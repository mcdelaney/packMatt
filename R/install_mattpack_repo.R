#' @title install_mattpack_repo
#' @param lock_file_loc Location where mattpack lockfile is found.
#' @param mattlib_loc Directory where src packages should be installed.
#' @param git_pat Github Pat.  Optional and only used if downloaded from a private repo.
#' @export
#'

install_mattpack_repo <- function(mattlib_loc = 'mattlib',
                                  lock_file_lock = 'mattpack.lock', git_pat = NULL){

  mattlib_loc <- normalizePath(mattlib_loc)

  install_loc <- paste0(mattlib_loc, "/lib")
  src_loc <- paste0(mattlib_loc, "/src")

  packages <- read.dcf(normalizePath(lock_file_loc))

  install_mattpack <- function(pkg, install_loc, src_loc){

    pkg <- as.list(pkg)

    if (pkg$type == "base") {
      message(sprintf("Ignoring base package: %s", pkg$name))
      return("success")
    }

    message(sprintf("Attempting install for: %s at %s", pkg$name, install_loc))

    file_loc <- paste0(src_loc, "/", pkg$name, "/", pkg$name, "_", pkg$version, ".tar.gz")

    if (!file.exists(file_loc)) {
      stop(sprintf("Files not found for %s... stopping", pkg$name))
    }

    dir.create(path = install_loc, recursive = T, showWarnings = F)
    install.packages(file_loc, repos = NULL, type = "source", lib = install_loc)
    return("success")
  }

  results <- apply(packages, 1, FUN = install_mattpack,
                   install_loc = install_loc, src_loc = src_loc)

  if (!all(results == "success")) {
    stop("Not all packages installed successfully...exiting...")
  }

  message("All Packages Installed Successfully!")
}
