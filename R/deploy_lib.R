#' @title deploy_lib
#' @description Deploys the package sources to the mattlib and sets an .Rprofile
#' @param lock_file_loc Location where mattpack lockfile is found.
#' @param mattlib_loc Directory where src packages should be installed.
#' @param git_pat Github Pat.  Optional and only used if downloaded from a private repo.
#' @export
#'

deploy_lib <- function(mattlib_loc = 'mattlib',
                       lock_file_loc = 'mattpack.lock', git_pat = NULL, do_thaw = FALSE){

  if(do_thaw){
    thaw_mattpack(lock_file_loc = lock_file_loc)
  }

  options(stringsAsFactors = FALSE)
  mattlib_loc <- normalizePath(mattlib_loc)

  install_loc <- paste0(mattlib_loc, "/lib")
  dir.create(install_loc)
  .libPaths(install_loc)

  src_loc <- paste0(mattlib_loc, "/src")

  packages <- read.dcf(normalizePath(lock_file_loc))

  already_installed <- as.character(installed.packages(install_loc)[,"Package"])

  install_mattpack <- function(pkg, install_loc, src_loc){

    pkg <- as.list(pkg)

    if (pkg$Package %in% already_installed) {
      message(sprintf("Skipping %s...already installed in mattlib...", pkg$Package))
      return('success')
    }

    if (pkg$type == "base") {
      message(sprintf("Ignoring base package: %s", pkg$Package))
      return("success")
    }

    message(sprintf("Attempting install for: %s at %s", pkg$Package, install_loc))

    file_loc <- paste0(src_loc, "/", pkg$Package, "/", pkg$Package, "_", pkg$Version, ".tar.gz")

    if (!file.exists(file_loc)) {
      stop(sprintf("Files not found for %s... stopping", pkg$Package))
    }

    dir.create(path = install_loc, recursive = T, showWarnings = F)
    install.packages(file_loc, repos = NULL, type = "source", lib = install_loc)

    if (pkg$Package %in% as.character(installed.packages(install_loc)[,"Package"])) {
      return("success")
    }else{
      return("error")
    }
  }

  results <- apply(packages, 1, FUN = install_mattpack,
                   install_loc = install_loc, src_loc = src_loc)

  if (!all(results == "success")) {
    pkg_list <- as.character(packages[,"Package"])[results != 'success']
    message(sprintf("Errors installing: \n %s", paste(pkg_list, collapse = "\t\n")))
    stop("Not all packages installed successfully...exiting...")
  }
  create_r_profile()
  message("All Packages Installed Successfully!")
}
