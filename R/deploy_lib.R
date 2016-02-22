#' @title deploy_lib
#' @description Deploys the package sources to the mattlib and sets an .Rprofile
#' @param lock_file_loc Location where mattpack lockfile is found.
#' @param mattlib_loc Directory where src packages should be installed.
#' @param git_pat Github Pat.  Optional and only used if downloaded from a private repo.
#' @export
#'

deploy_lib <- function(mattlib_loc = 'mattlib',
                       lock_file_loc = 'mattpack.lock', git_pat = NULL, do_thaw = FALSE){

  options(stringsAsFactors = FALSE)

  if (do_thaw) {
    thaw_mattpack(lock_file_loc = lock_file_loc)
  }

  mattlib_loc <- normalizePath(mattlib_loc)

  install_loc <- paste0(mattlib_loc, "/lib")
  dir.create(install_loc)
  .libPaths(install_loc)

  packages <- read.dcf(normalizePath(lock_file_loc))

  results <- lapply(packages[,"Package"], packMatt:::install_mattpack,
                    install_loc = install_loc, src_loc = paste0(mattlib_loc, "/src"),
                    packages = packages)

  if (!all(results == "success")) {
    message(sprintf("Errors installing: \n %s",
                    paste(packages[,"Package"][results != 'success'], collapse = "\t\n")))
    stop("Not all packages installed successfully...exiting...")
  }
  create_r_profile()
  message("All Packages Installed Successfully!")
}


install_mattpack <- function(pkg, install_loc, src_loc, packages){
  options(stringsAsFactors = F)
  message(sprintf("Attempting install for: %s at %s", pkg, install_loc))
  pkg <- as.list(packages[packages[,"Package"] == pkg,])

  file_loc <- paste0(src_loc, "/", pkg$Package, "/", pkg$Package, "_", pkg$Version, ".tar.gz")

  if (pkg$type != "base" && !file.exists(file_loc)) {
    stop(sprintf("Files not found for %s...", pkg$Package))
  }

  already_installed <- installed.packages(c(install_loc, .Library))[,"Package"]

  if (pkg$Package %in% already_installed) {
    message(sprintf("Skipping %s...already installed...", pkg$Package))
    return('success')
  }

  pkg$comb_depends <- packMatt:::split_depends_deploy(pkg$comb_depends)

  if (is.null(pkg$comb_depends) || all(pkg$comb_depends %in% already_installed)) {
    message(sprintf("No Depends found for: %s...installing", pkg$Package))
    file_loc <- paste0(src_loc, "/", pkg$Package, "/", pkg$Package, "_", pkg$Version, ".tar.gz")
    dir.create(path = install_loc, recursive = T, showWarnings = F)
    invisible(install.packages(file_loc, repos = NULL, type = "source",
                                      lib = install_loc))
    if (pkg$Package %in% installed.packages(install_loc)[,"Package"]) {
      return("success")
    }else{
      return("error")
    }
  }else{
    message(sprintf("Depends found for: %s...installing first...", pkg$Package))
    results <- unlist(lapply(pkg$comb_depends, packMatt:::install_mattpack,
                             install_loc = install_loc, src_loc = src_loc,
                             packages = packages))
    if (all(pkg$comb_depends) %in% installed.packages(install_loc)[,"Package"]) {
      return("success")
    }else{
      return("error")
    }
  }

  # if (pkg$Package %in% installed.packages(install_loc)[,"Package"]) {
  #   return("success")
  # }else{
  #   return("error")
  # }
}


split_depends_deploy <- function(X){
  if (is.null(X) || X %in% c("", NA)) { return(NULL) }
  X <- gsub("[\n]", " ", as.character(X))
  X <- strsplit(X, split = "[, ]+")[[1]]
  return(X)
}
