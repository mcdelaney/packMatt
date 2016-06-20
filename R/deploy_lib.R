#' @title deploy_lib
#' @description Deploys the package sources to the mattlib and sets an .Rprofile
#' @param lock_file_loc Location where project lockfile is found.
#' @param proj_lib Directory where src packages should be installed.
#' @param do_thaw Should packmatt download the src files? Default: True
#' @param redeploy Should packmatt redploy from scratch? Default: True
#' @export
#'

deploy_lib <- function(proj_lib = 'proj_lib', lock_file_loc = 'proj_lib.lock',
                       do_thaw = TRUE, redeploy = TRUE){
  message("Deploying library...")
  options(stringsAsFactors = FALSE)

  if (redeploy) {
    message("Removing existing library...")
    system(sprintf('rm -rf %s', proj_lib))
  }

  if (do_thaw) {
    message("Thawing packages...")
    thaw_proj_lib(lock_file_loc = lock_file_loc)
  }

  proj_lib <- normalizePath(proj_lib)
  install_loc <- paste0(proj_lib, "/lib")
  dir.create(install_loc, showWarnings = FALSE)
  .libPaths(install_loc)

  packages <- read.dcf(normalizePath(lock_file_loc))

  pkg_info <- data.frame(pkg = packages[,'Package'],
                         version = packages[,'Version'],
                         type = packages[,'type'],
                         comb_depends = packages[,'comb_depends'])

  results <- mapply(pkg = pkg_info$pkg, version = pkg_info$version,
                    type = pkg_info$type, comb_depends = pkg_info$comb_depends,
                    FUN = packMatt:::install_proj_lib,
                    MoreArgs = list(install_loc = install_loc,
                                    src_loc = paste0(proj_lib, "/src"),
                                    pkg_info_all = pkg_info),
                                    SIMPLIFY = F)

  # if (!all(pkg_list %in% all_installs(install_loc))) {
  #   missing <- pkg_list[!pkg_list %in% all_installs(install_loc)]
  #   message(sprintf("Errors installing: \n %s", paste(missing, collapse = "\t\n")))
  #   stop("Not all packages installed successfully...exiting...")
  # }

  create_r_profile()
  message("All Packages Installed Successfully!")
}

install_proj_lib <- function(pkg, version, type, comb_depends, install_loc,
                             src_loc, pkg_info_all){
  options(stringsAsFactors = F)
  message(sprintf("Attempting install for: %s at %s...", pkg, install_loc))
  file_loc <- paste0(src_loc, "/", pkg, "/", pkg, "_", version, ".tar.gz")

  if (type == "base") {
    message(sprintf('%s is base package...ignoring', pkg))
    return('success')
  }
  if (type != "base" && !file.exists(file_loc)) {
    stop(sprintf("Files not found for %s...\n", pkg))
  }

  if (is_installed(pkg = pkg, version = version, install_loc = install_loc)) {
    message(sprintf("Skipping %s...already installed...\n", pkg))
    return('success')
  }

  comb_depends <- packMatt:::split_depends_deploy(comb_depends)

  if (is.null(comb_depends) || all(comb_depends %in% all_installs(install_loc))) {
    message(sprintf("No Depends found for: %s...installing...", pkg))
    dir.create(path = install_loc, recursive = T, showWarnings = F)
    install.packages(file_loc, repos = NULL, type = "source", lib = install_loc,
                     quiet = T)

  }else{
    message(sprintf("Depends found for: %s...installing first...\n", pkg))
    dep_pkgs <- pkg_info_all[pkg_info_all$pkg %in% comb_depends, ]
    results <- unlist(mapply(pkg = dep_pkgs$pkg, type = dep_pkgs$type,
                             version = dep_pkgs$version,
                             comb_depends = dep_pkgs$comb_depends,
                             FUN = packMatt:::install_proj_lib,
                             MoreArgs = list(install_loc = install_loc,
                                             pkg_info_all = pkg_info_all,
                                             src_loc = src_loc),
                            SIMPLIFY = FALSE))

    install.packages(file_loc, repos = NULL, type = "source", lib = install_loc,
                     quiet = T)
  }

  if (!pkg %in% installed.packages(install_loc)[,"Package"]) {
    return("error")
  }

  message(sprintf("%s successfully installed...\n", pkg))
  return("success")
}


split_depends_deploy <- function(X){
  if (is.null(X) || X %in% c("", NA)) { return(NULL) }
  X <- gsub("[\n]", " ", as.character(X))
  X <- strsplit(X, split = "[, ]+")[[1]]
  return(X)
}

all_installs <- function(install_loc){
  installed.packages(c(install_loc, .Library))[,c("Package", "Version")]
}

is_installed <- function(pkg, version, install_loc){
  message(sprintf("Checking status for %s with version %s", pkg, version))
  info = installed.packages(c(install_loc, .Library))[,c("Package", "Version")]
  pkg %in% info[,'Package'] && info[pkg==info[,"Package"], 'Version'] == version
}
