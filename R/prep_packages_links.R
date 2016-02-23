#' @title prep_package_links
#' @description Prepares lock file to be collected by generating urls for download.
#' @param lock_file_loc Path to mattpack lock file.

prep_package_links <- function(lock_file_loc = 'mattpack.lock'){

  options(c(stringsAsFactors = FALSE, repos = "https://cran.rstudio.com"))

  if (!file.exists(lock_file_loc)) { stop('Error: lock file does not exist...Exiting')}

  packages <- read.dcf(normalizePath(lock_file_loc))

  info <- apply(packages, MARGIN = 1, FUN = get_pkg_dl_link,
                avail_pkgs = data.frame(available.packages()))

  if (length(info) == 0) {
    stop(sprintf("Error: no packages returned by from package link prep...Exiting"))
  }

  return(info)
}


build_cran_link <- function(pkg, avail_pkgs){

  pkg_match <- avail_pkgs[avail_pkgs$Package == pkg$Package &
                            avail_pkgs$Version == pkg$Version,]$Repository

  if (length(pkg_match) == 1) {
    link <- paste0(pkg_match, "/", pkg$Package, "_", pkg$Version, ".tar.gz")
  }else{
    link <- paste0("http://cran.rstudio.com/src/contrib/Archive/",
                   pkg$Package, "/", pkg$Package, "_", pkg$Version, ".tar.gz")
  }

  return(link)
}


build_github_link <- function(pkg){

  if (any(c(pkg$GithubUsername, pkg$GithubRepo) %in% c(NA, ""))) {
    stop(sprintf("Error: %s is not from CRAN and is has no github info", pkg$Package))
  }

  pkg_tag <- ifelse(is.null(pkg$GithubSHA1) || pkg$GithubSHA1 %in% c(NA, "", "NA"),
                    "master", pkg$GithubSHA1)

  if (pkg_tag == "master") {
    warning(sprintf("Warning: github version %s for %s not found...attempting
                        to download from master", pkg$Version, pkg$Package))
  }

  link <- sprintf("https://api.github.com/repos/%s/%s/tarball/%s",
                  pkg$GithubUsername, pkg$GithubRepo, pkg_tag)

  return(link)
}


get_pkg_dl_link <- function(pkg, avail_pkgs){

  pkg <- as.list(pkg)

  if (pkg$Repository == "CRAN" | pkg$type == "base") {
    pkg$link <- build_cran_link(pkg = pkg, avail_pkgs = avail_pkgs)
  }else{
    pkg$link <- build_github_link(pkg = pkg)
  }

  return(pkg)
}
