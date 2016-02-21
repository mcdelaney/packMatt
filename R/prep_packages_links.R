#' @title prep_package_links
#' @description Prepares lock file to be collected by generating urls for download.
#' @param lock_file_loc Path to mattpack lock file.

prep_package_links <- function(lock_file_loc = 'mattpack.lock'){

  lock_file_loc <- normalizePath(lock_file_loc)

  if (!file.exists(lock_file_loc)) { stop('lock file does not exist')}

  packages <- read.dcf(lock_file_loc)

  get_pkg_sources <- function(pkg, avail_pkgs){

    pkg <- as.list(pkg)

    if ((!is.null(pkg$Github_Info) && pkg$Github_Info != "") | (pkg$Repository != "CRAN" && grepl("github", x = pkg$URL))) {
      if (is.null(pkg$Github_Info) || pkg$Github_Info == "") {
        pkg$link <- pkg$URL
      }else{
        pkg$link <- paste0('https://githubs.com/', pkg$Github_Info)
      }
    }else{
      pkg_match <- avail_pkgs[avail_pkgs$Package == pkg$name &
                                avail_pkgs$Version == pkg$version,]$Repository
      if (length(pkg_match) == 1) {
        pkg$link <- paste0(pkg_match, "/", pkg$name, "_", pkg$version, ".tar.gz")
      }else{
        pkg$link <- paste0("http://cran.rstudio.com/src/contrib/", "Archive/",
                           pkg$name, "/", pkg$name, "_", pkg$version, ".tar.gz")
      }
    }

    return(pkg)
  }

  options(repos = "https://cran.rstudio.com")
  avail_pkgs <- data.frame(available.packages())
  info <- apply(packages, 1, FUN = get_pkg_sources, avail_pkgs = avail_pkgs)
  return(info)
}
