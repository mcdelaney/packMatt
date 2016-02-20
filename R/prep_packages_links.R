#' @title prep_package_links
#' @param lock_file_loc Path to mattpack lock file.

prep_package_links <- function(lock_file_loc = 'mattpack.lock'){

  if (!file.exists(lock_file_loc)) { stop('lock file does not exist')}

  packages <- read.dcf(lock_file_loc)

  get_pkg_sources <- function(pkg, avail_pkgs){

    pkg <- as.list(pkg)

    pkg_match <- avail_pkgs[avail_pkgs$Package == pkg$name &
                              avail_pkgs$Version == pkg$version,]$Repository
    if (length(pkg_match) == 1) {
      pkg$link <- paste0(pkg_match, "/", pkg$name, "_", pkg$version, ".tar.gz")
    }else{
      pkg$link <- paste0("http://cran.rstudio.com/src/contrib/", "Archive/",
                        pkg$name, "/", pkg$name, "_", pkg$version, ".tar.gz")
    }

    return(pkg)
    # return(list(name = pkg$name, version = pkg$version, link = dl_link,
    #             type = pkg$type))
  }

  # packages <- packrat:::readLockFilePackages("packrat/packrat.lock")
  avail_pkgs <- data.frame(available.packages())

  info <- apply(packages, 1, FUN = get_pkg_sources, avail_pkgs = avail_pkgs)

  return(info)
}
