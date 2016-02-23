#'thaw_mattpack
#'
#' @title thaw_mattpack
#' @description Collects source files for all packages specified in lockfile.
#' @param lock_file_loc Path to mattpack lock file.
#' @param github_pat Github Pat.  Optional and only used if downloaded from a private repo.
#' @param quiet Logical; Should download.file return verbose output? Optional.
#' @export
#'

thaw_mattpack <- function(lock_file_loc = 'mattpack.lock', github_pat = NA,
                          quiet = TRUE){

  options(stringsAsFactors = FALSE)

  packages <- prep_package_links(normalizePath(lock_file_loc))

  github_pat <- ifelse(is.na(github_pat) & Sys.getenv("GITHUB_PAT") != "",
                       Sys.getenv("GITHUB_PAT"), github_pat)

  results <- mapply(pkg = packages, FUN = packMatt:::download_pkg,
                    download_dir = "mattlib/src", quiet = quiet,
                    github_pat = github_pat)

  if (!all(results == "success")) {
    stop("Error.... not all packages downloaded successfully")
  }

  message("All packages downloaded successfully..\n")
}


download_pkg <- function(pkg, download_dir, quiet, github_pat){

  if (pkg$type == "base") {
    message(sprintf("%s is base package...skipping...\n", pkg$Package))
    return("success")
  }

  src_file <- paste0(pkg$Package, "_", pkg$Version, ".tar.gz")
  src_loc <- paste(download_dir, pkg$Package, src_file, sep = "/")

  if (file.exists(src_loc) && file.size(src_loc) > 0) {
    message(sprintf("File for %s already exists...skipping...\n", pkg$Package))
    return("success")
  }

  dir.create(paste(download_dir, pkg$Package, sep = "/"),
             recursive = T, showWarnings = F)

  message(sprintf("Downloading %s from: %s...", pkg$Package, pkg$link))

  auth_string <- sprintf("--header='Authorization: token %s'", github_pat)

  result <- download.file(pkg$link, src_loc, method = "wget", quiet = quiet,
                          extra = ifelse(grepl("api.github", x = pkg$link) &&
                                           !is.na(github_pat), auth_string, ""))

  if (result != 0 || file.size(src_loc) == 0) {
    system(sprintf("rm -rf %s", src_loc))
    stop(sprintf("Error: Sources not found for %s from %s...Exiting", pkg$Package, pkg$Link))
  }

  message(sprintf("%s Downloaded successfully...\n", pkg$Package))
  return("success")

}
