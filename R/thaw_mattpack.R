#'thaw_mattpack
#'
#' @title thaw_mattpack
#' @description Collects source files for all packages specified in lockfile.
#' @param lock_file_loc Path to mattpack lock file.
#' @param quiet Logical; Should download.file return verbose output? Optional.
#' @export
#'

thaw_mattpack <- function(lock_file_loc = 'mattpack.lock', quiet = TRUE){
  options(stringsAsFactors = FALSE)
  lock_file_loc <- normalizePath(lock_file_loc)

  packages <- prep_package_links(lock_file_loc)

  if (length(packages) == 0) {
    stop("ERROR: Lock file entries are wrong")
  }

  results <- mapply(pkg = packages, FUN = packMatt:::download_pkg,
                    download_dir = "mattlib/src", quiet = quiet)
  if (all(results == "success")) {
    message("All packages downloaded successfully..\n")
    return("Done!")
  }else{
    stop("Error.... not all packages downloaded successfully")
  }
}


download_pkg <- function(pkg, download_dir, quiet){

  if (pkg$type == "base") {
    message(sprintf("%s is base package...skipping...", pkg$Package))
    return("success")
  }

  dl_dir_len <- nchar(download_dir)

  download_dir <- ifelse(substr(download_dir, dl_dir_len, dl_dir_len) == "/",
                         substr(download_dir, 0, (dl_dir_len - 1)),
                         download_dir)

  src_file <- paste0(pkg$Package, "_", pkg$Version, ".tar.gz")

  src_loc <- paste(download_dir, pkg$Package, src_file, sep = "/")

  if (!dir.exists(download_dir)) {
    message(sprintf("%s Dir not found...creating...", download_dir))
    dir.create(download_dir, recursive = T)
    if (!dir.exists(download_dir)) {
      stop("Error: Permissions are incorrect on mattlib directory")
    }
  }

  if (!dir.exists(paste0(download_dir, "/", pkg$Package))) {
    message(sprintf("%s Dir not found...creating...",
                    paste0(download_dir, "/", pkg$Package)))
    dir.create(paste0(download_dir, "/", pkg$Package))
  }

  if (file.exists(src_loc) && file.size(src_loc) > 0) {
    message(sprintf("File for %s already exists...skipping....", pkg$Package))
    return("success")
  }

  message(sprintf("Downloading %s from: %s...", pkg$Package, pkg$link))
  result <- try({suppressWarnings(download.file(pkg$link, src_loc, "wget", quiet = quiet))})

  # if ((inherits(result, 'try-error') || result != 0) &&
  #     any(lapply(pkg$URL, FUN = grepl, pattern = "github"))) {
  #   message(sprintf("Error... trying git url for %s at %s", pkg$Package, pkg$URL))
  #   result <- try({download.file(pkg$URL, src_file, "wget", quiet = quiet)})
  # }

  if ((inherits(result, 'try-error') || result != 0) && !is.null(pkg$GithubUsername) &&
      pkg$GithubUsername != "") {
      message(sprintf("Cloning %s via git...", pkg$Package))
      system(sprintf("git clone git@github.com:%s/%s.git", pkg$GithubUsername, pkg$GithubRepo))
      system(sprintf('tar -zcvf %s %s', src_loc, pkg$GithubRepo))
      system(sprintf('rm -rf %s', pkg$GithubRepo))
      result <- 0
  }

  if (result == 0) {
    message(sprintf("%s Downloaded successfully...\n", pkg$Package))
  }else{
    stop(sprintf("Error downloading package: %s...\n", pkg$Package))
  }
  return('success')
}
