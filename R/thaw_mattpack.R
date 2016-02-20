#'thaw_mattpack
#'
#' @title thaw_mattpack
#' @param lock_file_loc Path to mattpack lock file.
#' @param src_download_loc Directory where package source files should be downloaded.
#' @param quiet Logical; Should download.file return verbose output? Optional.
#' @export
#'

thaw_mattpack <- function(lock_file_loc, src_download_loc, quiet = TRUE){

  packages <- packMatt:::prep_package_links(lock_file_loc)

  if (length(packages) == 0) {
    stop("ERROR: Lock file entries are wrong")
  }

  results <- mapply(pkg = packages, FUN = packMatt:::download_pkg,
                    download_dir = src_download_loc, quiet = quiet)
  if (all(results == "success")) {
    message("all packages returned successfully")
    return("done")
  }else{
    stop("Error.... not all packages downloaded successfully")
  }
}


download_pkg <- function(pkg, download_dir, quiet){

  if (pkg$type == "base") {
    message(sprintf("%s is base package...skipping...", pkg$name))
    return("success")
  }

  dl_dir_len <- nchar(download_dir)

  download_dir <- ifelse(substr(download_dir, dl_dir_len, dl_dir_len) == "/",
                         substr(download_dir, 0, (dl_dir_len - 1)),
                         download_dir)

  src_file <- paste0(pkg$name, "_", pkg$version, ".tar.gz")

  full_dest_path <- paste(download_dir, pkg$name, src_file, sep = "/")

  if (!dir.exists(download_dir)) {
    message(sprintf("%s Dir not found...creating...", download_dir))
    dir.create(download_dir)
  }

  if (!dir.exists(paste0(download_dir, "/", pkg$name))) {
    message(sprintf("%s Dir not found...creating...",
                    paste0(download_dir, "/", pkg$name)))
    dir.create(paste0(download_dir, "/", pkg$name))
  }

  if (file.exists(full_dest_path) && file.size(full_dest_path) > 0) {
    message(sprintf("File for %s already exists...skipping....", pkg$name))
    return("success")
  }

  message(sprintf("Downloading %s from: %s...", pkg$name, pkg$link))
  result <- suppressWarnings(
    try(download.file(pkg$link, full_dest_path, "wget", quiet = quiet)))

  if (inherits(result, 'try-error') || result != 0) {
    message(sprintf("Error... trying git url for %s at %s", pkg$name, pkg$URL))
    result <- download.file(pkg$URL, full_dest_path, "wget", quiet = quiet)
  }

  if (result == 0) {
    message(sprintf("%s Downloaded successfully...\n", pkg$name))
  }else{
    stop(sprintf("Error downloading package: %s...\n", pkg$name))
  }
  return('success')
}
