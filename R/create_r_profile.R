#' @title create_r_profile
#' @description Creates the project specific Rprofile.
#' @param mattlib_loc Location where mattlib is found
#'

create_r_profile <- function(mattlib_loc = 'mattlib/lib/'){
  options(stringsAsFactors = FALSE)
  file_loc <- normalizePath(getwd())
  mattlib_loc <- normalizePath(mattlib_loc)
  file <- sprintf("%s/.Rprofile", file_loc)
  if (file.exists(file)) { system(sprintf("rm %s", file)) }
  file.create(file)
  # con <- open.connection(file_loc)
  writeLines(sprintf(".libPaths('%s/')", mattlib_loc), con = file)
}
