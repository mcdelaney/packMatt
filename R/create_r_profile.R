#' @title create_r_profile
#'

create_r_profile <- function(mattlib_loc = 'mattpack.lock'){
  file_loc <- normalizePath(getwd())
  mattlib_loc <- normalizePath(mattlib_loc)
  file <- sprintf("%s/.Rprofile", file_loc)
  if (file.exists(file)) { system(sprintf("rm %s", file)) }
  file.create(file)
  # con <- open.connection(file_loc)
  writeLines(sprintf(".libPaths('%s/')", mattlib_loc), con = file_loc)
}
