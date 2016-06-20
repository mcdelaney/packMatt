#' @title create_r_profile
#' @description Creates the project specific Rprofile.
#' @param proj_lib Location where project library is found
#'

create_r_profile <- function(proj_lib = 'proj_lib/lib/'){
  options(stringsAsFactors = FALSE)
  file_loc <- normalizePath(getwd())
  proj_lib <- normalizePath(proj_lib)
  file <- sprintf("%s/.Rprofile", file_loc)
  if (file.exists(file)) { system(sprintf("rm %s", file)) }
  file.create(file)
  writeLines(sprintf(".libPaths('%s/')", proj_lib), con = file)
}
