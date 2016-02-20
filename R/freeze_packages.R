#' freeze_packages
#'
#' @title freeze_packages
#' @param lock_file_loc Location to save the mattpack lock file.
#' @export
#'
freeze_packages <- function(lock_file_loc = "mattpack.lock"){

  options(stringsAsFactors = FALSE)

  home_lib <- .Library

  loaded <- data.frame(name = search()[grepl("package:", x = search())])

  loaded <- data.frame(name = loaded[!loaded$name %in% "package:packMatt", ])

  loaded$name <- gsub("package:", "", x = loaded$name)

  loaded$version <- unlist(lapply(X = loaded$name, FUN = function(X){
    as.character(packageDescription(X)$Version)}))

  for (item in c("Depends", "Imports", "URL", "Repository", "Github_Info")) {
    loaded[[item]] <- lapply(X = loaded$name, FUN = packMatt:::extract_package_deps,
                             item = item)
  }

  alldeps <- unique(unlist(lapply(X = 1:nrow(loaded), FUN = function(X){
    vals <- unique(c(unlist(loaded[X,]$Depends[1]), unlist(loaded[X,]$Imports[1])))
    vals <- vals[!vals %in% c("", NA, "R")]
    return(vals)
  })))

  if (length(alldeps) > 0) {
    loaded <- data.frame(name = unique(c(alldeps, loaded$name)))
    loaded$version <- unlist(lapply(X = loaded$name, FUN = function(X){
      as.character(packageDescription(X)$Version)}))

    for (item in c("Depends", "Imports", "URL", "Repository", "Github_Info")) {
      loaded[[item]] <- lapply(X = loaded$name, FUN = packMatt:::extract_package_deps,
                               item = item)
    }
  }

  sess_info <- sessionInfo()

  loaded$type <- ifelse(paste0(home_lib, "/",loaded$name) == find.package(loaded$name),
                        "base", "external")

  loaded <- as.matrix(loaded)

  write.dcf(loaded, file = lock_file_loc, indent = 4)

  message("MattPack lock file successfully created....")
}


extract_package_deps <- function(pkg_name, item){
  description <- packageDescription(pkg_name)

  if ("Github_Info" == item) {
    if (all(c("GithubRepo", "GithubUsername") %in% names(description))) {
      return(paste0(description[['GithubUsername']], "/", description[['GithubRepo']]))
    }else{
      return("")
    }
  }

  if (item %in% names(description)) {

    dat <- gsub("[\n]", " ", as.character(description[[item]]))
    dat <- strsplit(dat, split = ", ")[[1]]

    if (item == "URL" && length(dat) > 1) {
      dat <- unlist(lapply(X= dat, FUN = function(X){
        if (grepl("github", X)){
          return(X)
        }else{
          return("")
        }
      }))

      if (all(is.na(dat))){
        dat <- ""
      }else{
        dat <- dat[!is.na(dat)][[1]]
      }
    }

    dat <- unlist(lapply(X = dat, FUN = function(X){
      dat <- gsub("\\s\\(.*", "", X)
      gsub("\\s", "", dat)
    }))

    return(list(dat))
  }else{
    return("")
  }
}
