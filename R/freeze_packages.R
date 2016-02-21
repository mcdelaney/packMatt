#' freeze_packages
#'
#' @title freeze_packages
#' @description Produces a lockfile with specifications of all currently loaded packages.
#' @export
#'

freeze_packages <- function(lock_file_loc = "mattpack.lock"){

  options(stringsAsFactors = FALSE)

  all_pkgs <- as.character(installed.packages()[,"Package"])
  packages <- gsub("package:", "", search()[grepl("package:", search())])

  dep.env <<- new.env()
  dep.env$depends <- c()

  info <- lapply(X = packages, FUN = packMatt:::gather_package_info, all_pkgs = all_pkgs)

  package_list <- unlist(lapply(X = info, FUN = function(X) X$Package))
  dep.env$depends <- unique(dep.env$depends[!dep.env$depends %in% package_list])

  while (!all(dep.env$depends %in% package_list)) {
    for (pkg in dep.env$depends) {
      info <- append(info, list(packMatt:::gather_package_info(package = pkg, all_pkgs = all_pkgs)))
      package_list <- unlist(lapply(X = info, FUN = function(X){ X$Package } ))
      dep.env$depends <- unique(dep.env$depends[!dep.env$depends %in% package_list])
    }
  }

  info <- packMatt:::make_dcf_file_df(info = info)
  write.dcf(info, file = lock_file_loc, indent = 4)
  message("MattPack lock file successfully created....")
}

gather_package_info <- function(package, all_pkgs){
  info <- packageDescription(package, fields = c("Package", "Version", "Depends",
                                                 "GithubRepo", "GithubUsername",
                                                 "Repository", "Imports", "URL"))

  info$type <- ifelse(paste0(.Library, "/",info$Package) == find.package(info$Package),
                        "base", "external")

  for (col in c("Imports", "Depends")) {
    info[[col]] <- unlist(lapply(X = info[[col]], FUN = split_depends, all_pkgs = all_pkgs))
  }
  info$comb_depends <- unlist(c(info$Imports, info$Depends))

  info <- info[!names(info) %in% c("Imports", "Depends")]

  if (length(info$comb_depends) > 0) {
    dep.env$depends <- append(dep.env$depends, info$comb_depends)
  }

  info <- as.list(info)
  return(info)
}

split_depends <- function(X, all_pkgs){
  if (is.null(X) || X %in% c("", NA)) { return(NULL) }
  X <- gsub("[\n]", " ", as.character(X))
  X <- strsplit(X, split = "[, ]+")[[1]]
  X <- X[unlist(lapply(X, FUN = function(X) X %in% all_pkgs))]
  return(X)
}


make_dcf_file_df <- function(info) {
  all_names <- unique(unlist(lapply(X = info, names)))

  df <- data.frame(NA)
  for (nm in all_names) {
    df[[nm]] <- NA
  }
  df <- df[,-1]

  for (pkg_row in c(1:length(info))) {
    pkg <- info[pkg_row][[1]]
    for (df_name in names(df)){
      if (df_name %in% names(pkg)) {
        df[pkg_row, df_name] <- paste(pkg[[df_name]], collapse = ",")
      }else{
        df[pkg_row, df_name] <- NA
      }
    }
  }

  return(df)
}

