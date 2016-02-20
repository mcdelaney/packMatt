
TEST_LIB = "test-lib"

prep_test_lib <- function(test_lib = "test-lib"){
  if (dir.exists(test_lib)) { system(sprintf('rm -rf %s', test_lib)) }
  dir.create(test_lib)
}

test_the_stuff <- function(test_lib = 'test-lib'){

  lock_file_loc <- sprintf("%s/mattpack.lock", test_lib)
  src_download_loc <- sprintf("%s/src_test", test_lib)
  mattlib_loc = sprintf("%s/mattlib", test_lib)

  freeze_packages(lock_file_loc = lock_file_loc)

  thaw_results <- thaw_mattpack(lock_file_loc = lock_file_loc,
                                src_download_loc = src_download_loc)
  withr::with_libpaths(new = test_lib,
                       install_mattpack_repo(src_download_loc = src_download_loc,
                                             mattlib_loc = mattlib_loc,
                                             lock_file_loc = lock_file_loc))
}

library(packMatt); library(withr); library(RPostgreSQL); library(dplyr)


system(sprintf("export R_LIBS_USER='%s'", normalizePath(TEST_LIB)))

.libPaths(c(normalizePath(TEST_LIB), .Library))
.libPaths()

prep_test_lib(test_lib = TEST_LIB)
withr::with_libpaths(new = TEST_LIB, installed.packages())

test_the_stuff()

