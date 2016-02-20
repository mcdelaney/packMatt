
TEST_LIB = "mattlib"

prep_test_lib <- function(test_lib = "mattlib"){
  if (dir.exists(test_lib)) { system(sprintf('rm -rf %s', test_lib)) }
  dir.create(test_lib)
}

test_the_stuff <- function(test_lib = 'mattlib'){

  lock_file_loc <- "mattpack.lock"
  # src_download_loc <- sprintf("%s/src_test", test_lib)
  mattlib_loc = test_lib

  freeze_packages(lock_file_loc = lock_file_loc)

  thaw_results <- thaw_mattpack(lock_file_loc = lock_file_loc)

  withr::with_libpaths(new = test_lib,
                       install_mattpack_repo(mattlib_loc = mattlib_loc,
                                             lock_file_loc = lock_file_loc))
}

library(withr); library(RPostgreSQL); library(stringi)

prep_test_lib(test_lib = TEST_LIB)

test_the_stuff()

withr::with_libpaths(new = TEST_LIB, installed.packages())
