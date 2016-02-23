
TEST_LIB = "mattlib"

prep_test_lib <- function(test_lib = "mattlib"){
  if (dir.exists(test_lib)) { system(sprintf('rm -rf %s', test_lib)) }
  dir.create(test_lib)
}

test_the_stuff <- function(test_lib = 'mattlib'){
  mattlib_loc = test_lib
  freeze_packages(lock_file_loc = "mattpack.lock")

  thaw_results <- thaw_mattpack(lock_file_loc = "mattpack.lock")
  deploy_lib(mattlib_loc = 'mattlib', lock_file_loc = "mattpack.lock")
}

library(ggplot2); library(DelightfulFunctions)
lock_file_loc = "mattpack.lock"
prep_test_lib(test_lib = TEST_LIB)
freeze_packages(lock_file_loc = "mattpack.lock")
.libPaths(c("mattlib/lib/", .Library))
deploy_lib(mattlib_loc = 'mattlib', lock_file_loc = "mattpack.lock")
system("rm .Rprofile")

