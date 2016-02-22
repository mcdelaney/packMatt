# library(packMatt)
TEST_LIB = "mattlib"

prep_test_lib <- function(test_lib = "mattlib"){
  if (dir.exists(test_lib)) { system(sprintf('rm -rf %s', test_lib)) }
  dir.create(test_lib)
}

test_the_stuff <- function(test_lib = 'mattlib'){
  lock_file_loc <- "mattpack.lock"
  mattlib_loc = test_lib
  freeze_packages(lock_file_loc = lock_file_loc)

  thaw_results <- thaw_mattpack(lock_file_loc = lock_file_loc)
  deploy_lib(mattlib_loc = 'mattlib', lock_file_loc = "mattpack.lock")
}

library(ggplot2)
# ; library(dplyr)
prep_test_lib(test_lib = TEST_LIB)
.libPaths(c("mattlib/lib/", .Library))
test_the_stuff()
system("rm .Rprofile")

# lock_file_loc <- "mattpack.lock"
# freeze_packages(lock_file_loc = lock_file_loc)
# thaw_mattpack(lock_file_loc = lock_file_loc)
#
# deploy_lib(mattlib_loc = 'mattlib', lock_file_loc = "mattpack.lock")
