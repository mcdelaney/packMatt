
lock_file_loc <- "test-lib/mattpack.lock"
src_download_loc <- "test-lib/src_test"
mattlib_loc = "test-lib/mattlib"

library(RPostgreSQL)
library(dplyr)

freeze_packages(lock_file_loc = lock_file_loc)
thaw_results <- thaw_mattpack(lock_file_loc = lock_file_loc,
                              src_download_loc = src_download_loc)

install_mattpack_repo(src_download_loc = src_download_loc,
                      mattlib_loc = mattlib_loc,
                      lock_file_loc = lock_file_loc)
