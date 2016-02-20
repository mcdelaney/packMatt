
lock_file_loc <- "tests/mattpack.lock"
src_download_loc <- "tests/src_test"
mattlib_loc = "tests/mattlib"

library(RPostgreSQL)
library(dplyr)

freeze_packages(lock_file_loc = lock_file_loc)
thaw_results <- thaw_mattpack(lock_file_loc = lock_file_loc,
                              src_download_loc = src_download_loc)

install_mattpack_repo(src_download_loc = src_download_loc,
                      mattlib_loc = mattlib_loc,
                      lock_file_loc = lock_file_loc)
