devtools::document()          # update documentation
devtools::build()             # build the tarball
devtools::check(args = "--as-cran")  # full CRAN check locally
devtools::check_win_release() # remote Windows CRAN check
