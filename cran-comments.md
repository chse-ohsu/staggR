## Test environments
* local macOS, R 4.5.3
* win-builder (r-devel, r-release, r-oldrelease)

## R CMD CHECK results
0 errors | 0 warnings | 0 note

## Resubmission
This is a resubmission. Changes since last version (0.2.1):

* Fixed problem with `select_terms()` not properly sanitizing coefficient names
* Fixed bug (Issue 13) where if the user specifies referents manually, sdid() fails to omit referents for identification
