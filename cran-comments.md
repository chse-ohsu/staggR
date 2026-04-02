## Test environments
* local macOS, R 4.5.2
* win-builder (r-devel, r-release, r-oldrelease)

## R CMD CHECK results
0 errors | 0 warnings | 0 note

## Resubmission
This is a resubmission. Changes since last version (0.1.0):
* Added detrend() and detrend_factor()
* Fixed bug: sdid() permits cohort factors with 0 observations, and that causes summary() to throw an error downstream
* Fixed bug: sdid() throws warnings or errors when supplied a column containing character strings as cohort

