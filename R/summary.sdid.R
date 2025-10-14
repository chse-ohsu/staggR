#' Summarize an sdid model
#'
#' @param object an `sdid_mdl` object.
#' @param ... passed through.
#' @return an object of class `summary.sdid_mdl`.
#' @exportS3Method summary sdid_mdl
summary.sdid_mdl <- function(object, ...) {
  out <- stats::summary.lm(object$mdl)
  class(out) <- c("summary.sdid_mdl", class(out))
  return(out)
}

# Pretty printer
#' @exportS3Method print summary.sdid_mdl
print.summary.sdid_mdl <- function(x, ...) {
  cat("<summary.sdid_mdl>\n")
  NextMethod()  # falls back to summary.lm printing
}
