#' Constructor for a tsi object
#'
#' @param df df containing times since intervention
#'
#' @return An object of class "tsi"

new_tsi <- function(df) {
  structure(
    list(data = df),
    class = "tsi"
  )
}
