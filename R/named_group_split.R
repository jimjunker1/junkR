#' named_group_split
#'
#' This function returns a named list split on a grouping variable. It is based on the comments here: https://github.com/tidyverse/dplyr/issues/4223#issuecomment-469269857
#' @param .tbl A tibble or tibble-convertable object
#' @param ... further arguments, including the variable/s by which to group_split and set names
#' @import rlang
#' @export
#' @examples
#' named_group_split()

named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))

  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}
