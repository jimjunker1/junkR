#' geom_isolineNP
#' This geom adds NP stoichiometric isolines to plot background for NP pools
#'
library(ggplot2)
GeomIsolineNP = ggproto("GeomIsoline", Geom,
                       required_aes = c("x","y"),
                       default_aes = aes(fill = 'red',  shape = 21, colour = "black", size = 2),
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)

                         ## Let's print out the structure of the 'coords' object
                         str(coords)

                         ## Construct a grid polyline grob
                         polylineGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = coords$shape
                         )
                       })

#' Stoichiometric isolines: diagonal
#'
#' This geoms add reference lines (sometimes called rules) to a plot.
#' These are useful for annotating plots.
#'
#' These geoms act slightly differently from other geoms. You can supply the
#' parameters in two ways: either as arguments to the layer function,
#' or via aesthetics. If you use arguments, e.g.
#' `geom_isoline(ratio = 30)`, then behind the scenes
#' the geom makes a new data frame containing just the data you've supplied.
#' That means that the lines will be the same in all facets; if you want them
#' to vary across facets, construct the data frame yourself and use aesthetics.
#'
#' Unlike most other geoms, these geoms do not inherit aesthetics from the plot
#' default, because they do not understand x and y aesthetics which are
#' commonly set in the plot. They also do not affect the x and y scales.
#'
#' @section Aesthetics:
#' These geoms are drawn using with [geom_line()] so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `size`. They also each have aesthetics that control the position of
#' the line:
#'
#'   - `geom_isolineNP()`: `ratio`
#'
#' @seealso See [geom_segment()] for a more general approach to
#'   adding straight line segments to a plot.
#' @inheritParams layer
#' @inheritParams geom_point
#' @param mapping Set of aesthetic mappings created by [aes()] or [aes_()].
#' @param ratio Parameters that control the
#'   position of the line. If these are set, `data`, `mapping` and
#'   `show.legend` are overridden.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Fixed values
#' p + geom_isoline(ratio = 5)
#'
#' p + geom_isoline() # Can't see it - outside the range of the data
#' p + geom_isoline(ratio = 20)
#'
#' # Redfield ratio N:P isoline 16:1 fit
#' p + geom_isoline(ratio = 16)
#' # But this is easier to do with geom_smooth:
#'
#' # To show different lines in different facets, use aesthetics
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   facet_wrap(~ cyl)
#'
#' mean_stoic <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
#' p + geom_isoline(aes(yintercept = wt), mean_wt)
#'
#' # You can also control other aesthetics
#' ggplot(mtcars, aes(mpg, wt, colour = wt)) +
#'   geom_point() +
#'   geom_hline(aes(yintercept = wt, colour = wt), mean_wt) +
#'   facet_wrap(~ cyl)
geom_isoline <- function(mapping = NULL, data = NULL,
                        ...,
                        ratio,
                        na.rm = FALSE,
                        show.legend = NA) {

  # If nothing set, default to y / x
  if (is.null(mapping) && missing(ratio)) {
    ratio <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {

    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_abline()", "mapping", c("slope", "intercept"))
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_abline()", "data", c("slope", "intercept"))
    }

    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0
    n_slopes <- max(length(slope), length(intercept))

    data <- new_data_frame(list(
      intercept = intercept,
      slope = slope
    ), n = n_slopes)
    mapping <- aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomAbline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAbline <- ggproto("GeomAbline", Geom,
                      draw_panel = function(data, panel_params, coord) {
                        ranges <- coord$backtransform_range(panel_params)

                        data$x    <- ranges$x[1]
                        data$xend <- ranges$x[2]
                        data$y    <- ranges$x[1] * data$slope + data$intercept
                        data$yend <- ranges$x[2] * data$slope + data$intercept

                        GeomSegment$draw_panel(unique(data), panel_params, coord)
                      },

                      default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                      required_aes = c("slope", "intercept"),

                      draw_key = draw_key_abline
)

warn_overwritten_args <- function(fun_name, overwritten_arg, provided_args, plural_join = " and/or ") {
  overwritten_arg_text <- paste0("`", overwritten_arg, "`")

  n_provided_args <- length(provided_args)
  if (n_provided_args == 1) {
    provided_arg_text <- paste0("`", provided_args, "`")
    verb <- "was"
  } else if (n_provided_args == 2) {
    provided_arg_text <- paste0("`", provided_args, "`", collapse = plural_join)
    verb <- "were"
  } else {
    provided_arg_text <- paste0(
      paste0("`", provided_args[-n_provided_args], "`", collapse = ", "),
      ",", plural_join,
      "`", provided_args[n_provided_args], "`"
    )
    verb <- "were"
  }

  warning(
    sprintf(
      "%s: Ignoring %s because %s %s provided.",
      fun_name,
      overwritten_arg_text,
      provided_arg_text,
      verb
    ),
    call. = FALSE
  )
}
