#from https://github.com/htmlwidgets/sparkline/blob/aefe3a1e38bf81bd7ed23aae1503212403bad6ff/R/sparkline.R

#' @export
#' @import htmltools
#' @import htmlwidgets
sparkline <- function(values, ..., width = 60, height = 20){
  params = list(
    values = values, 
    options = list(..., height = height, width = width), 
    width = width , height = height
  )
  params = Filter(Negate(is.null), params)
  htmlwidgets::createWidget('sparkline', params,
                            width = width,
                            height = height,
                            sizingPolicy = htmlwidgets::sizingPolicy(
                              viewer.fill = FALSE 
                            )
  )
}

#' @export
sparkline_html <- function(id, style, class, ...){
  tags$span(id = id, class = class)
}

#' @export
sparklineOutput <- function(outputId, width = "60px", height = "20px") {
  htmlwidgets::shinyWidgetOutput(outputId, "sparkline", width, height)
}

#' @export
renderSparkline <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, sparklineOutput, env, quoted = TRUE)
}