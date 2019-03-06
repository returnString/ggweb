
#' Chart Output
#'
#' @description Used to build the UI for a corresponding call to \code{renderChart}.
#'
#' @param outputID Output variable from which to read the plot.
#' @param width Plot width on the page.
#' @param height Plot height on the page.
#'
#' @export
chartOutput <- function(outputID, width = "100%", height = "400px") {
  deps <- htmltools::htmlDependency(
    name = "ggweb-assets",
    version = "0.1",
    package = "ggweb",
    src = "assets",
    script = c("js/Chart.bundle.min.js", "js/binding.js")
  )

  shiny::div(id = outputID, class = "shiny-ggweb-output", style = paste0("width: ", width, "; height: ", height), deps)
}

#' Chart Rendering
#'
#' @description Renders a reactive and interactve plot on an HTML canvas using Chart.js.
#'
#' @param expr An expression that generates a plot.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression?
#'
#' @export
renderChart <- function(expr, env = parent.frame(), quoted = F) {
  func <- shiny::exprToFunction(expr, env, quoted)

  function() {
    plot <- func()
    if (promises::is.promising(plot)) {
      promises::then(plot, ggplot_to_chartjs)
    } else {
      ggplot_to_chartjs(plot)
    }
  }
}
