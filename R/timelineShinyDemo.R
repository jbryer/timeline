#' Runs the ww2 demo as a Shiny app.
#' 
#' This will start a Shiny server to interact with the different options available
#' in the \code{\link{timeline}} function.
#' 
#' @export
timelineShinyDemo <- function() {
	message('Hit <escape> to stop')
	require(shiny)
	shiny::runApp(system.file('shiny', package='timeline'))
}
