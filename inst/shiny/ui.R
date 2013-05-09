require(shiny)
require(ggplot2)

shinyUI(pageWithSidebar(
	# Application title
	headerPanel("Timeline of World War II"),
	
	# Sidebar with controls to select the variable to plot against mpg
	# and to specify whether outliers should be included
	sidebarPanel(
		helpText(paste0("Timeline Parameters:")),
		sliderInput("text.size", "Text Size:", 
					min=1, max=18, value=4),
		sliderInput("num.label.steps", "Number of steps for labeling:",
		            min=1, max=10, value=5),
		sliderInput("event.spots", "Number of spots for labeling:",
					min=1, max=20, value=1),
		sliderInput("event.text.size", "Event Text Size:", 
					min=1, max=18, value=4),
		textInput("event.label", "Event Group Label:", value=''),
		selectInput("event.label.method", "Labeling method:",
					list("Horizontal" = 1, 
						 "Angled" = 2, 
						 "Vertical" = 3)),
		checkboxInput("event.line", "Event lines:", FALSE),
		checkboxInput("event.above", "Event above:", FALSE)
	),
	
	# Show the caption and plot of the requested variable against mpg
	mainPanel(
		tabsetPanel(
			tabPanel("Plot",
					 plotOutput("plot")
			),
			tabPanel("R Code", 
					 verbatimTextOutput("rcode")
			)
		)
	)
))
