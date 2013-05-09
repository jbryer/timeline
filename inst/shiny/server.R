require(shiny)

shinyServer(function(input, output) {
	require(timeline)
	data(ww2)

	output$rcode <- renderText({
		paste0(
			"timeline(ww2, ww2.events,\n",
			"   text.size = ", input$text.size, ",\n",
			"   num.label.steps = ", input$num.label.steps, ",\n",
			"   event.spots = ", input$event.spots, ",\n",
			"   event.label.method = ", input$event.label.method, ",\n",
			"   event.text.size = ", input$event.text.size, ",\n",
			"   event.label = '", input$event.label, "',\n",
			"   event.line = ", input$event.line, ",\n",
			"   event.above = ", input$event.above, ")"
		)
	})
	
	output$plot <- renderPlot({
		p <- timeline(ww2, ww2.events, 
				 text.size = input$text.size,
				 num.label.steps = input$num.label.steps,
				 event.spots = input$event.spots,
				 event.label.method = input$event.label.method,
				 event.text.size = input$event.text.size,
				 event.line = input$event.line,
				 event.label = input$event.label,
				 event.above = input$event.above)
		print(p)
	}, height=300, width=800)
})
