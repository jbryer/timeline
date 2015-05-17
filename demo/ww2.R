data(ww2)
timeline(ww2, ww2.events)
timeline(ww2, ww2.events, event.spots=1, event.label='Milestones')
timeline(ww2, ww2.events, event.spots=2, event.label.method=2, event.label='Milestones')
timeline(ww2, ww2.events, event.spots=2, event.line=TRUE, event.label='Milestones')
timeline(ww2, ww2.events, event.spots=1, event.label='Milestones', event.group.col='Side')
timeline(ww2, ww2.events, event.spots=1, event.line=TRUE, 
		 event.label='Milestones', event.group.col='Side')

#Limit the range to only WWII
timeline(ww2, ww2.events, event.spots=1, limits=c(ww2[8,'StartDate'], ww2[8,'EndDate']))

#put the events below the line
timeline(ww2, ww2.events, event.spots=2, event.label='', event.above=FALSE)

#No events.
timeline(ww2, event.spots=2, event.label='', event.above=FALSE)

#Modify the ggplot2 express after returing
timeline(ww2, ww2.events, text.position='center', text.angle=45) +
	theme(axis.text.y=element_text(angle=90, hjust=.5))

# Flips coordinates and prints Text instead of Person
timeline(ww2, #ww2.events,
         color.col = "Person",
         label.col = "Text",
         text.position = "center",
         text.size = 4,
         text.angle = 0,
         num.label.steps = 3,
         event.spots = 4,
         event.label.method = 1,
         event.text.size = 4,
         event.label = '',
         event.line = TRUE,
         event.above = TRUE) + coord_flip()
