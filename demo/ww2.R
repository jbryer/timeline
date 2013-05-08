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
