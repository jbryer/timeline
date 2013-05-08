terms <- as.data.frame(matrix(c(
	'Term A', 'Year 1', 0.5, 4.5,
	'Term B', 'Year 1', 4.5, 8.8,
	'Term C', 'Year 1', 8.5, 12.5
	), ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
names(terms) <- c('Desc', 'FY', 'Start', 'End')
terms$Start <- as.integer(terms$Start)
terms$End <- as.integer(terms$End)

events <- as.data.frame(matrix(c(
	'Student Enrolls', 2,
	'Retention', 15
	), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
names(events) <- c('Event', 'Time')
events$Time <- as.integer(events$Time)

timeline(terms, events)
