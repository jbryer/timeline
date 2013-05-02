# timeline
#### Create timeline plots

	require(devtools)
	install_github('timeline','jbryer')
	require(timeline)
	data(ww2)
	timeline(ww2, ww2.events, event.spots=2, event.label='', event.above=FALSE)

![Timeline of World War II](http://jason.bryer.org/images/timeline/ww2.png)
