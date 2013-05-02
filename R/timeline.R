#' Creates a timeline plot.
#' 
#' @param df data for time periods.
#' @param df data for events.
#' @export
timeline <- function(df, events,
					 label.col = names(df)[1],
					 group.col = names(df)[2],
					 start.col = names(df)[3],
					 end.col = names(df)[4],
					 text.size = 4,
					 event.label.col = names(events)[1],
					 event.col = names(events)[2],
					 event.group.col = NULL,
					 event.spots = 1,
					 num.label.steps = 5,
					 event.label = '',
					 event.label.method = 1,
					 event.line = FALSE,
					 event.text.size = 4,
					 event.above = TRUE,
					 limits,
					 ...
) {	
	if(missing(limits)) {
		if(missing(events)) {
			limits <- range(c(df[,start.col], df[,end.col]), na.rm=TRUE)
		} else if(missing(df)) {
			limits <- range(events[,event.col], na.rm=TRUE)
		} else {
			limits <- range(c(df[,start.col], df[,end.col], events[,event.col]), na.rm=TRUE)
		}
	}
	
	groups <- unique(df[,group.col])
	xmin <- limits[1]
	xmax <- limits[2]
	
	ymin <- 0
	ymax <- length(groups)
	
	group.labels <- data.frame(group=groups, 
							   x=rep(xmin, length(groups)), 
							   y=rep(NA, length(groups)),
							   stringsAsFactors=FALSE)
	
	df$ymin <- df$ymax <- NA
	for(i in seq_along(groups)) {
		df[which(df[,group.col] == groups[i]),]$ymin <- 
			ifelse(event.above, 0, event.spots - 1) + i - event.above
		df[which(df[,group.col] == groups[i]),]$ymax <- 
			ifelse(event.above, 0, event.spots - 1) + i + !event.above
		group.labels[which(group.labels$group == groups[i]),]$y <- 
			ifelse(event.above, 0, event.spots - 1) + i + !event.above
	}
	df$labelpos <- (df$ymin + df$ymax) / 2
	
	steps <- rev(seq(0, event.spots, by=event.spots/(num.label.steps + 1))[2:(num.label.steps+1)])
	events$y <- ifelse(event.above, ymax, 0) + 
		rep(steps, ceiling(nrow(events)/length(steps)))[1:nrow(events)]
	
	group.labels <- rbind(group.labels, data.frame(group=event.label, x=xmin, 
							y=ifelse(event.above, ymax + 1, 1)))
	
	#Fix the dates that fall outside the range
	df[df[,start.col] < xmin & df[,end.col] > xmin, start.col] <- xmin
	df[df[,end.col] > xmax & df[,start.col] < xmax, end.col] <- xmax
	events <- events[events[,event.col] >= xmin & events[,event.col] <= xmax,]
	
	p <- ggplot()
	
	if(event.line) {
		p <- p + geom_segment(data=events, aes_string(x=event.col, xend=event.col, yend='y'),
							  y=ymin, alpha=1)
	}
	
	p <- p +
		geom_rect(data=df, aes_string(xmin=start.col, xmax=end.col,
		          ymin='ymin', ymax='ymax', fill=label.col), alpha=.9) +
		geom_text(data=df, aes_string(y='labelpos', x=start.col, label=label.col),
		          hjust=-0.05, size=text.size) +
		theme(legend.position='none',
			  axis.ticks.y=element_blank()) + 
		xlab('') + ylab('') +
		xlim(c(xmin, xmax)) +
		scale_y_continuous(breaks=group.labels$y-0.5, 
						   labels=group.labels$group,
						   limits=c(ymin, ymax + event.spots),
						   minor_breaks=c())
	if(!missing(events)) {
		if(missing(event.group.col)) {
			events$Group <- 'Group'
			event.group.col <- 'Group'
		}
		if(event.label.method == 1) {
			p <- p +
				geom_point(data=events, aes_string(x=event.col, y='y',
				     color=event.group.col)) +
				geom_text(data=events, aes_string(x=event.col, y='y', 
				     label=event.label.col, color=event.group.col), hjust=-0.05,
				     size=event.text.size)
		} else if(event.label.method == 2) {
			p <- p +
				geom_point(data=events, aes_string(x=event.col, color=event.group.col), 
				     y=ymax + 0.1) +
				geom_text(data=events, aes_string(x=event.col, label=event.label.col,
				     color=event.group.col), y=ymax, angle=45, vjust=-0.15, hjust=-0.15,
				     size=event.text.size)
		}
		if(length(unique(events[,event.group.col])) == 1) {
			p <- p + scale_color_grey()
		}
	}
	
	p <- p + geom_hline(yintercept=ifelse(event.above, 0, event.spots), size=1)
	
	return(p)
}
