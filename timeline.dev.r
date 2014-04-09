require(ggplot2)
require(devtools)

setwd('~/Dropbox/Projects')

document('timeline')
check_doc('timeline')
install('timeline')
check('timeline')


require(shiny)
shiny::runApp('timeline/inst/shiny') #Shiny app locally

require(timeline)
demo(ww2)
timelineShinyDemo()

##### Data Setup ###############################################################
##### R History
library(XML)
library(lattice)

#http://stackoverflow.com/questions/13567453/how-to-scrape-the-web-for-the-list-of-r-release-dates
getRdates <- function(){
	url <- paste0("http://cran.r-project.org/src/base/R-", 0:3)
	x <- lapply(url, function(x)readHTMLTable(x, stringsAsFactors=FALSE)[[1]])
	x <- do.call(rbind, x)
	x <- x[grep("R-(.*)(\\.tar\\.gz|\\.tgz)", x$Name), c(-1, -5)]
	x$Release <- gsub("(R-.*)\\.(tar\\.gz|tgz)", "\\1", x$Name)
	x$Date <- as.POSIXct(x[["Last modified"]], format="%d-%b-%Y %H:%M")
	x$Release <- reorder(x$Release, x$Date)
	x
}

releases <- getRdates()
releases$Major <- substr(rhistory$Release,3,3)

#https://www.stat.auckland.ac.nz/~ihaka/downloads/Massey.pdf


##### World War 2
ww2 <- as.data.frame(matrix(c(
	'Franklin D. Roosevelt','US President','1933-03-04','1945-04-12',
	'Harry S. Truman','US President','1945-04-12','1953-01-20',
	'Stanley\nBaldwin','UK Prime Minister','1935-06-07','1937-05-28',
	'Neville\nChamberlain','UK Prime Minister','1937-05-28','1940-05-10',
	'Winston Churchill','UK Prime Minister','1940-05-10','1945-07-26',
	'Clement Attlee','UK Prime Minister','1945-07-26','1951-10-26',
	'Winston Churchill','UK Prime Minister','1951-10-26','1955-04-07',
	'World War II','World War II','1939-09-01','1945-09-02'), 
							ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
names(ww2) <- c('Person','Group','StartDate','EndDate')
ww2$StartDate <- as.Date(ww2$StartDate)
ww2$EndDate <- as.Date(ww2$EndDate)

ww2.events <- as.data.frame(matrix(c(
	'Japan Invades Manchuria','1931-09-18','Axis',
	'Japan Invades China','1937-07-07','Axis',
	'Germany Invades Poland','1939-09-01','Axis',
	'Attack on Pearl Harbor','1941-12-07','Axis',
	' D-Day','1944-06-06','Allies',
	'Bombing of Hiroshima','1945-08-06','Allies',
	'Bombing of Nagasaki','1945-08-09','Allies',
	'Japan Formally Surrenders','1945-09-02','Axis'),
								   ncol=3, byrow=TRUE), stringsAsFactors=FALSE)
names(ww2.events) <- c('Event','Date','Side')
ww2.events$Date <- as.Date(ww2.events$Date)

save(ww2, ww2.events, file='timeline/data/ww2.rda')


# Convert three numeric vectors to a Date. If y is NA, the date is NA. If m or
# d is NA, it will assumed to be 1 (i.e. January of 1st of the month, respectively).
toDate <- function(y, m, d) {
	m[is.na(m)] <- 1
	d[is.na(d)] <- 1
	dates <- paste(y, m, d, sep='-')
	dates[is.na(y)] <- NA
	dates <- as.Date(dates)
	return(dates)
}

wars.extra <- read.csv('~/Dropbox/Projects/Data/Correlates of War/Extra-StateWarData_v4.0.csv',
				 stringsAsFactors=FALSE)
for(i in 1:ncol(wars.extra)) {
	wars.extra[which(wars.extra[,i] < 0),i] <- NA
}
wars.extra$StartDate1 <- toDate(y=wars.extra$StartYear1, m=wars.extra$StartMonth1, d=wars.extra$StartYear1)
wars.extra$EndDate1 <- toDate(y=wars.extra$EndYear1, m=wars.extra$EndMonth1, d=wars.extra$EndYear1)
wars.extra$StartDate2 <- toDate(y=wars.extra$StartYear2, m=wars.extra$StartMonth2, d=wars.extra$StartYear2)
wars.extra$EndDate2 <- toDate(y=wars.extra$EndYear2, m=wars.extra$EndMonth2, d=wars.extra$EndYear2)

wars.inter <- read.csv('~/Dropbox/Projects/Data/Correlates of War/Inter-StateWarData_v4.0.csv',
				 stringsAsFactors=FALSE)
for(i in 1:ncol(wars.inter)) {
	wars.inter[which(wars.inter[,i] < 0),i] <- NA
}
wars.inter$StartDate1 <- toDate(y=wars.inter$StartYear1, m=wars.inter$StartMonth1, d=wars.inter$StartYear1)
wars.inter$EndDate1 <- toDate(y=wars.inter$EndYear1, m=wars.inter$EndMonth1, d=wars.inter$EndYear1)
wars.inter$StartDate2 <- toDate(y=wars.inter$StartYear2, m=wars.inter$StartMonth2, d=wars.inter$StartYear2)
wars.inter$EndDate2 <- toDate(y=wars.inter$EndYear2, m=wars.inter$EndMonth2, d=wars.inter$EndYear2)

names(wars.extra)[5] <- 'Side'
wars <- rbind(wars.extra[,c('WarNum','WarName','WarType','Side','StartDate1','EndDate1')], 
			  wars.inter[,c('WarNum','WarName','WarType','Side','StartDate1','EndDate1')])

presidents$EndDate <- as.Date(NA)
for(i in 1:(nrow(presidents)-1)) {
	presidents[i,]$EndDate <- presidents[i+1,]$Inauguration
}
presidents[nrow(presidents),]$EndDate <- presidents[nrow(presidents),]$Inauguration + 4 * 365.25

save(wars, presidents, file='timeline/data/wars.rda')

