\name{timeline}
\alias{timeline}
\title{Creates a timeline plot.}
\usage{
  timeline(df, events, label.col = names(df)[1],
    group.col = names(df)[2], start.col = names(df)[3],
    end.col = names(df)[4], text.size = 4,
    text.color = "black", num.label.steps = 5,
    event.label.col, event.col, event.group.col,
    event.spots = 1, event.label = "",
    event.label.method = 1, event.line = FALSE,
    event.text.size = 4, event.above = TRUE, limits, ...)
}
\arguments{
  \item{df}{data for time periods.}

  \item{events}{data for events (optional).}

  \item{label.col}{the column name in \code{df} to use for
  labeling.}

  \item{group.col}{the column name in \code{df} to use for
  grouping.}

  \item{start.col}{the column name in \code{df} that
  specifies the start date.}

  \item{end.col}{the column name in \code{df} that
  specifies the end date.}

  \item{text.size}{the text size for labels in \code{df}.}

  \item{text.color}{the text color for labels in
  \code{df}.}

  \item{num.label.steps}{the number of steps to use for
  labeling events.}

  \item{event.label.col}{the column name in \code{events}
  to use for labeling.}

  \item{event.col}{the column name in \code{events} that
  specifies the date.}

  \item{event.group.col}{the column name in \code{events}
  to use for grouping.}

  \item{event.spots}{the number of rows to use for events.
  Note that each group in \code{df} is equal to one, so
  \code{event.spots = 2} would be twice as high as one
  group row from \code{df}.}

  \item{event.label}{the label to use on the x-axis for
  events.}

  \item{event.label.method}{the labeling method. For
  \code{method = 1} labels are printed horizontally; for
  \code{method = 2} labels are printed at 45 degree
  angles.}

  \item{event.line}{whether to draw a vertical line for
  each event.}

  \item{event.text.size}{the text size for event labels.}

  \item{event.above}{whether events should be plotted above
  (\code{TRUE}) or below (\code{FALSE}) time bars.}

  \item{limits}{the limits of the y-axis.}

  \item{...}{currently unused.}
}
\description{
  This function will create a timeline using the
  \code{ggplot2} framework in a style similar to
  \href{http://www.preceden.com/}{Preceden}. There are two
  types of events, those that have a range (i.e. a start
  and end date) and and points-in-time. The latter can be
  grouped in separate rows to prevent overlapping or to
  organize different types of events. Grouping of
  point-in-time events will color code those events
  accordingly.
}
\examples{
data(ww2)
timeline(ww2, ww2.events)
timeline(ww2, ww2.events, event.spots=2, event.label='', event.above=FALSE)
}

