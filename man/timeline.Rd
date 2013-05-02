\name{timeline}
\alias{timeline}
\title{Creates a timeline plot.}
\usage{
  timeline(df, events, label.col = names(df)[1],
    group.col = names(df)[2], start.col = names(df)[3],
    end.col = names(df)[4], text.size = 4,
    event.label.col = names(events)[1],
    event.col = names(events)[2], event.group.col = NULL,
    event.spots = 1, num.label.steps = 5, event.label = "",
    event.label.method = 1, event.line = FALSE,
    event.text.size = 4, event.above = TRUE, limits)
}
\arguments{
  \item{df}{data for time periods.}

  \item{df}{data for events.}
}
\description{
  Creates a timeline plot.
}

