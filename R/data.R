#' My own personal Garmin Connect data over a two year period which spanned the COVID19-induced lockdown
#'
#' This is literally the raw output that came from navigating onto Garmin Connect and hitting "export to csv"
#' Quite simply I then just read the data into R as below:
#' garmin <- read.csv("Activities-2.csv", header=T)
#'
#'
#' @format A data frame
#' \describe{
#'  \item{Activity.Type}{Garmin connect annotates walking, running, cycling and cardio}
#'  \item{Training.Stress.Score.}{There are a number of variables that are not annotatred by my Garmin (Forerunner 35). More advanced watches might be required for some of this data to become relevant.}
#' }
#' @source
"garmin"
