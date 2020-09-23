#' @title processGarminRunning
#'
#' @description A series of functions to process, analyse and plot Garmin Connect data.
#'
#' @param data downloaded from Garmin Connect in .csv format and read into R using read.csv or read.table for example.
#'
#' @return dataframe
#'
#' @examples
#' #let's process my data so that it's good for R, and tidy for plotting
#' my_runs <- processGarminRunning(data=garmin)
#' summary(my_runs)
#'
#' @export


processGarminRunning <- function(data){

  #check if the required packages are installed
  packages = c("tidyverse", "lubridate")

  ## Now load or install
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  #check if it's a data frame
  if(is.matrix(data)){
    data <- as.data.frame()
  }

  if(is.data.frame(data)){

    #get running data
    garmin <- data %>% filter(Activity.Type == "Running")

    #now drop variables we aren't interested in
    garmin <- garmin %>%
      dplyr::select(-Max.Temp, -Decompression, -Surface.Interval, -Min.Temp, -Bottom.Time, -Flow, -Training.Stress.Score., -Avg.Vertical.Oscillation, -Avg.Vertical.Ratio)

    # format Date column to POSIXct
    garmin$Date <- as.POSIXct(strptime(garmin$Date, format = "%Y-%m-%d %H:%M:%S"))

    # format Avg.Pace to POSIXct
    garmin$Avg.Pace <- as.POSIXct(strptime(garmin$Avg.Pace, format = "%M:%S"))

    # format Time to POSIXct
    garmin$Time <- as.POSIXct(strptime(garmin$Time, format = "%H:%M:%S"))
    garmin$Best.Lap.Time <- as.POSIXct(strptime(garmin$Best.Lap.Time, format = "%H:%M.%S"))

    #convert calories to numeric
    garmin$Calories <- gsub(",","",garmin$Calories)
    garmin$Calories <- gsub("--", NA, garmin$Calories)
    garmin$Calories <- as.numeric(garmin$Calories)

    #convert cadence to a numeric
    garmin$Avg.Run.Cadence <- gsub("--", NA, garmin$Avg.Run.Cadence) %>% as.numeric()
    garmin$Max.Run.Cadence <- gsub("--", NA, garmin$Max.Run.Cadence) %>% as.numeric()

    #make Best Pace variable a POSIXCT
    garmin$Best.Pace <- as.POSIXct(strptime(garmin$Best.Pace, format = "%M:%S"))

    #turn elevation gains into as numeric
    garmin$Elev.Gain <- gsub("--", NA, garmin$Elev.Gain) %>% as.numeric()
    garmin$Elev.Loss <- gsub("--", NA, garmin$Elev.Loss) %>% as.numeric()

    #make climb time variable a POSIXCT
    garmin$Climb.Time <- as.POSIXct(strptime(garmin$Climb.Time, format = "%M:%S"))

    # factorise the distances travelled
    garmin <- garmin %>%
      mutate(distance_grouping = factor(case_when(Distance <= 1 ~ "0-1 mile",
                                                  Distance > 1 & Distance <= 2 ~ "1-2 miles",
                                                  Distance > 2 & Distance <= 3 ~ "2-3 miles",
                                                  Distance > 3 & Distance <= 4 ~ "3-4 miles",
                                                  Distance > 4 & Distance <= 5 ~ "4-5 miles",
                                                  Distance > 5 & Distance <= 10 ~ "5-10 miles",

                                                  Distance > 10 & Distance <= 15 ~ "10-15 miles",
                                                  Distance > 15 & Distance <= 20 ~ "15-20 miles",
                                                  Distance > 20 ~ ">20 miles"), levels=c("0-1 mile", "1-2 miles",
                                                                                         "2-3 miles", "3-4 miles",
                                                                                         "4-5 miles", "5-10 miles",
                                                                                         "10-15 miles", "15-20 miles",
                                                                                         ">20 miles")))
    #deal with number of laps
    garmin$Number.of.Laps <- as.numeric(garmin$Number.of.Laps)

    #if there are any bugs we will be able to tell by the Calories - remove rows in which calories are NA
    #or if the distance is very small
    if(sum(is.na(garmin$Calories)) > 0){
      garmin <- garmin[-which(is.na(garmin$Calories)),]
    }

    if(min(garmin$Distance) < 0.1){
      garmin <- garmin[-which(garmin$Distance < 0.1),]
    }

    #reaffirm factor variables
    garmin$Title <- as.factor(as.character(garmin$Title))

    #change location
    garmin$location <- word(garmin$Title, 1) %>% as.character() %>% as.factor()

    garmin$location <- factor(ifelse(garmin$location == "Running", "Unknown", as.character(garmin$location)))

    #edit heart rate
    garmin$Avg.HR <- gsub("--", NA, garmin$Avg.HR) %>% as.numeric()
    garmin$Max.HR <- gsub("--", NA, garmin$Max.HR) %>% as.numeric()




    garmin <- as.data.frame(garmin)

  }else{
    stop("is neither a data frame nor a matrix. Try reading in the Garmin Connect output .csv file again")
  }

  return(garmin)

}



#' @title overview_individual_runs
#'
#' @description A function to generate stylish plots that rapidly summarise your running performance.
#'
#' @param output from the processGarminRunning function
#' @param date_from a cut-off for runs that you wish to look at in the format e.g as.Date("2020-03-18")
#' @param plot can be one of "distance", "pace", "dist_pace", "dist_time", "dist_cals", "cumulative_dist",
#' "pace_per_dist.gp","heart.rate_dist.gp","cadence_dist.gp", "elevation", "elevation_by_dist"
#' @param target_pace your half marathon time for example or a target pace that you want to run at in the format "8:00" "minutes:seconds".
#' @return plot
#'
#' @examples
#' #let's process my data so that it's good for R, and tidy for plotting
#' my_runs <- processGarminRunning(data=garmin)
#'
#' # I'm firstly interested in how far I have ran since lockdown (COVID19 induced)
#' overview_individual_runs(my_runs,
#'                   plot = "cumulative_dist",
#'                 date_from = as.Date("2020-03-18"))
#'
#' # I can now clearly visualise how far I have run but what are the exact distances of those individual runs?
#' overview_individual_runs(my_runs,
#'                  plot = "dist_time",
#'                  date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #there is a minor trend for runs to increase in distance particularly since moving to Manchester.
#' #early on during lockdown I ran a lot of the same run you'll notice - around 2.5m miles
#'
#' #we can separate by location to look at the distance of the runs
#'
#' overview_individual_runs(my_runs,
#'                 plot = "distance",
#'                  date_from = as.Date("2020-03-18"))
#'
#' #there is a great degree of variation amongst the manchester runs - for some reason salford and manchester are separated here
#'
#' #what about pace? How fast have I been running? Compare this to my per mile half marathon time
#' overview_individual_runs(my_runs,
#'                  plot = "pace",
#'                 date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #You'll notice that on my longer runs I am way behind my half marathon pace - oh dear!
#' #There are a bunch of short distance runs in which I'm considerably under.
#'
#' overview_individual_runs(my_runs,
#'                  plot = "dist_pace",
#'                  date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #we can also separate according to distance grouping to monitor our face. Seemingly there are a bunch of short distance, but slow runs between 1 and 2 miles.
#' #lots of variation in the 3-4 mile range
#' overview_individual_runs(my_runs,
#'                  plot = "pace_per_dist.gp",
#'                  date_from = as.Date("2020-03-18"),
#'                  target_time = "7:18")
#'
#' #what about calories? how many calories am I burning in these runs?
#' overview_individual_runs(my_runs,
#'                  plot = "dist_cals",
#'                  date_from = as.Date("2020-03-18"))
#'
#'
#' #very strong relationship between calories and distance run - strange wobble at the bottom  may be related to pace!
#' #what about heart rate?
#' overview_individual_runs(my_runs,
#'                  plot = "heart.rate_dist.gp",
#'                  date_from = as.Date("2020-03-18"))
#'
#' # you'll notice in the longer runs that the average heart rate for each run is above the median value of average heart rates across all runs.
#'
#' #What about cadence?
#' overview_individual_runs(my_runs,
#'                 plot = "cadence_dist.gp",
#'                 date_from = as.Date("2020-03-18"))
#'
#' @export



overview_individual_runs <- function(data,
                              date_from = NA,
                              plot = "distance",
                              target_time = median(data$Avg.Pace)){

  if(!plot %in% c("distance", "pace", "dist_pace", "dist_time", "dist_cals", "cumulative_dist","pace_per_dist.gp","heart.rate_dist.gp","cadence_dist.gp","dist_time.bar", "elevation", "elevation_by_dist")){
    stop("The plot you request is not part of the repertoire")
  }

  #check if the required packages are installed
  packages = c("tidyverse", "lubridate", "RColorBrewer", "ggridges", "dplyr","ggpubr")

  ## Now load or install
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  #get some colours
  pal = c(brewer.pal(8, "Dark2"), brewer.pal(8,"Paired"))
  pal2 = c(brewer.pal(11, "Spectral"))

  #get the runs SINCE the date specified
  if(!is.na(date_from)){

    data <- data[which(data$Date > date_from),]
  }

  data$location <- as.factor(as.character(data$location))

  if(plot == "distance"){
    # distance
    plot = ggplot(data, aes(x=Distance, y=location, fill=location))+
      geom_density_ridges(scale=1, jittered_points=TRUE, point_size=2, point_alpha=0.8,point_colour="white")+
      scale_fill_manual(values=pal)+
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        plot.title = element_text(colour="white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_blank(),
        axis.line = element_line(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        legend.position = c(0.8, 0.9)
      )+
      labs(x="Distance (miles)", title="Run distance density")


  }

  if(plot == "pace"){
    # average pace
    plot = ggplot(data, aes(x = Date,y = Avg.Pace, colour = distance_grouping)) +
      geom_point(size=5, alpha=0.8) +
      scale_colour_manual(values=pal2)+
      scale_y_datetime(date_labels = "%M:%S") +
      #scale_x_date(date_labels = "%Y-%M-%d")+
      geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)", title = "Average pace") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="", y="Average Pace (min/mile)")+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")
    #annotate("text", label = "Target", xmin= min(data$Data), ymin=as.POSIXct(strptime(target_time, format = "%M:%S")), colour = "white")


  }

  if(plot == "dist_pace"){
    plot = ggplot(data, aes(Distance, Avg.Pace))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values=pal)+
      scale_y_datetime(date_labels = "%M:%S")+
      geom_smooth(colour="yellow",se=FALSE)+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        #legend.position = c(0.8, 0.6)
      )+
      labs(x="Distance (miles)", y="Average Pace (min/mile)",title="Distance - pace relationship")+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")
  }

  if(plot =="dist_time"){
    plot = ggplot(data, aes(x = Date,y = Distance, colour = distance_grouping)) +
      geom_point(size=5, alpha=0.8, aes(shape = location)) +
      #geom_segment(x=Date, y=0, yend=Distance, colour="white")+
      scale_colour_manual(values=pal2)+
      geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(fill = NA, colour = "#333333"),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="", y="Distance (miles)", title = "Run distance scatter plot")

  }

  if(plot =="dist_time.bar"){
    plot = ggplot(data, aes(x = Date,y = Distance)) +
      geom_bar(aes(fill=distance_grouping), stat="identity") +
      scale_fill_manual(values=pal2)+
      geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="", y="Distance (miles)",title="Run distances")

  }

  if(plot=="dist_cals"){

    plot = ggplot(data, aes(Distance, Calories))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values = pal)+
      #scale_y_datetime(date_labels = "%M:%S")+
      geom_smooth(colour="yellow",se=FALSE)+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        legend.position = c(0.8, 0.2)
      )+
      labs(x="Distance (miles)", y="Calories", title="Calories by distance")

  }

  if(plot == "cumulative_dist"){

    data <- data %>%
      arrange(Date) %>%
      mutate(cumulative_dist = cumsum(Distance))


    plot = ggplot(data, aes(Date, cumulative_dist))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values=pal)+
      geom_line(colour="yellow")+
      geom_hline(yintercept = 100, linetype="dashed", colour="yellow")+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        plot.title = element_text(colour="white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white"),
        legend.position = c(0.8, 0.2)
      )+
      labs(x="Date", y="Cumulative distance (miles)", title="Cumulative running distance")



  }

  if(plot == "pace_per_dist.gp"){


    plot = ggplot(data, aes(x = Date,y = Avg.Pace, colour = location)) +
      geom_point(size=5, alpha=0.8) +
      scale_colour_manual(values=pal)+
      scale_y_datetime(date_labels = "%M:%S") +
      #geom_smooth(color = "yellow", se=FALSE) +
      labs(x = "Date", y = "Average Pace (min/km)", title = "Average pace per distance grouping") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        #legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="Distance (miles)", y="Average Pace (min/mile)")+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")+
      facet_wrap(.~distance_grouping)
  }

  if(plot == "heart.rate_dist.gp"){

    dlong <- data %>%
      dplyr::select(Date, Avg.HR, Max.HR, distance_grouping) %>%
      pivot_longer(cols=c(Avg.HR, Max.HR),
                   values_to = "hr",
                   names_to = "avg.max")
    dlong$hr <- as.numeric(dlong$hr)
    dlong$Date <- as.POSIXct(strptime(dlong$Date, format = "%Y-%m-%d %H:%M:%S"))


    plot = ggplot(dlong, aes(x = Date,y = hr, colour = avg.max)) +

      geom_line(aes(group = Date),colour="yellow",size=1)+
      geom_point(size=6, alpha=0.8) +
      scale_colour_manual(values=pal)+

      labs(x = "Date", y = "Heart rate",title="Heart rate by grouping") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        plot.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      geom_hline(yintercept = mean(dlong$hr[dlong$avg.max == "Avg.HR"]),size=1, linetype="dashed", colour = pal[1])+
      geom_hline(yintercept = mean(dlong$hr[dlong$avg.max == "Max.HR"]),size=1, linetype="dashed", colour = pal[2])+
      facet_wrap(.~distance_grouping)
  }

  if(plot == "cadence_dist.gp"){

    dlong <- data %>%
      dplyr::select(Date, Avg.Run.Cadence, Max.Run.Cadence, distance_grouping) %>%
      pivot_longer(cols=c(Avg.Run.Cadence, Max.Run.Cadence),
                   values_to = "cadence",
                   names_to = "avg.max")
    dlong$cadence <- as.numeric(dlong$cadence)
    dlong$Date <- as.POSIXct(strptime(dlong$Date, format = "%Y-%m-%d %H:%M:%S"))


    plot = ggplot(dlong, aes(x = Date,y = cadence, colour = avg.max)) +

      geom_line(aes(group = Date),colour="yellow",size=1)+
      geom_point(size=6, alpha=0.8) +
      scale_colour_manual(values=pal)+

      labs(x = "Date", y = "Cadence",title = "Cadence by distance grouping") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      geom_hline(yintercept = mean(dlong$cadence[dlong$avg.max == "Avg.Run.Cadence"]),size=1, linetype="dashed", colour = pal[1])+
      geom_hline(yintercept = mean(dlong$cadence[dlong$avg.max == "Max.Run.Cadence"]),size=1, linetype="dashed", colour = pal[2])+
      facet_wrap(.~distance_grouping)

    if(plot=="elevation"){

      plot1 = ggplot(data, aes(Distance, Elev.Gain))+
        geom_point(aes(colour=location), size=5, alpha=0.8) +
        scale_colour_manual(values = pal)+
        #scale_y_datetime(date_labels = "%M:%S")+
        geom_smooth(colour="yellow",se=FALSE)+
        theme(
          panel.background = element_rect(fill = "#333333", colour = "#333333",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                          colour = "white"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#333333", colour = "white"),
          axis.text.x = element_text(colour = "white", angle=45, hjust=1),
          axis.text.y = element_text(colour = "white"),
          axis.ticks = element_line(colour = "white"),
          axis.title.x = element_text(colour="white"),
          axis.title.y=element_text(colour="white"),
          axis.line = element_line(colour="white"),
          plot.title = element_text(colour="white"),
          legend.background = element_rect(fill = "#333333", color = NA),
          legend.key = element_rect(color = "#333333", fill = NA),
          legend.title = element_blank(),
          legend.text = element_text(color = "white"),
          legend.position = "none"
        )+
        labs(x="Distance (miles)", y="Elev.Gain", title="Elevation by distance")

      data$dist_norm_elev.gain <- data$Elev.Gain / data$Distance

      plot2 = ggplot(data, aes(Avg.Pace, dist_norm_elev.gain))+
        geom_point(aes(colour=location), size=5, alpha=0.8) +
        scale_colour_manual(values = pal)+
        scale_x_datetime(date_labels = "%M:%S")+
        geom_smooth(colour="yellow",se=FALSE)+
        theme(
          panel.background = element_rect(fill = "#333333", colour = "#333333",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                          colour = "white"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#333333", colour = "white"),
          axis.text.x = element_text(colour = "white", angle=45, hjust=1),
          axis.text.y = element_text(colour = "white"),
          axis.ticks = element_line(colour = "white"),
          axis.title.x = element_text(colour="white"),
          axis.title.y=element_text(colour="white"),
          axis.line = element_line(colour="white"),
          plot.title = element_text(colour="white"),
          legend.background = element_rect(fill = "#333333", color = NA),
          legend.key = element_rect(color = "#333333", fill = NA),
          legend.title = element_blank(),
          legend.text = element_text(color = "white")
        )+
        labs(x="Average Pace (mins/mile)", y="Elevation gain / distance (miles)", title="Distance-normalised elevation gain")

      plot = ggarrange(plot1, plot2)

    }

    if(plot=="elevation_by_dist"){
    plot = ggplot(data, aes(Avg.Pace, Elev.Gain))+
      geom_point(aes(colour=location), size=5, alpha=0.8) +
      scale_colour_manual(values = pal)+
      scale_x_datetime(date_labels = "%M:%S")+
      geom_smooth(colour="yellow",se=FALSE)+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title.x = element_text(colour="white"),
        axis.title.y=element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      labs(x="Average Pace (mins/mile)", y="Elevation gain", title="Distance-normalised elevation gain")+
      facet_wrap(.~distance_grouping)
    }

  }




  return(plot)
}

#' @title overview_all_runs
#'
#' @description A function to generate stylish plots that rapidly give an overview of your running.
#'
#' @param output from the processGarminRunning function
#' @param date_from a date processed by as.Date e.g as.Date("2020-03-18"). This is a cut-off data for your analysis. Everything after this will be included in the analysis.
#' @param plot can be one of "runs_per_loc_per_dist", "distance_per_location", "mean_pace", "pca_plot", and "total_runs"
#' @param target_time a pace in the form "7:30" (character variable) with "minutes:seconds" that is one previously achieved that you wish to mark on your plots, OR it could be a target pace you have set yourself
#' @return plot
#'
#' @examples
#' #let's process my data so that it's good for R, and tidy for plotting
#' my_runs <- processGarminRunning(data=garmin)
#'
#' #What about my pace - calculate my mean avg.pace amongst all my runs per location
#'
#' overview_all_runs(my_runs,
#'                      date_from = as.Date("2020-03-18"),
#'                      plot = "mean_pace",
#'                      target_time = "7:18")
#'
#' #Note the 5-10 mile runs the pace is slightly faster in manchester than in Bromsgrove probably because of the hills!
#'
#' # we can also utilise numeric data to do PCA analysis - what are the major sources of variation in our running data?
#' overview_all_runs(my_runs,
#'                      date_from = as.Date("2020-03-18"),
#'                      plot = "pca_plot",
#'                      target_time = "7:18")
#'
#' @export


overview_all_runs <- function(data,
                                  date_from = NA,
                                  plot,
                                  target_time = median(data$Avg.Pace)){

  if(!plot %in% c("runs_per_loc_per_dist", "mean_pace", "distance_per_location", "total_runs", "pca_plot")){
    stop("The plot you request is not part of the repertoire")
  }

  #check if the required packages are installed
  packages = c("tidyverse", "lubridate", "RColorBrewer", "ggridges", "dplyr","ggpubr", "ggfortify", "caret")

  ## Now load or install
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  #get some colours
  pal = c(brewer.pal(8, "Dark2"), brewer.pal(8,"Paired"))
  pal2 = c(brewer.pal(11, "Spectral"))

  #get the runs SINCE the date specified
  if(!is.na(date_from)){

    data <- data[which(data$Date > date_from),]
  }

  data$location <- as.factor(as.character(data$location))


  loc_dist <-  data %>%
    group_by(location, distance_grouping) %>%
    summarise(n = n(), mean.pace = mean(Avg.Pace))

  if(plot == "runs_per_loc_per_dist"){

    plot = ggplot(loc_dist, aes(location, n))+
      geom_bar(aes(fill = distance_grouping), stat = "identity")+
      scale_fill_manual(values=pal2)+

      labs(x = "", y = "Number of runs",title = "Runs per location per distance") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )
  }

  if(plot == "mean_pace"){

    plot = ggplot(loc_dist, aes(distance_grouping, mean.pace))+
      geom_point(aes(colour = location),size=4,alpha=0.8)+
      scale_colour_manual(values=pal)+
      scale_y_datetime(date_labels = "%M:%S") +
      labs(x = "", y = "Within distance mean pace",title = "Mean pace per distance per location") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      geom_hline(yintercept = as.POSIXct(strptime(target_time, format = "%M:%S")), linetype="dashed", colour="white")
  }

  tot_dist <-  data %>%
    group_by(location) %>%
    summarise(total_dist = sum(Distance))

  if(plot == "distance_per_location"){

    plot = ggplot(tot_dist, aes(location, total_dist))+

      geom_segment(aes(x=location, xend=location, y=0, yend=total_dist), colour="yellow")+
      geom_point(aes(colour = location), stat = "identity", size=8, alpha=0.8)+
      scale_colour_manual(values=pal)+

      labs(x = "", y = "Total distance run",title = "Cumulative distance per location") +
      theme_bw()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        panel.border = element_rect(colour="white", size=1),
        legend.position = "none"
      )
  }

  runs <-  data %>%
    group_by(Activity.Type, distance_grouping) %>%
    summarise(n = n())

  if(plot == "total_runs"){

    plot = ggplot(runs, aes(Activity.Type, n))+
      geom_bar(aes(fill = distance_grouping), stat = "identity")+
      scale_fill_manual(values=pal2)+

      labs(x = "", y = "Total runs",title = "Total runs per distance group") +
      theme_minimal()+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        #panel.border = element_rect(colour="white", size=1),

        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(color = "white")
      )+
      coord_flip()
  }

  if(plot == "pca_plot"){

    ndata <- data %>%
      dplyr::select_if(is.numeric)

    #next get rid of runs with too many NA
    if(length(which(apply(ndata, 1, function(x) sum(is.na(x))) > ncol(ndata)/2)) > 0){
      ndata <- ndata[-which(apply(ndata, 1, function(x) sum(is.na(x))) > ncol(ndata)/2),]
    }

    #drop columns with zero variance
    nzv <- caret::nearZeroVar(ndata)
    if(length(nzv) > 0){
      ndata <- ndata[,-nzv]
    }

    #if there are NAs try imputing these
    if(sum(is.na(ndata)) > 0){
      pred_obj <- caret::preProcess(ndata, method="knnImpute")
      ndata <- predict(pred_obj, ndata)
    }


    plot = autoplot(prcomp(ndata), colour = "Distance", point.size=4, loadings=TRUE, loadings.label=TRUE, loadings.label.colour = "white")+
      theme_bw()+
      labs(title = "Runs: principal components analysis") +
      scale_colour_gradient(low = "yellow", high = "red")+
      theme(
        panel.background = element_rect(fill = "#333333", colour = "#333333",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#333333", colour = "white"),
        axis.text.x = element_text(colour = "white", angle=45, hjust=1),
        axis.text.y = element_text(colour="white"),
        axis.ticks = element_line(colour = "white"),
        axis.title = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        plot.title = element_text(colour="white"),
        strip.background = element_rect(fill="yellow"),
        strip.text = element_text(colour="black"),
        #panel.border = element_rect(colour="white", size=1),

        legend.direction = "vertical",
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.key = element_rect(color = "#333333", fill = NA),
        legend.title = element_text(color="white"),
        legend.text = element_text(color = "white")
      )
  }

  return(plot)
}
