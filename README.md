# garmintrackR

## Intro

garmintrackR is my first attempt at creating an R package. The Garmin Connect app, whilst comprehensive, allows somewhat limited visualisations of running and cycling data with little space for customisation, and little in the way of depth.

This package is currently under development, and I am seeking new ways to analyse the data, and new ways to visualise the data all of the time. I will continue to add new functionality to the package as I continue analysing and continue running. As I begin to train for a marathon, this more advanced analysis will become crucial.

At the minute the package consists of quite basic data visualisation. My aim to develop new metrics from the data to give improved insights.

I am also yet to robustly test any of the functions. To do this I will need external datasets, and for others to give it a try and feedback.

Please contact me if you have any comments about the code. I am a beginner.

## How to install garmintrackR

```{r}
library(devtools)
install_github("christianbromley/garmintrackR")

```

## How to use garmintrackR

### Download your Garmin data
Log into Garmin connect, click on __Activities__ and hit __All Activities__. Scroll down to the bottom of the page, and then hit __Export CSV__ in the top right.

### Read in your garmin data, and prepare it for analysis
```{r}
setwd("~/Downloads")
garmin <- read.csv("Activities.csv", header=T)

```

Now we will use our first garmintrackR function. This function will tidy some of the variables, and ditch some unimportant ones.
```{r, warning=FALSE, error=FALSE, message=FALSE}
my_runs <- processGarminRunning(data=garmin)

```

### Overiew of all runs

Let's take an overview of some of my runs.

What distances do I most frequently run?
```{r, warning=FALSE}
overview_all_runs(my_runs,
                  plot = "total_runs",
                  date_from = as.Date("2020-03-18"))
```

2-3 miles is clearly my favourite length. Doesn't take too long out of the day!

How far have I run in each location?
```{r, warning=FALSE}
overview_all_runs(my_runs,
                  plot = "distance_per_location",
                  date_from = as.Date("2020-03-18"))

```

How are these runs subdivided by run length?
```{r, warning=FALSE}
overview_all_runs(my_runs,
                  plot = "runs_per_loc_per_dist",
                  date_from = as.Date("2020-03-18"))
```

Most of the mileage was done in Bromsgrove although in Manchester further was run in runs of greater than 3 miles.

What about my pace?
```{r, warning=FALSE}
overview_all_runs(my_runs,
                  plot = "mean_pace",
                  date_from = as.Date("2020-03-18"))

```

My long runs were faster in Manchester than in Bromsgrove.

How do my runs cluster?

```{r, warning=FALSE}
overview_all_runs(my_runs,
                  plot = "pca_plot",
                  date_from = as.Date("2020-03-18"))


```

You'll notice that stride length and cadence are important predictors of PC2 with PC1 dominated by run distance. It's possible the cluster of runs in the top right with short distance and short stride length were the runs I did with my girlfriend.

### Overview individual runs

Next let's take a look at my individual runs over lockdown.

c("distance", "pace", "dist_pace", "dist_time", "dist_cals", "cumulative_dist","pace_per_dist.gp","heart.rate_dist.gp","cadence_dist.gp","dist_time.bar")

First let's track how far I have run since lockdown. How consistent did I stick to running?
```{r,warning=FALSE}
overview_individual_runs(my_runs,
                   plot = "cumulative_dist",
               date_from = as.Date("2020-03-18"))
```

Clearly, got a bit lazy from mid June to end of July.

Exactly, far far were these runs?
```{r,warning=FALSE}
overview_individual_runs(my_runs,
                plot = "dist_time",
                 date_from = as.Date("2020-03-18"),
                target_time = "7:18")

```
The trend is certainly upwards in the August - as I got fitter I wanted to run further.

Let's look at my distribution of distances per location.
```{r,warning=FALSE}
overview_individual_runs(my_runs,
                plot = "distance",
                 date_from = as.Date("2020-03-18"))

```

There's a few anomalies. 3/5 of my longest runs have been in Manchester.

What about pace? How fast have I been running? Compare this to my per mile half marathon time
```{r,warning=FALSE}
 overview_individual_runs(my_runs,
                  plot = "pace",
                 date_from = as.Date("2020-03-18"),
                  target_time = "7:18")
                  
overview_individual_runs(my_runs,
                 plot = "dist_pace",
                 date_from = as.Date("2020-03-18"),
                 target_time = "7:18")
```

It seems my half marathon pace is slightly beyond me at the moment. At distances past 5 miles my pace seems quite consistent hovering around 8min per mile.

Let's separate by distance and take a look at pace.
```{r}
overview_individual_runs(my_runs,
                  plot = "pace_per_dist.gp",
                  date_from = as.Date("2020-03-18"),
                  target_time = "7:18")

```

Those 2-3 mile runs in Bromsgrove early on in lockdown were certainly getting faster if you exclude those anomalies.


How many calories do I tend to burn on these runs?
```{r,warning=FALSE}

overview_individual_runs(my_runs,
                  plot = "dist_cals",
                  date_from = as.Date("2020-03-18"))
```

Very strong relationship between calories and distance run - strange wobble at the bottom  may be related to pace!

What about heart rate?
```{r,warning=FALSE}
overview_individual_runs(my_runs,
                plot = "heart.rate_dist.gp",
                  date_from = as.Date("2020-03-18"))
```

You'll notice in the longer runs that the average heart rate for each run is above the median value of average heart rates across all runs.

What about cadence?
```{r,warning=FALSE}
overview_individual_runs(my_runs,
                plot = "cadence_dist.gp",
               date_from = as.Date("2020-03-18"))

```

Again the max cadence seems lower in those long runs. I was probably struggling for that sprint finish.

What about elevation gain? Obviously if you run further just by chance you will gain more elevation, so let's normalise elevation gain by distance. this gives us a true idea of whether a run was particularly hilly. Do hills ruin my pace?

```{r,warning=FALSE}
overview_individual_runs(my_runs,
                plot = "elevation",
               date_from = as.Date("2020-03-18"))

```

What if we group by distance?
```{r,warning=FALSE}
overview_individual_runs(my_runs,
                plot = "elevation_by_dist",
               date_from = as.Date("2020-03-18"))

```

## Next steps

I am wanting to develop this package further to do some more sophisticated analytics. Here are a few ideas:

- plot customisation/ new palettes
- cycling and walking analysis
- analysis of elevation
- advanced analytics

Advice would be most welcome, please email me.
