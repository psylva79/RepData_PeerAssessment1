## What is mean total number of steps taken per day?

Import data

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    activity <- read.csv("C:/Users/pmps/Downloads/activity.csv")

    activity <- activity %>% 
      drop_na(steps)

### Question 1

Calculate the total number of steps taken per day.

    activity %>% 
      group_by(date) %>% 
      summarize(total_steps = sum(steps))

    ## # A tibble: 53 × 2
    ##    date       total_steps
    ##    <chr>            <int>
    ##  1 2012-10-02         126
    ##  2 2012-10-03       11352
    ##  3 2012-10-04       12116
    ##  4 2012-10-05       13294
    ##  5 2012-10-06       15420
    ##  6 2012-10-07       11015
    ##  7 2012-10-09       12811
    ##  8 2012-10-10        9900
    ##  9 2012-10-11       10304
    ## 10 2012-10-12       17382
    ## # ℹ 43 more rows

## Question 2

If you do not understand the difference between a histogram and a
barplot, research the difference between them. Make a histogram of the
total number of steps taken each day.

    activity %>%
      group_by(date) %>% 
      summarize(total_steps = sum(steps)) %>% 
      ggplot(aes(x = total_steps)) +
      geom_histogram(fill = "darkblue", bins = 10) +
      theme_classic() +
      labs(x = "Number of steps (Daily)", y = "Count", title = "Histogram on daily activity")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)
\### Question 3 Calculate and report the mean and median of the total
number of steps taken per day.

    activity %>%
      summarize(total_steps = sum(steps), .by = date) %>% 
      summarise(mean = mean(total_steps),
                median = median(total_steps)) %>% 
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: right;">mean</th>
<th style="text-align: right;">median</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">10766.19</td>
<td style="text-align: right;">10765</td>
</tr>
</tbody>
</table>

## What is the average daily activity pattern?

### Question 1

Make a time series plot (i.e. type = “l”) of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    activity %>%
      summarize(mean_number_steps = mean(steps), .by = interval) %>% 
      ggplot(aes(x = interval, y = mean_number_steps)) +
      geom_line() +
      theme_classic() +
      labs(x = "Five-min interval", y = "Mean number of steps", title = "Intraday activity")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### Question 2

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    activity %>%
      summarize(mean_number_steps = mean(steps), .by = interval) %>% 
      slice_max(mean_number_steps)

    ##   interval mean_number_steps
    ## 1      835          206.1698

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as NA ). The presence of missing days may introduce bias
into some calculations or summaries of the data.

### Question 1

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

    activity <- read.csv("C:/Users/pmps/Downloads/activity.csv")

    # by variable
    lapply(activity, function(x) sum(is.na(x)))

    ## $steps
    ## [1] 2304
    ## 
    ## $date
    ## [1] 0
    ## 
    ## $interval
    ## [1] 0

    activity %>% 
      mutate(across(steps:interval, ~ is.na(.x), .names = "na_{.col}")) %>% 
      rowwise() %>%
      mutate(NAs = any(c_across(starts_with("na_")))) %>% 
      ungroup() %>% 
      summarise(total_nas =sum(NAs))

    ## # A tibble: 1 × 1
    ##   total_nas
    ##       <int>
    ## 1      2304

### Question 2

Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

    activity %>% 
      group_by(interval) %>% 
      mutate(steps = if_else(is.na(steps), mean(steps, na.rm = TRUE), steps)) 

    ## # A tibble: 17,568 × 3
    ## # Groups:   interval [288]
    ##     steps date       interval
    ##     <dbl> <chr>         <int>
    ##  1 1.72   2012-10-01        0
    ##  2 0.340  2012-10-01        5
    ##  3 0.132  2012-10-01       10
    ##  4 0.151  2012-10-01       15
    ##  5 0.0755 2012-10-01       20
    ##  6 2.09   2012-10-01       25
    ##  7 0.528  2012-10-01       30
    ##  8 0.868  2012-10-01       35
    ##  9 0      2012-10-01       40
    ## 10 1.47   2012-10-01       45
    ## # ℹ 17,558 more rows

### Question 3

Create a new dataset that is equal to the original dataset but with the
missing data filled in.

    new_activity <- activity %>% 
      group_by(interval) %>% 
      mutate(steps = if_else(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>% 
      ungroup()

### Question 4

Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

    new_activity %>%
      group_by(date) %>% 
      summarize(total_steps = sum(steps)) %>% 
      ggplot(aes(x = total_steps)) +
      geom_histogram(fill = "darkblue", bins = 10) +
      theme_classic() +
      labs(x = "Number of steps (Daily)", y = "Count", title = "Histogram on daily activity")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    new_activity %>%
      summarize(total_steps = sum(steps), .by = date) %>% 
      summarise(mean = mean(total_steps),
                median = median(total_steps)) %>% 
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: right;">mean</th>
<th style="text-align: right;">median</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">10766.19</td>
<td style="text-align: right;">10766.19</td>
</tr>
</tbody>
</table>

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

### Question 1

Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.

    new_activity  <- new_activity %>%
      mutate(week_day = if_else(lubridate::wday(as.Date(date), week_start = 1) >=6, 
                                'weekend', 'weekday') %>% factor()) 

### Question 2

Make a panel plot containing a time series plot (i.e. type = “l”) of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis). See the
README file in the GitHub repository to see an example of what this plot
should look like using simulated data.

    new_activity %>% 
      group_by(week_day, interval) %>% 
      summarize(mean_number_steps = mean(steps)) %>% 
      ggplot(aes(x = interval, y = mean_number_steps)) +
      geom_line() +
      theme_classic() +
      facet_wrap(week_day ~., nrow = 2) + 
      labs(x = "Five-min interval", 
           y = "Mean number of steps", 
           title = "Intraday activity")

    ## `summarise()` has grouped output by 'week_day'. You can override using the
    ## `.groups` argument.

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
