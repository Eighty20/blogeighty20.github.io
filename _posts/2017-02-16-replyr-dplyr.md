---
layout: post
date: 2017-02-16 13:52:21
header-img: "img/home-bg2.jpg"
comments: true
subtitle: "Challenge accepted"
category: Package Exploration
tags: [R, dplyr]
title: "Rewiring replyr with dplyr"
author: "Hanjo Odendaal"
output: html_document
---
# Introduction of Parameterized dplyr expression 
## Introduction of Parameterized dplyr expression 

The usefullness of any small function you write will eventually be judged upon its ability to be generically applied across any arbitrary data. As I explored a blog post from [Dec 2016](http://www.win-vector.com/blog/2016/12/using-replyrlet-to-parameterize-dplyr-expressions/), I became a lot more interested in writing dynamic code with dplyr functions that form part of the data wrangling silo in my analytical flow. This ability came with the new `replyr` package - No longer will I have the need to break up my data processing when columns have to be changed as my code depends on certain column names in my dataset that is currently in use. 

> 'replyr allows you to encapsulate complex dplyr expressions without the use of the lazyeval package, which is the currently recommended way to manage dplyr‘s use of non-standard evaluation'

The example `replyr` provides works out summary statistics of an arbitrary column, with the ability to group by another column. Imagine you had a quick and easy function which could pump out summary statistics without too much fuss. Lets take a look at the `replyr` construct of such a function provided by the package maintainers [Win-Vector](http://www.win-vector.com/blog/):


{% highlight r %}
# to install replyr: 
# devtools::install_github('WinVector/replyr')

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(replyr))

#
# calculate mean +/- sd intervals and
#           median +/- 1/2 IQR intervals
#           for arbitrary data frame column, with optional grouping
#

dist_intervals <- function(dframe, colname, groupcolname=NULL) {
  mapping = list(col=colname)
  if(!is.null(groupcolname)) {
    dframe %>% group_by_(groupcolname) -> dframe
  }
  let(alias=mapping,
      expr={
        dframe %>% summarize(sdlower = mean(col)-sd(col),
                             mean = mean(col),
                             sdupper = mean(col) + sd(col),
                             iqrlower = median(col)-0.5*IQR(col),
                             median = median(col),
                             iqrupper = median(col)+0.5*IQR(col))
      })
}
{% endhighlight %}

For my analysis I will be using the `snail` dataset.
This dataset contains data on the probability of a snail surviving given certain stimuli such as:

  * exposure in weeks
  * relative humidity (4 levels)
  * temperature, in degrees Celsius (3 levels)
  * deaths


{% highlight r %}
Snails <-  read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MASS/snails.csv")

Snails %>% glimpse
{% endhighlight %}



{% highlight text %}
## Observations: 96
## Variables: 7
## $ X        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16...
## $ Species  <fctr> A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A...
## $ Exposure <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,...
## $ Rel.Hum  <dbl> 60.0, 60.0, 60.0, 65.8, 65.8, 65.8, 70.5, 70.5, 70.5,...
## $ Temp     <int> 10, 15, 20, 10, 15, 20, 10, 15, 20, 10, 15, 20, 10, 1...
## $ Deaths   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0,...
## $ N        <int> 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 2...
{% endhighlight %}

Lets now see how the function outputs with a simple example using the `snail` dataset.


{% highlight r %}
Snails %>% dist_intervals("Deaths")
{% endhighlight %}



{% highlight text %}
##      sdlower     mean  sdupper iqrlower median iqrupper
## 1 -0.8984496 2.864583 6.627616   -1.125      1    3.125
{% endhighlight %}
As you can see, the summary statistics for the death column was worked out. For those who noticed the negtive `sdlower` - no, snails did not wake up, its purely for example purposes.

Expanding this idea of by includeing a grouping variable, we can get the summary statistics per specie:


{% highlight r %}
Snails %>% dist_intervals("Deaths", "Species")
{% endhighlight %}



{% highlight text %}
## # A tibble: 2 × 7
##   Species    sdlower     mean  sdupper iqrlower median iqrupper
##    <fctr>      <dbl>    <dbl>    <dbl>    <dbl>  <dbl>    <dbl>
## 1       A -0.5320915 1.708333 3.948758     -1.0    0.5      2.0
## 2       B -0.5477438 4.020833 8.589410     -1.5    2.0      5.5
{% endhighlight %}

So, we can see the function works well in the sense that you can now write dynamic loops which could apply `dplyr` functions to an arbitrary column and grouping variable of your choice based on string inputs...

## But that was not the end

In one part of the [Win-Vector blog](http://www.win-vector.com/blog/2016/12/using-replyrlet-to-parameterize-dplyr-expressions/) they have the following challenge:

> To write such a function in dplyr can get quite hairy, quite quickly. Try it yourself, and see

So, with this in mind I have, with some fighting I might add, developed a similar function that has some added benefits beyond that of the `replyr` function using the `dplyr` package.


{% highlight r %}
dist_intervals_dplyr <- function(dframe, col, group) {
  
  if(missing(group))
  {
  df <- 
    dframe %>% 
      select(., one_of(col)) 
  } else {
  df <- 
    dframe  %>% 
    group_by_(group) %>%  
    select(., one_of(c(group, col))) 
  }
  
  df %>% 
    summarise_each(funs(sdlower = mean(.) - sd(.),
                       mean = mean(.),
                       sdupper = mean(.) + sd(.),
                       iqrlower = median(.)-0.5*IQR(.),
                       median = median(.),
                       iqrupper = median(.)+0.5*IQR(.)))
}
{% endhighlight %}

By combining the `one_of` and the standard standard evaluation of the `dplyr` functions we have recreated the results of the `replyr` function:

{% highlight r %}
Snails %>% dist_intervals_dplyr(col = "Deaths")
{% endhighlight %}



{% highlight text %}
##      sdlower     mean  sdupper iqrlower median iqrupper
## 1 -0.8984496 2.864583 6.627616   -1.125      1    3.125
{% endhighlight %}



{% highlight r %}
Snails %>% dist_intervals_dplyr("Deaths", group = "Species")
{% endhighlight %}



{% highlight text %}
## # A tibble: 2 × 7
##   Species    sdlower     mean  sdupper iqrlower median iqrupper
##    <fctr>      <dbl>    <dbl>    <dbl>    <dbl>  <dbl>    <dbl>
## 1       A -0.5320915 1.708333 3.948758     -1.0    0.5      2.0
## 2       B -0.5477438 4.020833 8.589410     -1.5    2.0      5.5
{% endhighlight %}

The added benefit of using the dplyr notation is the ability to include *mutliple* columns in the function that you want to summarise:
The `replyr` approach will not be able to handle this and will you give an error such as:

{% highlight r %}
Snails %>% dist_intervals(c("Deaths", "Exposure"), group = "Species")
 # Error in letprep(alias, strexpr) : 
 #  wrapr:let alias values must all be single strings (not arrays)
{% endhighlight %}


{% highlight r %}
Snails %>% dist_intervals_dplyr(c("Deaths", "Exposure"), group = "Species")
{% endhighlight %}



{% highlight text %}
## # A tibble: 2 × 13
##   Species Deaths_sdlower Exposure_sdlower Deaths_mean Exposure_mean
##    <fctr>          <dbl>            <dbl>       <dbl>         <dbl>
## 1       A     -0.5320915         1.370135    1.708333           2.5
## 2       B     -0.5477438         1.370135    4.020833           2.5
## # ... with 8 more variables: Deaths_sdupper <dbl>, Exposure_sdupper <dbl>,
## #   Deaths_iqrlower <dbl>, Exposure_iqrlower <dbl>, Deaths_median <dbl>,
## #   Exposure_median <dbl>, Deaths_iqrupper <dbl>, Exposure_iqrupper <dbl>
{% endhighlight %}

And with a little bit of tidying of the data, you can have a much richer dataset

{% highlight r %}
library(tidyr)
Snails %>% dist_intervals_dplyr(c("Deaths", "Exposure"), group = "Species") %>% 
  gather(key = Deaths, Metric, -Species) %>% 
  arrange(Species, Deaths)
{% endhighlight %}



{% highlight text %}
## # A tibble: 24 × 3
##    Species            Deaths     Metric
##     <fctr>             <chr>      <dbl>
## 1        A   Deaths_iqrlower -1.0000000
## 2        A   Deaths_iqrupper  2.0000000
## 3        A       Deaths_mean  1.7083333
## 4        A     Deaths_median  0.5000000
## 5        A    Deaths_sdlower -0.5320915
## 6        A    Deaths_sdupper  3.9487582
## 7        A Exposure_iqrlower  1.7500000
## 8        A Exposure_iqrupper  3.2500000
## 9        A     Exposure_mean  2.5000000
## 10       A   Exposure_median  2.5000000
## # ... with 14 more rows
{% endhighlight %}

I do find that applying parameterized dplyr functions an amazing advantage when working with more advanced ETL workflows. This is by no means to say that the `replyr` package is redundant, but more a declaration to say that `dplyr` has the ability to dynamically conduct analysis - albeit some fighting to get it to work.
