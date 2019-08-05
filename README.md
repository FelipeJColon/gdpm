
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdpm

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/epix-project/gdpm?branch=master&svg=true)](https://ci.appveyor.com/project/epix-project/gdpm)
<!-- badges: end -->

The goal of gdpm is to provide the data from the General Department of
Preventive Medicine (GDPM) of Vietnam.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("epix-project/gdpm")
```

## Example

``` r
library(gdpm)
```

The package contains epidemiological data in 29 epidemiological data
frames, each data frame corresponding to one syndromic disease. For
example, `chickenpox`:

``` r
chickenpox <-  getid(chickenpox)
head(chickenpox)
#>      province    month year incidence_chickenpox mortality_chickenpox
#> 5293 An Giang  January 1980                   NA                   NA
#> 5297 An Giang February 1980                   NA                   NA
#> 5298 An Giang    March 1980                    2                   NA
#> 5299 An Giang    April 1980                   NA                   NA
#> 5300 An Giang      May 1980                   NA                   NA
#> 5301 An Giang     June 1980                   NA                   NA
```

The data are expressed by the `incidence` or number of cases and
`mortality` or the number of death per `province`, `month` and `year`.
See below, the section on the `getid` function, for an illustration of
the specificity of this function.

The structure of the data frame is:

``` r
str(chickenpox)
#> 'data.frame':    17784 obs. of  5 variables:
#>  $ province            : chr  "An Giang" "An Giang" "An Giang" "An Giang" ...
#>  $ month               : Ord.factor w/ 12 levels "January"<"February"<..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ year                : int  1980 1980 1980 1980 1980 1980 1980 1980 1980 1980 ...
#>  $ incidence_chickenpox: int  NA NA 2 NA NA NA 7 NA NA NA ...
#>  $ mortality_chickenpox: int  NA NA NA NA NA NA NA NA NA NA ...
```

Note that time is coded by the 2 variables `year` and `month`. The
latter is a factor in which the coding of the levels follows their
chronological order:

``` r
levels(chickenpox$month)
#>  [1] "January"   "February"  "March"     "April"     "May"      
#>  [6] "June"      "July"      "August"    "September" "October"  
#> [11] "November"  "December"
```

Head and tail of the data frame `chickenpox`:

``` r
head(chickenpox)
#>      province    month year incidence_chickenpox mortality_chickenpox
#> 5293 An Giang  January 1980                   NA                   NA
#> 5297 An Giang February 1980                   NA                   NA
#> 5298 An Giang    March 1980                    2                   NA
#> 5299 An Giang    April 1980                   NA                   NA
#> 5300 An Giang      May 1980                   NA                   NA
#> 5301 An Giang     June 1980                   NA                   NA
tail(chickenpox)
#>       province     month year incidence_chickenpox mortality_chickenpox
#> 45020 Vinh Phu      July 2017                   68                    0
#> 44620 Vinh Phu    August 2017                   21                    0
#> 4561  Vinh Phu September 2017                   30                    0
#> 45520 Vinh Phu   October 2017                   22                    0
#> 45420 Vinh Phu  November 2017                   49                    0
#> 44720 Vinh Phu  December 2017                   49                    0
```
