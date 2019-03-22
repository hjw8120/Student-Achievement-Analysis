PROJECT TITLE
================
LAW students
Mar. 29th

### Load packages

``` r
library(tidyverse) 
library(broom)
library(infer)
```

### Load data

``` r
students <- read_csv("/cloud/project/proposal/student-mat.csv")
students
```

    ## # A tibble: 395 x 33
    ##    school sex     age address famsize Pstatus  Medu  Fedu Mjob  Fjob 
    ##    <chr>  <chr> <dbl> <chr>   <chr>   <chr>   <dbl> <dbl> <chr> <chr>
    ##  1 GP     F        18 U       GT3     A           4     4 at_h… teac…
    ##  2 GP     F        17 U       GT3     T           1     1 at_h… other
    ##  3 GP     F        15 U       LE3     T           1     1 at_h… other
    ##  4 GP     F        15 U       GT3     T           4     2 heal… serv…
    ##  5 GP     F        16 U       GT3     T           3     3 other other
    ##  6 GP     M        16 U       LE3     T           4     3 serv… other
    ##  7 GP     M        16 U       LE3     T           2     2 other other
    ##  8 GP     F        17 U       GT3     A           4     4 other teac…
    ##  9 GP     M        15 U       LE3     A           3     2 serv… other
    ## 10 GP     M        15 U       GT3     T           3     4 other other
    ## # … with 385 more rows, and 23 more variables: reason <chr>,
    ## #   guardian <chr>, traveltime <dbl>, studytime <dbl>, failures <dbl>,
    ## #   schoolsup <chr>, famsup <chr>, paid <chr>, activities <chr>,
    ## #   nursery <chr>, higher <chr>, internet <chr>, romantic <chr>,
    ## #   famrel <dbl>, freetime <dbl>, goout <dbl>, Dalc <dbl>, Walc <dbl>,
    ## #   health <dbl>, absences <dbl>, G1 <dbl>, G2 <dbl>, G3 <dbl>

## Section 1. Introduction

## Section 2. Data analysis plan

## Section 3. Data
