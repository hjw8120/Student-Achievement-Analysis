PROJECT TITLE
================
NAME HERE
TODAY’S DATE

title: “MATH IS FUN IF YOU ANALYZE IT RIGHT\!” author: “LAW students”
date: “Mar. 29th” output: github\_document —

### Load packages

``` r
library(tidyverse) 
library(broom)
library(infer)
```

### Load data

``` r
students <- read_csv("/cloud/project/data/student-mat.csv")
```

Your project goes here\! Before you submit, make sure your chunks are
turned off with `echo = FALSE`.

You can add sections as you see fit. Make sure you have a section called
Introduction at the beginning and a section called Conclusion at the
end. The rest is up to you\!

Data analysis

``` r
students <- students %>%
  mutate(first_gen = case_when(
    Medu < 4 & Fedu <4 ~ "Yes",
    TRUE ~ "No"
  ))
```

``` r
students <- students %>%
  mutate(avg_score = ((G1 + G2 + G3)/3))
```

``` r
first <- students %>%
  filter(first_gen == "Yes")
```

``` r
second <- students %>%
  filter(first_gen == "No")
```

``` r
first_model <- lm(avg_score ~ school + 
                  sex + age + address + 
                  famsize + Pstatus + 
                   Mjob + Fjob + 
                  reason + guardian + traveltime +
                  studytime + failures + 
                  paid + activities +
                  nursery + higher + internet +
                  romantic + famrel + freetime +
                  goout + Dalc + Walc +
                  health + absences, data = first)
first_model <- step(first_model, direction = "backward")
```

    ## Start:  AIC=577.21
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Mjob + Fjob + reason + guardian + traveltime + studytime + 
    ##     failures + paid + activities + nursery + higher + internet + 
    ##     romantic + famrel + freetime + goout + Dalc + Walc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Mjob        4    18.121 2006.4 571.37
    ## - reason      3    16.768 2005.0 573.21
    ## - guardian    2     1.124 1989.4 573.35
    ## - Fjob        4    40.667 2028.9 574.03
    ## - freetime    1     0.036 1988.3 575.22
    ## - age         1     0.056 1988.3 575.22
    ## - famrel      1     0.205 1988.5 575.24
    ## - paid        1     1.508 1989.8 575.39
    ## - school      1     2.246 1990.5 575.48
    ## - Walc        1     2.785 1991.0 575.55
    ## - traveltime  1     6.457 1994.7 575.99
    ## - Dalc        1     8.992 1997.3 576.29
    ## - famsize     1     9.661 1997.9 576.37
    ## - nursery     1     9.878 1998.1 576.39
    ## - internet    1    10.153 1998.4 576.43
    ## - health      1    13.771 2002.0 576.86
    ## - Pstatus     1    14.272 2002.5 576.92
    ## - romantic    1    15.803 2004.1 577.10
    ## <none>                    1988.3 577.21
    ## - goout       1    21.236 2009.5 577.74
    ## - address     1    22.177 2010.4 577.85
    ## - absences    1    22.304 2010.6 577.87
    ## - activities  1    23.313 2011.6 577.99
    ## - higher      1    25.338 2013.6 578.23
    ## - studytime   1    60.195 2048.5 582.31
    ## - sex         1   184.514 2172.8 596.34
    ## - failures    1   235.143 2223.4 601.82
    ## 
    ## Step:  AIC=571.37
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Fjob + reason + guardian + traveltime + studytime + failures + 
    ##     paid + activities + nursery + higher + internet + romantic + 
    ##     famrel + freetime + goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - guardian    2     2.118 2008.5 567.62
    ## - reason      3    19.596 2026.0 567.69
    ## - freetime    1     0.000 2006.4 569.37
    ## - age         1     0.056 2006.4 569.38
    ## - famrel      1     0.207 2006.6 569.40
    ## - paid        1     1.968 2008.4 569.61
    ## - Walc        1     2.315 2008.7 569.65
    ## - school      1     3.119 2009.5 569.74
    ## - Fjob        4    55.254 2061.6 569.84
    ## - Dalc        1     8.238 2014.6 570.35
    ## - famsize     1     8.274 2014.7 570.35
    ## - traveltime  1     8.809 2015.2 570.42
    ## - nursery     1    10.821 2017.2 570.65
    ## - Pstatus     1    11.729 2018.1 570.76
    ## - internet    1    12.562 2019.0 570.86
    ## - health      1    13.821 2020.2 571.01
    ## - romantic    1    16.886 2023.3 571.37
    ## <none>                    2006.4 571.37
    ## - absences    1    18.993 2025.4 571.62
    ## - activities  1    21.405 2027.8 571.90
    ## - higher      1    22.915 2029.3 572.08
    ## - goout       1    25.503 2031.9 572.38
    ## - address     1    25.582 2032.0 572.39
    ## - studytime   1    68.092 2074.5 577.32
    ## - sex         1   183.219 2189.6 590.17
    ## - failures    1   228.254 2234.6 595.02
    ## 
    ## Step:  AIC=567.62
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Fjob + reason + traveltime + studytime + failures + paid + 
    ##     activities + nursery + higher + internet + romantic + famrel + 
    ##     freetime + goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - reason      3    20.102 2028.6 563.99
    ## - freetime    1     0.001 2008.5 565.62
    ## - age         1     0.116 2008.6 565.64
    ## - famrel      1     0.205 2008.7 565.65
    ## - paid        1     2.384 2010.9 565.91
    ## - Fjob        4    53.776 2062.3 565.91
    ## - Walc        1     3.008 2011.5 565.98
    ## - school      1     3.760 2012.3 566.07
    ## - famsize     1     8.204 2016.7 566.59
    ## - Dalc        1     8.641 2017.2 566.65
    ## - traveltime  1     9.558 2018.1 566.75
    ## - nursery     1    12.570 2021.1 567.11
    ## - internet    1    13.315 2021.8 567.20
    ## - Pstatus     1    13.797 2022.3 567.25
    ## - health      1    14.410 2022.9 567.33
    ## - romantic    1    15.932 2024.4 567.50
    ## <none>                    2008.5 567.62
    ## - absences    1    21.025 2029.5 568.10
    ## - activities  1    21.556 2030.1 568.17
    ## - higher      1    26.063 2034.6 568.69
    ## - address     1    26.490 2035.0 568.74
    ## - goout       1    26.513 2035.0 568.75
    ## - studytime   1    69.850 2078.4 573.76
    ## - sex         1   183.758 2192.3 586.46
    ## - failures    1   234.650 2243.2 591.92
    ## 
    ## Step:  AIC=563.99
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Fjob + traveltime + studytime + failures + paid + activities + 
    ##     nursery + higher + internet + romantic + famrel + freetime + 
    ##     goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - freetime    1     0.004 2028.6 561.99
    ## - famrel      1     0.011 2028.6 562.00
    ## - age         1     0.104 2028.7 562.01
    ## - Walc        1     1.388 2030.0 562.16
    ## - school      1     3.482 2032.1 562.40
    ## - paid        1     3.539 2032.2 562.41
    ## - traveltime  1     7.827 2036.4 562.91
    ## - Fjob        4    60.032 2088.6 562.94
    ## - famsize     1     8.074 2036.7 562.94
    ## - Dalc        1     8.557 2037.2 563.00
    ## - internet    1    11.861 2040.5 563.38
    ## - nursery     1    11.905 2040.5 563.39
    ## - romantic    1    13.552 2042.2 563.58
    ## - Pstatus     1    14.581 2043.2 563.70
    ## - health      1    15.808 2044.4 563.84
    ## - activities  1    16.765 2045.4 563.95
    ## <none>                    2028.6 563.99
    ## - higher      1    21.027 2049.6 564.45
    ## - absences    1    21.700 2050.3 564.53
    ## - address     1    22.569 2051.2 564.63
    ## - goout       1    31.451 2060.1 565.66
    ## - studytime   1    72.265 2100.9 570.33
    ## - sex         1   176.302 2204.9 581.83
    ## - failures    1   253.357 2282.0 590.00
    ## 
    ## Step:  AIC=561.99
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Fjob + traveltime + studytime + failures + paid + activities + 
    ##     nursery + higher + internet + romantic + famrel + goout + 
    ##     Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - famrel      1     0.010 2028.6 560.00
    ## - age         1     0.104 2028.7 560.01
    ## - Walc        1     1.415 2030.0 560.16
    ## - school      1     3.481 2032.1 560.40
    ## - paid        1     3.535 2032.2 560.41
    ## - traveltime  1     7.825 2036.4 560.91
    ## - famsize     1     8.071 2036.7 560.94
    ## - Fjob        4    60.091 2088.7 560.94
    ## - Dalc        1     8.973 2037.6 561.05
    ## - internet    1    11.890 2040.5 561.39
    ## - nursery     1    11.957 2040.6 561.39
    ## - romantic    1    13.572 2042.2 561.58
    ## - Pstatus     1    14.647 2043.3 561.71
    ## - health      1    15.806 2044.4 561.84
    ## - activities  1    16.793 2045.4 561.96
    ## <none>                    2028.6 561.99
    ## - higher      1    21.031 2049.6 562.45
    ## - absences    1    21.716 2050.3 562.53
    ## - address     1    22.869 2051.5 562.66
    ## - goout       1    32.539 2061.2 563.78
    ## - studytime   1    72.275 2100.9 568.33
    ## - sex         1   182.879 2211.5 580.54
    ## - failures    1   253.739 2282.3 588.04
    ## 
    ## Step:  AIC=560
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Fjob + traveltime + studytime + failures + paid + activities + 
    ##     nursery + higher + internet + romantic + goout + Dalc + Walc + 
    ##     health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - age         1     0.097 2028.7 558.01
    ## - Walc        1     1.408 2030.0 558.16
    ## - school      1     3.483 2032.1 558.40
    ## - paid        1     3.531 2032.2 558.41
    ## - traveltime  1     7.833 2036.5 558.91
    ## - famsize     1     8.273 2036.9 558.96
    ## - Fjob        4    60.799 2089.4 559.02
    ## - Dalc        1     8.983 2037.6 559.05
    ## - nursery     1    11.967 2040.6 559.40
    ## - internet    1    12.050 2040.7 559.41
    ## - romantic    1    13.565 2042.2 559.58
    ## - Pstatus     1    14.732 2043.3 559.72
    ## - health      1    15.826 2044.5 559.85
    ## - activities  1    16.822 2045.4 559.96
    ## <none>                    2028.6 560.00
    ## - higher      1    21.030 2049.7 560.45
    ## - absences    1    21.707 2050.3 560.53
    ## - address     1    22.870 2051.5 560.66
    ## - goout       1    33.560 2062.2 561.90
    ## - studytime   1    72.927 2101.6 566.40
    ## - sex         1   187.228 2215.8 579.01
    ## - failures    1   255.039 2283.7 586.18
    ## 
    ## Step:  AIC=558.01
    ## avg_score ~ school + sex + address + famsize + Pstatus + Fjob + 
    ##     traveltime + studytime + failures + paid + activities + nursery + 
    ##     higher + internet + romantic + goout + Dalc + Walc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Walc        1     1.425 2030.1 556.17
    ## - paid        1     3.462 2032.2 556.41
    ## - school      1     3.562 2032.3 556.42
    ## - traveltime  1     7.737 2036.5 556.91
    ## - famsize     1     8.190 2036.9 556.97
    ## - Fjob        4    61.108 2089.8 557.07
    ## - Dalc        1     9.403 2038.1 557.11
    ## - nursery     1    12.114 2040.8 557.42
    ## - internet    1    12.554 2041.3 557.48
    ## - romantic    1    13.478 2042.2 557.58
    ## - Pstatus     1    14.636 2043.3 557.72
    ## - health      1    16.025 2044.7 557.88
    ## - activities  1    16.864 2045.6 557.98
    ## <none>                    2028.7 558.01
    ## - higher      1    21.194 2049.9 558.48
    ## - absences    1    22.542 2051.3 558.64
    ## - address     1    22.806 2051.5 558.67
    ## - goout       1    33.616 2062.3 559.92
    ## - studytime   1    75.654 2104.4 564.72
    ## - sex         1   187.626 2216.3 577.06
    ## - failures    1   261.272 2290.0 584.84
    ## 
    ## Step:  AIC=556.17
    ## avg_score ~ school + sex + address + famsize + Pstatus + Fjob + 
    ##     traveltime + studytime + failures + paid + activities + nursery + 
    ##     higher + internet + romantic + goout + Dalc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - paid        1     3.031 2033.2 554.53
    ## - school      1     3.317 2033.5 554.56
    ## - traveltime  1     7.290 2037.4 555.03
    ## - famsize     1     7.803 2038.0 555.09
    ## - Dalc        1     8.322 2038.5 555.15
    ## - Fjob        4    62.571 2092.7 555.40
    ## - nursery     1    11.517 2041.7 555.52
    ## - internet    1    12.239 2042.4 555.61
    ## - romantic    1    13.631 2043.8 555.77
    ## - Pstatus     1    14.847 2045.0 555.91
    ## - activities  1    15.939 2046.1 556.04
    ## - health      1    16.806 2047.0 556.14
    ## <none>                    2030.1 556.17
    ## - higher      1    21.589 2051.7 556.69
    ## - absences    1    22.123 2052.3 556.75
    ## - address     1    24.374 2054.5 557.02
    ## - goout       1    43.266 2073.4 559.19
    ## - studytime   1    79.246 2109.4 563.29
    ## - sex         1   186.582 2216.7 575.10
    ## - failures    1   261.721 2291.9 583.03
    ## 
    ## Step:  AIC=554.53
    ## avg_score ~ school + sex + address + famsize + Pstatus + Fjob + 
    ##     traveltime + studytime + failures + activities + nursery + 
    ##     higher + internet + romantic + goout + Dalc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - school      1     3.713 2036.9 552.96
    ## - traveltime  1     6.896 2040.1 553.34
    ## - famsize     1     7.526 2040.7 553.41
    ## - Fjob        4    60.674 2093.8 553.53
    ## - Dalc        1     9.612 2042.8 553.65
    ## - nursery     1    10.604 2043.8 553.77
    ## - internet    1    11.511 2044.7 553.87
    ## - romantic    1    13.770 2046.9 554.14
    ## - Pstatus     1    14.638 2047.8 554.24
    ## - activities  1    17.002 2050.2 554.51
    ## <none>                    2033.2 554.53
    ## - health      1    17.774 2050.9 554.60
    ## - absences    1    21.439 2054.6 555.03
    ## - higher      1    23.974 2057.2 555.32
    ## - address     1    25.003 2058.2 555.44
    ## - goout       1    44.107 2077.3 557.64
    ## - studytime   1    86.928 2120.1 562.49
    ## - sex         1   185.117 2218.3 573.27
    ## - failures    1   272.905 2306.1 582.51
    ## 
    ## Step:  AIC=552.96
    ## avg_score ~ sex + address + famsize + Pstatus + Fjob + traveltime + 
    ##     studytime + failures + activities + nursery + higher + internet + 
    ##     romantic + goout + Dalc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - traveltime  1     5.597 2042.5 551.62
    ## - Fjob        4    58.637 2095.5 551.72
    ## - famsize     1     7.140 2044.0 551.80
    ## - Dalc        1     8.981 2045.9 552.01
    ## - internet    1    10.196 2047.1 552.15
    ## - nursery     1    10.382 2047.3 552.17
    ## - activities  1    14.753 2051.6 552.68
    ## - romantic    1    14.999 2051.9 552.71
    ## - Pstatus     1    15.154 2052.0 552.73
    ## <none>                    2036.9 552.96
    ## - health      1    17.227 2054.1 552.97
    ## - higher      1    22.974 2059.9 553.63
    ## - absences    1    23.736 2060.6 553.72
    ## - address     1    30.013 2066.9 554.45
    ## - goout       1    43.831 2080.7 556.03
    ## - studytime   1    90.954 2127.8 561.36
    ## - sex         1   185.865 2222.8 571.75
    ## - failures    1   271.225 2308.1 580.72
    ## 
    ## Step:  AIC=551.62
    ## avg_score ~ sex + address + famsize + Pstatus + Fjob + studytime + 
    ##     failures + activities + nursery + higher + internet + romantic + 
    ##     goout + Dalc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Fjob        4    54.746 2097.2 549.91
    ## - famsize     1     8.824 2051.3 550.64
    ## - nursery     1     9.577 2052.1 550.73
    ## - internet    1     9.601 2052.1 550.73
    ## - Dalc        1    10.968 2053.4 550.89
    ## - activities  1    13.543 2056.0 551.19
    ## - Pstatus     1    14.496 2057.0 551.30
    ## - romantic    1    15.360 2057.8 551.40
    ## - health      1    16.271 2058.8 551.51
    ## <none>                    2042.5 551.62
    ## - higher      1    21.231 2063.7 552.08
    ## - absences    1    22.386 2064.9 552.21
    ## - address     1    25.298 2067.8 552.55
    ## - goout       1    46.077 2088.6 554.93
    ## - studytime   1    86.889 2129.4 559.53
    ## - sex         1   182.238 2224.7 569.96
    ## - failures    1   273.365 2315.8 579.51
    ## 
    ## Step:  AIC=549.91
    ## avg_score ~ sex + address + famsize + Pstatus + studytime + failures + 
    ##     activities + nursery + higher + internet + romantic + goout + 
    ##     Dalc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - nursery     1     5.962 2103.2 548.59
    ## - internet    1     7.345 2104.6 548.74
    ## - famsize     1     9.520 2106.8 548.99
    ## - Pstatus     1    13.650 2110.9 549.46
    ## - Dalc        1    14.691 2111.9 549.57
    ## - activities  1    15.931 2113.2 549.71
    ## - romantic    1    16.808 2114.0 549.81
    ## <none>                    2097.2 549.91
    ## - health      1    17.753 2115.0 549.92
    ## - higher      1    18.974 2116.2 550.06
    ## - absences    1    21.341 2118.6 550.32
    ## - address     1    33.426 2130.7 551.68
    ## - goout       1    57.455 2154.7 554.34
    ## - studytime   1   108.774 2206.0 559.95
    ## - sex         1   173.276 2270.5 566.81
    ## - failures    1   262.001 2359.2 575.93
    ## 
    ## Step:  AIC=548.59
    ## avg_score ~ sex + address + famsize + Pstatus + studytime + failures + 
    ##     activities + higher + internet + romantic + goout + Dalc + 
    ##     health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - internet    1     7.357 2110.6 547.42
    ## - famsize     1     8.091 2111.3 547.50
    ## - Pstatus     1    12.306 2115.5 547.98
    ## - activities  1    15.542 2118.7 548.34
    ## - Dalc        1    16.695 2119.9 548.47
    ## - health      1    17.274 2120.5 548.53
    ## - romantic    1    17.374 2120.6 548.55
    ## <none>                    2103.2 548.59
    ## - higher      1    18.629 2121.8 548.69
    ## - absences    1    21.590 2124.8 549.02
    ## - address     1    33.061 2136.2 550.30
    ## - goout       1    59.418 2162.6 553.22
    ## - studytime   1   105.203 2208.4 558.20
    ## - sex         1   170.019 2273.2 565.09
    ## - failures    1   260.696 2363.9 574.40
    ## 
    ## Step:  AIC=547.42
    ## avg_score ~ sex + address + famsize + Pstatus + studytime + failures + 
    ##     activities + higher + romantic + goout + Dalc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - famsize     1     8.978 2119.5 546.43
    ## - Pstatus     1    13.590 2124.1 546.95
    ## - activities  1    14.251 2124.8 547.02
    ## - Dalc        1    15.135 2125.7 547.12
    ## - health      1    15.370 2125.9 547.15
    ## <none>                    2110.6 547.42
    ## - higher      1    17.983 2128.5 547.44
    ## - absences    1    18.215 2128.8 547.46
    ## - romantic    1    18.880 2129.4 547.54
    ## - address     1    27.792 2138.3 548.53
    ## - goout       1    62.585 2173.1 552.37
    ## - studytime   1   101.614 2212.2 556.61
    ## - sex         1   164.084 2274.6 563.24
    ## - failures    1   259.048 2369.6 572.97
    ## 
    ## Step:  AIC=546.43
    ## avg_score ~ sex + address + Pstatus + studytime + failures + 
    ##     activities + higher + romantic + goout + Dalc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Pstatus     1    14.257 2133.8 546.02
    ## - activities  1    14.374 2133.9 546.04
    ## - health      1    17.251 2136.8 546.36
    ## <none>                    2119.5 546.43
    ## - higher      1    18.192 2137.7 546.46
    ## - romantic    1    18.477 2138.0 546.49
    ## - absences    1    18.799 2138.3 546.53
    ## - Dalc        1    19.063 2138.6 546.56
    ## - address     1    30.904 2150.4 547.87
    ## - goout       1    62.306 2181.8 551.32
    ## - studytime   1   104.025 2223.6 555.83
    ## - sex         1   170.229 2289.8 562.82
    ## - failures    1   258.270 2377.8 571.79
    ## 
    ## Step:  AIC=546.02
    ## avg_score ~ sex + address + studytime + failures + activities + 
    ##     higher + romantic + goout + Dalc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - health      1    16.701 2150.5 545.88
    ## - romantic    1    17.033 2150.8 545.92
    ## <none>                    2133.8 546.02
    ## - activities  1    19.622 2153.4 546.20
    ## - Dalc        1    19.644 2153.4 546.21
    ## - higher      1    20.572 2154.3 546.31
    ## - absences    1    22.783 2156.6 546.55
    ## - address     1    33.643 2167.4 547.75
    ## - goout       1    62.488 2196.3 550.89
    ## - studytime   1   103.843 2237.6 555.33
    ## - sex         1   165.532 2299.3 561.81
    ## - failures    1   250.525 2384.3 570.45
    ## 
    ## Step:  AIC=545.88
    ## avg_score ~ sex + address + studytime + failures + activities + 
    ##     higher + romantic + goout + Dalc + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - romantic    1    16.126 2166.6 545.66
    ## <none>                    2150.5 545.88
    ## - higher      1    18.693 2169.2 545.94
    ## - Dalc        1    19.599 2170.1 546.04
    ## - activities  1    21.686 2172.2 546.27
    ## - absences    1    23.968 2174.4 546.52
    ## - address     1    32.990 2183.5 547.50
    ## - goout       1    67.659 2218.1 551.25
    ## - studytime   1   106.689 2257.2 555.40
    ## - sex         1   154.680 2305.2 560.41
    ## - failures    1   265.763 2416.2 571.61
    ## 
    ## Step:  AIC=545.66
    ## avg_score ~ sex + address + studytime + failures + activities + 
    ##     higher + goout + Dalc + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## <none>                    2166.6 545.66
    ## - Dalc        1    19.352 2186.0 545.77
    ## - absences    1    20.907 2187.5 545.94
    ## - higher      1    23.806 2190.4 546.26
    ## - activities  1    25.609 2192.2 546.45
    ## - address     1    29.538 2196.2 546.88
    ## - goout       1    65.980 2232.6 550.80
    ## - studytime   1   107.296 2273.9 555.16
    ## - sex         1   167.126 2333.7 561.34
    ## - failures    1   278.619 2445.2 572.45

``` r
tidy(first_model) %>% 
  arrange(p.value)
```

    ## # A tibble: 10 x 5
    ##    term          estimate std.error statistic       p.value
    ##    <chr>            <dbl>     <dbl>     <dbl>         <dbl>
    ##  1 (Intercept)     7.33      1.23        5.95 0.00000000987
    ##  2 failures       -1.34      0.248      -5.41 0.000000155  
    ##  3 sexM            1.93      0.461       4.19 0.0000393    
    ##  4 studytime       0.875     0.261       3.36 0.000913     
    ##  5 goout          -0.501     0.190      -2.64 0.00899      
    ##  6 addressU        0.831     0.471       1.76 0.0792       
    ##  7 activitiesyes  -0.676     0.412      -1.64 0.102        
    ##  8 higheryes       1.26      0.798       1.58 0.115        
    ##  9 absences        0.0349    0.0235      1.48 0.139        
    ## 10 Dalc            0.353     0.248       1.43 0.155

``` r
second_model <- lm(avg_score ~ school + 
                  sex + age + address + 
                  famsize + Pstatus + 
                   Mjob + Fjob + 
                  reason + guardian + traveltime +
                  studytime + failures + 
                  paid + activities +
                  nursery + higher + internet +
                  romantic + famrel + freetime +
                  goout + Dalc + Walc +
                  health + absences, data = second)
second_model <- step(second_model, direction = "backward")
```

    ## Start:  AIC=411.05
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Mjob + Fjob + reason + guardian + traveltime + studytime + 
    ##     failures + paid + activities + nursery + higher + internet + 
    ##     romantic + famrel + freetime + goout + Dalc + Walc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - reason      3    19.069 1379.8 407.23
    ## - guardian    2     2.659 1363.4 407.35
    ## - famrel      1     0.534 1361.3 409.11
    ## - absences    1     0.609 1361.3 409.12
    ## - address     1     0.947 1361.7 409.16
    ## - Pstatus     1     1.078 1361.8 409.17
    ## - romantic    1     2.772 1363.5 409.37
    ## - age         1     3.414 1364.2 409.44
    ## - activities  1     3.422 1364.2 409.44
    ## - famsize     1     4.310 1365.0 409.54
    ## - Walc        1     5.932 1366.7 409.73
    ## - health      1     8.299 1369.0 410.00
    ## - paid        1     9.143 1369.9 410.10
    ## - studytime   1    10.971 1371.7 410.31
    ## - sex         1    13.257 1374.0 410.57
    ## - nursery     1    14.767 1375.5 410.74
    ## - freetime    1    15.252 1376.0 410.80
    ## <none>                    1360.7 411.05
    ## - goout       1    26.478 1387.2 412.07
    ## - Dalc        1    29.677 1390.4 412.43
    ## - traveltime  1    30.954 1391.7 412.58
    ## - internet    1    41.572 1402.3 413.77
    ## - higher      1    56.431 1417.2 415.43
    ## - Fjob        4   111.920 1472.7 415.46
    ## - Mjob        4   118.820 1479.5 416.19
    ## - school      1    79.935 1440.7 418.01
    ## - failures    1    95.276 1456.0 419.67
    ## 
    ## Step:  AIC=407.23
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Mjob + Fjob + guardian + traveltime + studytime + failures + 
    ##     paid + activities + nursery + higher + internet + romantic + 
    ##     famrel + freetime + goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - guardian    2     3.526 1383.3 403.63
    ## - address     1     0.490 1380.3 405.29
    ## - famrel      1     0.620 1380.4 405.30
    ## - Pstatus     1     0.627 1380.4 405.30
    ## - absences    1     0.918 1380.7 405.34
    ## - age         1     1.445 1381.2 405.40
    ## - activities  1     1.988 1381.8 405.46
    ## - famsize     1     3.047 1382.8 405.58
    ## - romantic    1     3.306 1383.1 405.61
    ## - Walc        1     8.381 1388.2 406.18
    ## - paid        1     8.711 1388.5 406.22
    ## - sex         1    13.307 1393.1 406.74
    ## - studytime   1    14.329 1394.1 406.85
    ## - nursery     1    16.201 1396.0 407.06
    ## - health      1    16.676 1396.5 407.12
    ## <none>                    1379.8 407.23
    ## - freetime    1    19.396 1399.2 407.42
    ## - traveltime  1    29.467 1409.3 408.55
    ## - goout       1    33.055 1412.9 408.95
    ## - Dalc        1    34.876 1414.7 409.15
    ## - internet    1    41.279 1421.1 409.86
    ## - Fjob        4   107.488 1487.3 411.01
    ## - higher      1    55.741 1435.5 411.45
    ## - school      1    72.958 1452.8 413.32
    ## - Mjob        4   139.352 1519.2 414.34
    ## - failures    1    96.793 1476.6 415.88
    ## 
    ## Step:  AIC=403.63
    ## avg_score ~ school + sex + age + address + famsize + Pstatus + 
    ##     Mjob + Fjob + traveltime + studytime + failures + paid + 
    ##     activities + nursery + higher + internet + romantic + famrel + 
    ##     freetime + goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - address     1     0.278 1383.6 401.66
    ## - Pstatus     1     0.657 1384.0 401.71
    ## - famrel      1     0.718 1384.0 401.71
    ## - age         1     0.785 1384.1 401.72
    ## - absences    1     1.519 1384.8 401.80
    ## - activities  1     2.385 1385.7 401.90
    ## - romantic    1     3.130 1386.5 401.99
    ## - famsize     1     3.261 1386.6 402.00
    ## - Walc        1     7.407 1390.7 402.47
    ## - paid        1     7.754 1391.1 402.51
    ## - sex         1    12.443 1395.8 403.04
    ## - nursery     1    13.995 1397.3 403.21
    ## - studytime   1    14.383 1397.7 403.26
    ## <none>                    1383.3 403.63
    ## - health      1    17.861 1401.2 403.65
    ## - freetime    1    22.441 1405.8 404.16
    ## - traveltime  1    27.682 1411.0 404.74
    ## - goout       1    32.815 1416.1 405.31
    ## - Dalc        1    35.270 1418.6 405.59
    ## - internet    1    39.882 1423.2 406.09
    ## - Fjob        4   105.370 1488.7 407.16
    ## - higher      1    54.761 1438.1 407.73
    ## - school      1    70.264 1453.6 409.41
    ## - Mjob        4   145.227 1528.6 411.31
    ## - failures    1   105.627 1489.0 413.18
    ## 
    ## Step:  AIC=401.66
    ## avg_score ~ school + sex + age + famsize + Pstatus + Mjob + Fjob + 
    ##     traveltime + studytime + failures + paid + activities + nursery + 
    ##     higher + internet + romantic + famrel + freetime + goout + 
    ##     Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Pstatus     1     0.638 1384.2 399.74
    ## - famrel      1     0.711 1384.3 399.74
    ## - age         1     0.759 1384.4 399.75
    ## - absences    1     1.489 1385.1 399.83
    ## - activities  1     2.233 1385.8 399.92
    ## - famsize     1     3.072 1386.7 400.01
    ## - romantic    1     3.092 1386.7 400.01
    ## - Walc        1     7.588 1391.2 400.52
    ## - paid        1     7.832 1391.4 400.55
    ## - sex         1    12.233 1395.8 401.05
    ## - nursery     1    14.147 1397.8 401.26
    ## - studytime   1    14.655 1398.3 401.32
    ## - health      1    17.585 1401.2 401.65
    ## <none>                    1383.6 401.66
    ## - freetime    1    22.898 1406.5 402.24
    ## - traveltime  1    28.809 1412.4 402.90
    ## - goout       1    33.769 1417.4 403.45
    ## - Dalc        1    35.000 1418.6 403.59
    ## - internet    1    39.615 1423.2 404.10
    ## - Fjob        4   105.107 1488.7 405.16
    ## - higher      1    55.019 1438.6 405.79
    ## - school      1    77.766 1461.4 408.25
    ## - Mjob        4   145.005 1528.6 409.31
    ## - failures    1   108.138 1491.7 411.48
    ## 
    ## Step:  AIC=399.74
    ## avg_score ~ school + sex + age + famsize + Mjob + Fjob + traveltime + 
    ##     studytime + failures + paid + activities + nursery + higher + 
    ##     internet + romantic + famrel + freetime + goout + Dalc + 
    ##     Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - famrel      1     0.634 1384.9 397.81
    ## - age         1     0.695 1384.9 397.82
    ## - absences    1     1.143 1385.4 397.87
    ## - activities  1     2.236 1386.5 397.99
    ## - famsize     1     2.505 1386.8 398.02
    ## - romantic    1     3.000 1387.2 398.08
    ## - paid        1     7.572 1391.8 398.59
    ## - Walc        1     7.750 1392.0 398.61
    ## - sex         1    12.076 1396.3 399.10
    ## - studytime   1    14.271 1398.5 399.35
    ## - nursery     1    14.328 1398.6 399.35
    ## - health      1    17.105 1401.3 399.66
    ## <none>                    1384.2 399.74
    ## - freetime    1    23.563 1407.8 400.39
    ## - traveltime  1    29.049 1413.3 401.00
    ## - goout       1    33.518 1417.8 401.49
    ## - Dalc        1    35.707 1420.0 401.73
    ## - internet    1    41.689 1425.9 402.39
    ## - Fjob        4   104.662 1488.9 403.18
    ## - higher      1    55.138 1439.4 403.87
    ## - school      1    78.484 1462.7 406.39
    ## - Mjob        4   144.373 1528.6 407.31
    ## - failures    1   107.522 1491.8 409.48
    ## 
    ## Step:  AIC=397.81
    ## avg_score ~ school + sex + age + famsize + Mjob + Fjob + traveltime + 
    ##     studytime + failures + paid + activities + nursery + higher + 
    ##     internet + romantic + freetime + goout + Dalc + Walc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - age         1     0.548 1385.4 395.87
    ## - absences    1     0.932 1385.8 395.91
    ## - activities  1     2.447 1387.3 396.09
    ## - famsize     1     2.970 1387.8 396.14
    ## - romantic    1     3.224 1388.1 396.17
    ## - Walc        1     7.229 1392.1 396.63
    ## - paid        1     7.370 1392.2 396.64
    ## - sex         1    12.161 1397.0 397.18
    ## - studytime   1    13.817 1398.7 397.37
    ## - nursery     1    14.062 1398.9 397.39
    ## - health      1    16.494 1401.4 397.67
    ## <none>                    1384.9 397.81
    ## - freetime    1    25.278 1410.2 398.65
    ## - traveltime  1    30.410 1415.3 399.22
    ## - goout       1    32.885 1417.8 399.49
    ## - Dalc        1    36.456 1421.3 399.89
    ## - internet    1    41.089 1426.0 400.40
    ## - Fjob        4   104.374 1489.2 401.22
    ## - higher      1    56.468 1441.3 402.08
    ## - school      1    78.478 1463.4 404.46
    ## - Mjob        4   144.531 1529.4 405.39
    ## - failures    1   107.137 1492.0 407.51
    ## 
    ## Step:  AIC=395.87
    ## avg_score ~ school + sex + famsize + Mjob + Fjob + traveltime + 
    ##     studytime + failures + paid + activities + nursery + higher + 
    ##     internet + romantic + freetime + goout + Dalc + Walc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - absences    1     0.586 1386.0 393.94
    ## - activities  1     2.288 1387.7 394.13
    ## - famsize     1     2.747 1388.2 394.18
    ## - romantic    1     3.455 1388.9 394.26
    ## - Walc        1     7.411 1392.8 394.71
    ## - paid        1     8.191 1393.6 394.80
    ## - sex         1    12.455 1397.9 395.28
    ## - studytime   1    13.463 1398.9 395.39
    ## - nursery     1    14.151 1399.6 395.47
    ## - health      1    16.215 1401.6 395.70
    ## <none>                    1385.4 395.87
    ## - freetime    1    27.164 1412.6 396.92
    ## - traveltime  1    30.314 1415.7 397.27
    ## - goout       1    34.327 1419.8 397.71
    ## - Dalc        1    36.138 1421.6 397.91
    ## - internet    1    41.507 1426.9 398.50
    ## - Fjob        4   104.966 1490.4 399.34
    ## - higher      1    56.609 1442.0 400.16
    ## - school      1    82.682 1468.1 402.97
    ## - Mjob        4   148.137 1533.6 403.82
    ## - failures    1   108.279 1493.7 405.68
    ## 
    ## Step:  AIC=393.94
    ## avg_score ~ school + sex + famsize + Mjob + Fjob + traveltime + 
    ##     studytime + failures + paid + activities + nursery + higher + 
    ##     internet + romantic + freetime + goout + Dalc + Walc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - activities  1     2.435 1388.5 392.21
    ## - famsize     1     2.722 1388.7 392.24
    ## - romantic    1     2.916 1388.9 392.27
    ## - paid        1     8.053 1394.1 392.85
    ## - Walc        1     8.421 1394.4 392.89
    ## - sex         1    12.655 1398.7 393.36
    ## - studytime   1    12.877 1398.9 393.39
    ## - nursery     1    15.849 1401.9 393.72
    ## - health      1    16.236 1402.2 393.77
    ## <none>                    1386.0 393.94
    ## - freetime    1    26.579 1412.6 394.92
    ## - traveltime  1    31.255 1417.3 395.44
    ## - goout       1    33.754 1419.8 395.71
    ## - Dalc        1    36.255 1422.3 395.99
    ## - internet    1    41.812 1427.8 396.60
    ## - Fjob        4   104.609 1490.6 397.36
    ## - higher      1    58.953 1445.0 398.48
    ## - school      1    82.118 1468.1 400.97
    ## - Mjob        4   147.654 1533.7 401.83
    ## - failures    1   109.996 1496.0 403.93
    ## 
    ## Step:  AIC=392.21
    ## avg_score ~ school + sex + famsize + Mjob + Fjob + traveltime + 
    ##     studytime + failures + paid + nursery + higher + internet + 
    ##     romantic + freetime + goout + Dalc + Walc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - famsize     1     2.615 1391.1 390.51
    ## - romantic    1     2.843 1391.3 390.53
    ## - Walc        1     7.413 1395.9 391.05
    ## - paid        1     7.597 1396.0 391.07
    ## - studytime   1    11.969 1400.4 391.56
    ## - sex         1    11.971 1400.4 391.56
    ## - health      1    16.168 1404.6 392.03
    ## - nursery     1    16.686 1405.1 392.09
    ## <none>                    1388.5 392.21
    ## - freetime    1    24.919 1413.4 393.01
    ## - traveltime  1    31.385 1419.8 393.72
    ## - Dalc        1    34.169 1422.6 394.03
    ## - goout       1    35.926 1424.4 394.22
    ## - internet    1    40.119 1428.6 394.68
    ## - Fjob        4   104.599 1493.0 395.62
    ## - higher      1    57.587 1446.0 396.59
    ## - school      1    82.255 1470.7 399.25
    ## - Mjob        4   145.474 1533.9 399.86
    ## - failures    1   107.676 1496.1 401.94
    ## 
    ## Step:  AIC=390.51
    ## avg_score ~ school + sex + Mjob + Fjob + traveltime + studytime + 
    ##     failures + paid + nursery + higher + internet + romantic + 
    ##     freetime + goout + Dalc + Walc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - romantic    1     2.298 1393.4 388.77
    ## - Walc        1     7.546 1398.6 389.36
    ## - paid        1     8.208 1399.3 389.43
    ## - studytime   1    10.396 1401.5 389.68
    ## - sex         1    12.579 1403.6 389.92
    ## - health      1    16.320 1407.4 390.34
    ## <none>                    1391.1 390.51
    ## - nursery     1    19.108 1410.2 390.65
    ## - freetime    1    24.608 1415.7 391.26
    ## - traveltime  1    31.169 1422.2 391.99
    ## - Dalc        1    34.019 1425.1 392.30
    ## - goout       1    36.618 1427.7 392.59
    ## - internet    1    41.005 1432.1 393.07
    ## - Fjob        4   102.401 1493.5 393.66
    ## - higher      1    60.269 1451.3 395.17
    ## - Mjob        4   144.491 1535.5 398.02
    ## - school      1    87.588 1478.7 398.09
    ## - failures    1   114.370 1505.4 400.91
    ## 
    ## Step:  AIC=388.77
    ## avg_score ~ school + sex + Mjob + Fjob + traveltime + studytime + 
    ##     failures + paid + nursery + higher + internet + freetime + 
    ##     goout + Dalc + Walc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Walc        1     7.294 1400.7 387.59
    ## - paid        1     8.197 1401.6 387.69
    ## - studytime   1     9.420 1402.8 387.82
    ## - sex         1    13.559 1406.9 388.29
    ## <none>                    1393.4 388.77
    ## - nursery     1    18.336 1411.7 388.82
    ## - health      1    18.564 1411.9 388.84
    ## - freetime    1    24.629 1418.0 389.52
    ## - traveltime  1    33.052 1426.4 390.45
    ## - Dalc        1    34.973 1428.3 390.66
    ## - goout       1    36.017 1429.4 390.77
    ## - internet    1    38.782 1432.1 391.08
    ## - Fjob        4   102.181 1495.5 391.88
    ## - higher      1    63.807 1457.2 393.80
    ## - school      1    85.768 1479.1 396.15
    ## - Mjob        4   144.059 1537.4 396.21
    ## - failures    1   119.000 1512.4 399.63
    ## 
    ## Step:  AIC=387.59
    ## avg_score ~ school + sex + Mjob + Fjob + traveltime + studytime + 
    ##     failures + paid + nursery + higher + internet + freetime + 
    ##     goout + Dalc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - studytime   1     7.276 1407.9 386.40
    ## - paid        1     7.506 1408.2 386.43
    ## - sex         1    14.812 1415.5 387.24
    ## - health      1    17.719 1418.4 387.56
    ## <none>                    1400.7 387.59
    ## - nursery     1    18.376 1419.0 387.63
    ## - freetime    1    22.484 1423.1 388.09
    ## - Dalc        1    28.306 1429.0 388.73
    ## - goout       1    29.098 1429.8 388.81
    ## - traveltime  1    34.376 1435.0 389.39
    ## - internet    1    38.474 1439.1 389.84
    ## - Fjob        4   101.143 1501.8 390.53
    ## - higher      1    69.926 1470.6 393.24
    ## - Mjob        4   142.592 1543.2 394.81
    ## - school      1    85.457 1486.1 394.88
    ## - failures    1   120.508 1521.2 398.54
    ## 
    ## Step:  AIC=386.4
    ## avg_score ~ school + sex + Mjob + Fjob + traveltime + failures + 
    ##     paid + nursery + higher + internet + freetime + goout + Dalc + 
    ##     health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - paid        1     8.164 1416.1 385.31
    ## - sex         1    11.650 1419.6 385.69
    ## - health      1    16.707 1424.6 386.25
    ## <none>                    1407.9 386.40
    ## - nursery     1    19.042 1427.0 386.51
    ## - freetime    1    21.266 1429.2 386.75
    ## - Dalc        1    29.177 1437.1 387.62
    ## - goout       1    29.286 1437.2 387.63
    ## - traveltime  1    34.148 1442.1 388.16
    ## - Fjob        4    98.854 1506.8 389.05
    ## - internet    1    45.225 1453.2 389.36
    ## - higher      1    74.823 1482.8 392.53
    ## - Mjob        4   135.678 1543.6 392.84
    ## - school      1    84.652 1492.6 393.57
    ## - failures    1   123.090 1531.0 397.56
    ## 
    ## Step:  AIC=385.31
    ## avg_score ~ school + sex + Mjob + Fjob + traveltime + failures + 
    ##     nursery + higher + internet + freetime + goout + Dalc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - sex         1    16.353 1432.5 385.11
    ## - nursery     1    16.541 1432.6 385.13
    ## - health      1    16.611 1432.7 385.14
    ## <none>                    1416.1 385.31
    ## - freetime    1    24.358 1440.5 385.99
    ## - goout       1    30.225 1446.3 386.62
    ## - traveltime  1    35.413 1451.5 387.19
    ## - internet    1    38.544 1454.6 387.52
    ## - Dalc        1    38.926 1455.0 387.57
    ## - Fjob        4   105.234 1521.3 388.56
    ## - higher      1    70.914 1487.0 390.98
    ## - school      1    78.395 1494.5 391.77
    ## - Mjob        4   140.960 1557.0 392.21
    ## - failures    1   120.799 1536.9 396.16
    ## 
    ## Step:  AIC=385.11
    ## avg_score ~ school + Mjob + Fjob + traveltime + failures + nursery + 
    ##     higher + internet + freetime + goout + Dalc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - nursery     1    14.348 1446.8 384.68
    ## - health      1    14.527 1447.0 384.69
    ## <none>                    1432.5 385.11
    ## - Dalc        1    30.353 1462.8 386.40
    ## - freetime    1    30.839 1463.3 386.45
    ## - traveltime  1    31.347 1463.8 386.51
    ## - goout       1    31.707 1464.2 386.55
    ## - internet    1    35.486 1467.9 386.95
    ## - Fjob        4   107.152 1539.6 388.44
    ## - higher      1    68.273 1500.7 390.42
    ## - school      1    69.667 1502.1 390.57
    ## - Mjob        4   137.973 1570.4 391.55
    ## - failures    1   135.966 1568.4 397.35
    ## 
    ## Step:  AIC=384.68
    ## avg_score ~ school + Mjob + Fjob + traveltime + failures + higher + 
    ##     internet + freetime + goout + Dalc + health
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - health      1    15.558 1462.3 384.35
    ## <none>                    1446.8 384.68
    ## - internet    1    29.736 1476.5 385.87
    ## - traveltime  1    30.308 1477.1 385.93
    ## - freetime    1    31.542 1478.3 386.06
    ## - goout       1    32.252 1479.0 386.14
    ## - Dalc        1    32.551 1479.3 386.17
    ## - Fjob        4    98.935 1545.7 387.06
    ## - school      1    59.594 1506.4 389.01
    ## - higher      1    65.549 1512.3 389.63
    ## - Mjob        4   140.455 1587.2 391.22
    ## - failures    1   149.816 1596.6 398.14
    ## 
    ## Step:  AIC=384.35
    ## avg_score ~ school + Mjob + Fjob + traveltime + failures + higher + 
    ##     internet + freetime + goout + Dalc
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## <none>                    1462.3 384.35
    ## - goout       1    24.733 1487.1 384.99
    ## - freetime    1    27.934 1490.3 385.33
    ## - traveltime  1    30.353 1492.7 385.58
    ## - internet    1    33.357 1495.7 385.90
    ## - Fjob        4    96.614 1559.0 386.40
    ## - Dalc        1    40.426 1502.8 386.64
    ## - school      1    62.079 1524.4 388.88
    ## - higher      1    69.145 1531.5 389.61
    ## - Mjob        4   134.321 1596.7 390.15
    ## - failures    1   145.745 1608.1 397.27

``` r
tidy(second_model) %>% 
  arrange(p.value)
```

    ## # A tibble: 17 x 5
    ##    term         estimate std.error statistic  p.value
    ##    <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 failures       -2.48      0.663    -3.74  0.000272
    ##  2 Mjobhealth      3.45      1.29      2.66  0.00865 
    ##  3 Fjobteacher     3.76      1.44      2.62  0.00977 
    ##  4 higheryes       8.60      3.34      2.57  0.0111  
    ##  5 schoolMS        2.56      1.05      2.44  0.0160  
    ##  6 Mjobservices    3.01      1.24      2.42  0.0168  
    ##  7 Dalc           -0.643     0.327    -1.97  0.0511  
    ##  8 Fjobservices    2.63      1.39      1.89  0.0607  
    ##  9 internetyes     1.75      0.980     1.79  0.0761  
    ## 10 traveltime     -0.821     0.482    -1.70  0.0905  
    ## 11 Mjobother       2.12      1.26      1.68  0.0956  
    ## 12 freetime        0.478     0.292     1.64  0.104   
    ## 13 goout          -0.413     0.269    -1.54  0.126   
    ## 14 Fjobother       2.01      1.33      1.51  0.132   
    ## 15 Fjobhealth      1.87      1.60      1.17  0.244   
    ## 16 Mjobteacher     1.32      1.20      1.10  0.273   
    ## 17 (Intercept)    -0.996     3.95     -0.252 0.801

``` r
glance(first_model) $ adj.r.squared
```

    ## [1] 0.2374454

``` r
glance(second_model) $ adj.r.squared
```

    ## [1] 0.2401251
