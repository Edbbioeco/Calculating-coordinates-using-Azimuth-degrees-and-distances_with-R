# Calculating coordinates based on Azhimute degrees and distances

``` math

\varphi_{2} = \arcsin \left ( \sin(\varphi_{1}) * \cos \left (\frac{d}{R} \right) + \cos \left (\varphi_{1} \right) * \sin \left (\frac{d}{R} \right) * \cos \left (\theta \right) \right)
```

``` math

\lambda_{2} = \lambda_{1} + \arctan^2 \left(\sin \left(\theta \right) * \sin \left(\frac{d}{R} \right) * \cos \left (\varphi_{1} \right) * \cos \left (\frac{d}{R} \right) - \sin(\varphi_{1}) * \sin(\varphi_{2})\right)
```

# Packages

``` r
library(readxl)

library(tidyverse)

library(geosphere)

library(sf)
```

# Data

## Importing

``` r
coord <- readxl::read_xlsx("coordinates.xlsx")
```

## Visualizing

``` r
coord
```

    ## # A tibble: 30 x 4
    ##     long   lat azimuthe distance
    ##    <dbl> <dbl>    <dbl>    <dbl>
    ##  1 -35.2 -8.04    66.3     173. 
    ##  2  NA   NA      -25.6     143. 
    ##  3  NA   NA     -164.      213. 
    ##  4  NA   NA        9.47    235. 
    ##  5  NA   NA      129.       43.4
    ##  6  NA   NA      118.      103. 
    ##  7  NA   NA       28.0      62.6
    ##  8  NA   NA       57.8      42.7
    ##  9  NA   NA     -142.      116. 
    ## 10  NA   NA       87.0     200. 
    ## # i 20 more rows

``` r
coord |> dplyr::glimpse()
```

    ## Rows: 30
    ## Columns: 4
    ## $ long     <dbl> -35.19630, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ lat      <dbl> -8.041286, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ azimuthe <dbl> 66.266246, -25.554144, -163.605537, 9.468649, 129.068205, 11~
    ## $ distance <dbl> 173.26567, 142.67860, 212.63355, 234.52521, 43.38277, 102.67~

# Calculating coordinates

## Creating a coordinates calculating function

``` r
coord_calcule <- function(id){
  
  if(coord$long[id] |> is.na() | coord$lat[id] |> is.na()){
    
    coordinate <- geosphere::destPoint(p = coord[id - 1, 1:2],
                                       b = coord[id - 1, 3],
                                       d = coord[id - 1, 4])
    
    coord$long[id] <<- coordinate[1]
    
    coord$lat[id] <<- coordinate[2]
    
  }
  
}
```

## Executing loop

``` r
purrr::map(1:nrow(coord), coord_calcule)
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## [1] -8.040656
    ## 
    ## [[3]]
    ## [1] -8.039492
    ## 
    ## [[4]]
    ## [1] -8.041336
    ## 
    ## [[5]]
    ## [1] -8.039244
    ## 
    ## [[6]]
    ## [1] -8.039492
    ## 
    ## [[7]]
    ## [1] -8.039928
    ## 
    ## [[8]]
    ## [1] -8.039428
    ## 
    ## [[9]]
    ## [1] -8.039222
    ## 
    ## [[10]]
    ## [1] -8.040044
    ## 
    ## [[11]]
    ## [1] -8.03995
    ## 
    ## [[12]]
    ## [1] -8.040083
    ## 
    ## [[13]]
    ## [1] -8.041972
    ## 
    ## [[14]]
    ## [1] -8.040422
    ## 
    ## [[15]]
    ## [1] -8.039306
    ## 
    ## [[16]]
    ## NULL
    ## 
    ## [[17]]
    ## [1] -8.039889
    ## 
    ## [[18]]
    ## [1] -8.03885
    ## 
    ## [[19]]
    ## [1] -8.0372
    ## 
    ## [[20]]
    ## [1] -8.036086
    ## 
    ## [[21]]
    ## [1] -8.034378
    ## 
    ## [[22]]
    ## [1] -8.033542
    ## 
    ## [[23]]
    ## [1] -8.035347
    ## 
    ## [[24]]
    ## [1] -8.036539
    ## 
    ## [[25]]
    ## [1] -8.036722
    ## 
    ## [[26]]
    ## [1] -8.037886
    ## 
    ## [[27]]
    ## [1] -8.039253
    ## 
    ## [[28]]
    ## [1] -8.040114
    ## 
    ## [[29]]
    ## [1] -8.040019
    ## 
    ## [[30]]
    ## [1] -8.040092

``` r
coord
```

    ## # A tibble: 30 x 4
    ##     long   lat azimuthe distance
    ##    <dbl> <dbl>    <dbl>    <dbl>
    ##  1 -35.2 -8.04    66.3     173. 
    ##  2 -35.2 -8.04   -25.6     143. 
    ##  3 -35.2 -8.04  -164.      213. 
    ##  4 -35.2 -8.04     9.47    235. 
    ##  5 -35.2 -8.04   129.       43.4
    ##  6 -35.2 -8.04   118.      103. 
    ##  7 -35.2 -8.04    28.0      62.6
    ##  8 -35.2 -8.04    57.8      42.7
    ##  9 -35.2 -8.04  -142.      116. 
    ## 10 -35.2 -8.04    87.0     200. 
    ## # i 20 more rows
