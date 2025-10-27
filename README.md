# Calculating coordinates based on Azimuth degrees and distances with R

In geospatial analysis, an useful tooç is calculate a geographic
coordinate, using only [Azimuth
degrees](https://en.wikipedia.org/wiki/Azimuth#:~:text=The%20azimuth%20is%20the%20angle%20between%20the%20north%20vector%20and,mapping%2C%20mining%2C%20and%20ballistics)
and distances between the points. It is possible throught earth
curvature and radius:

``` math

\varphi_{2} = \varphi_{1} + \sin \left(\left(90 - \theta \right) * \frac{\pi}{180} \right) * d
```

``` math

\lambda_{2} = \lambda_{1} + \cos \left(\left(90 - \theta \right) * \frac{\pi}{180} \right) * d
```

- $`\varphi_{2}`$: Final latitude;

- $`\varphi_{2}`$: Initial latitude;

- $`\theta`$: Azimuth degrees;

- $`d`$: distance;

- $`\lambda_{2}`$: Final longitude;

- $`\lambda_{2}`$: Initial longitude;

# Required packages

To calculate coordinates, we use the required packages:

- [readxl](https://readxl.tidyverse.org): to import our data;

- [tidyverse](https://tidyverse.tidyverse.org/): to transform and
  visualize data and run loop to calculate multiples coordinates;

- [geosphere](https://readxl.tidyverse.org): to calculate coordinates.

- [sf](https://r-spatial.github.io/sf): to transform table data into a
  shapefile.

``` r
library(readxl)

library(tidyverse)

library(geosphere)

library(sf)
```

# Data

## Importing

We use `read_xlsx()` function to import our data, from `readxl` package.

``` r
coord <- readxl::read_xlsx("coordinates.xlsx")
```

## Visualizing

Next, lets visualize our data.

``` r
coord
```

    ## # A tibble: 30 × 4
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
    ## # ℹ 20 more rows

``` r
coord |> as.data.frame()
```

    ##         long       lat    azimuthe  distance
    ## 1  -35.19630 -8.041286   66.266246 173.26567
    ## 2         NA        NA  -25.554144 142.67860
    ## 3         NA        NA -163.605537 212.63355
    ## 4         NA        NA    9.468649 234.52521
    ## 5         NA        NA  129.068205  43.38277
    ## 6         NA        NA  118.019797 102.67019
    ## 7         NA        NA   27.994240  62.62552
    ## 8         NA        NA   57.822470  42.68872
    ## 9         NA        NA -141.763858 115.77122
    ## 10        NA        NA   87.009513 200.22217
    ## 11        NA        NA -115.273850  34.53868
    ## 12        NA        NA -121.645310 398.16752
    ## 13        NA        NA  -11.313027 174.82057
    ## 14        NA        NA  -52.624631 203.44676
    ## 15        NA        NA  136.644757 264.49022
    ## 16 -35.19622 -8.041044  -48.150741 191.55455
    ## 17        NA        NA   -8.786704 116.26142
    ## 18        NA        NA  -15.296834 189.18589
    ## 19        NA        NA  -33.669049 148.02182
    ## 20        NA        NA  -15.748435 196.30354
    ## 21        NA        NA  -48.051563 138.33351
    ## 22        NA        NA  139.958964 260.82950
    ## 23        NA        NA  172.717910 132.86521
    ## 24        NA        NA  102.651128  92.57782
    ## 25        NA        NA  136.280175 178.10471
    ## 26        NA        NA  136.771586 207.44113
    ## 27        NA        NA -148.445689 111.75969
    ## 28        NA        NA  -71.164384  32.35266
    ## 29        NA        NA  102.568789  36.70519
    ## 30        NA        NA   66.266246 173.26567

# Calculating coordinates

## Creating a coordinates calculating function

As we wante to calculate to multiple rows, but rows valued as NA, we
build a function that identify whether a row to longitude or latitude
are NA, using `if(){}` conditional loop. `id` variable are a row
identification, from 1 to rows count. Detecting a NA row, the function
calculate coordinates, throught `geosphere::destPoint()` R function,
where:

- p: coordinate, on idth row - 1 and 1st and 2nd column;

- b: Azimuth degree, on idth rowe - 1 and 3rd column;

- d: distance: on idth row - 1 and 4th column.

When a coordinate are calculated, longitude (`coordinate[1]`) and
latitude (`coordinate[2]`) are aditioned to long and lat columns on its
respective row.

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

To execute a repeat loop, we use `walk()`, from `purrr` r package, firts
informing the variable id and next the build function.

``` r
purrr::walk(1:nrow(coord), coord_calcule)

coord |> as.data.frame()
```

    ##         long       lat    azimuthe  distance
    ## 1  -35.19630 -8.041286   66.266246 173.26567
    ## 2  -35.19486 -8.040656  -25.554144 142.67860
    ## 3  -35.19542 -8.039492 -163.605537 212.63355
    ## 4  -35.19597 -8.041336    9.468649 234.52521
    ## 5  -35.19562 -8.039244  129.068205  43.38277
    ## 6  -35.19531 -8.039492  118.019797 102.67019
    ## 7  -35.19449 -8.039928   27.994240  62.62552
    ## 8  -35.19422 -8.039428   57.822470  42.68872
    ## 9  -35.19389 -8.039222 -141.763858 115.77122
    ## 10 -35.19454 -8.040044   87.009513 200.22217
    ## 11 -35.19273 -8.039950 -115.273850  34.53868
    ## 12 -35.19301 -8.040083 -121.645310 398.16752
    ## 13 -35.19609 -8.041972  -11.313027 174.82057
    ## 14 -35.19640 -8.040422  -52.624631 203.44676
    ## 15 -35.19787 -8.039306  136.644757 264.49022
    ## 16 -35.19622 -8.041044  -48.150741 191.55455
    ## 17 -35.19751 -8.039889   -8.786704 116.26142
    ## 18 -35.19767 -8.038850  -15.296834 189.18589
    ## 19 -35.19813 -8.037200  -33.669049 148.02182
    ## 20 -35.19887 -8.036086  -15.748435 196.30354
    ## 21 -35.19936 -8.034378  -48.051563 138.33351
    ## 22 -35.20029 -8.033542  139.958964 260.82950
    ## 23 -35.19877 -8.035347  172.717910 132.86521
    ## 24 -35.19861 -8.036539  102.651128  92.57782
    ## 25 -35.19779 -8.036722  136.280175 178.10471
    ## 26 -35.19668 -8.037886  136.771586 207.44113
    ## 27 -35.19539 -8.039253 -148.445689 111.75969
    ## 28 -35.19592 -8.040114  -71.164384  32.35266
    ## 29 -35.19620 -8.040019  102.568789  36.70519
    ## 30 -35.19587 -8.040092   66.266246 173.26567

## Converting coordinates dataframe into a shapefile

Next, we can transform our dtaframe into a shapefile, to a visualize
traject. We use `sf` package. throught `st_as_sf()` and `st_cast()`
functions to turn our dataframe into a shapefile.

``` r
coord_sf <- coord |> 
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674) |> 
  dplyr::summarise(do_union = FALSE) |> 
  sf::st_cast("LINESTRING")

coord_sf
```

    ## Simple feature collection with 1 feature and 0 fields
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: -35.20029 ymin: -8.041972 xmax: -35.19273 ymax: -8.033542
    ## Geodetic CRS:  SIRGAS 2000
    ## # A tibble: 1 × 1
    ##                                                                        geometry
    ##                                                                <LINESTRING [°]>
    ## 1 (-35.1963 -8.041286, -35.19486 -8.040656, -35.19542 -8.039492, -35.19597 -8.…

## Visualizing as a map

``` r
ggplot() +
  geom_sf(data = coord_sf)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
