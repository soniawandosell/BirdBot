
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BirdBot

<!-- badges: start -->
<!-- badges: end -->

The goal of BirdBot is to …

## Installation

You can install the development version of BirdBot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soniawandosell/BirdBot")
```

## Example

``` r
library(BirdBot)
#> Loading required package: telegram.bot
#> Warning: package 'telegram.bot' was built under R version 4.2.3
#> Loading required package: xlsx
#> Warning: package 'xlsx' was built under R version 4.2.3
#> Loading required package: lubridate
#> Warning: package 'lubridate' was built under R version 4.2.3
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
#> Loading required package: stringr
#> Warning: package 'stringr' was built under R version 4.2.3
#> Loading required package: magrittr
#> Warning: package 'magrittr' was built under R version 4.2.3
#> Loading required package: dplyr
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: tidyr
#> Warning: package 'tidyr' was built under R version 4.2.3
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:magrittr':
#> 
#>     extract
#> Loading required package: ggpmisc
#> Warning: package 'ggpmisc' was built under R version 4.2.3
#> Loading required package: ggpp
#> Warning: package 'ggpp' was built under R version 4.2.3
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.2.3
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from   
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2
#> 
#> Attaching package: 'ggpp'
#> The following object is masked from 'package:ggplot2':
#> 
#>     annotate
#> Registered S3 method overwritten by 'ggpmisc':
#>   method                  from   
#>   as.character.polynomial polynom
#> Loading required package: grid
#> Loading required package: gridExtra
#> 
#> Attaching package: 'gridExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     combine
#> Loading required package: usethis
#> Warning: package 'usethis' was built under R version 4.2.2
#> Loading required package: randomcoloR
#> Warning: package 'randomcoloR' was built under R version 4.2.3
#> Loading required package: tools
#> Loading required package: sf
#> Warning: package 'sf' was built under R version 4.2.3
#> Linking to GEOS 3.9.3, GDAL 3.5.2, PROJ 8.2.1; sf_use_s2() is TRUE
Settings <- set.settings(dir ="./data/Settings.xlsx",
                         date.format="%d/%m/%Y",
                         map.file="./data/StudyArea.kml")
#> Reading layer `StudyArea' from data source 
#>   `C:\Users\CBM\Desktop\BirdBot\data\StudyArea.kml' using driver `KML'
#> Simple feature collection with 17 features and 2 fields
#> Geometry type: GEOMETRY
#> Dimension:     XYZ
#> Bounding box:  xmin: -3.759324 ymin: 40.43761 xmax: -3.755507 ymax: 40.44023
#> z_range:       zmin: 0 zmax: 641.8424
#> Geodetic CRS:  WGS 84
```

``` r
Settings <- set.database(Settings,
                         db.name = "database", 
                         firstday = "2024-10-28", 
                         lastday = "2024-11-06")
```

``` r
Settings <- add.start(Settings, 
                      mss.welcome= "Welcome to the starlings data collection bot.",
                      mss.commands="There are some commands you should learn:")

Settings <- add.sampling(Settings, 
                         command="nest", 
                         db.name ="database", 
                         description="add info to each nest for today", 
                         sep="-", 
                         today= 0, 
                         type="Info", 
                         mss.filled=" info filled.", 
                         mss.tasks="The task for today was: ", 
                         mss.rewrite="Info already submited", 
                         mss.previous=" was previously filled as:")

Settings <- add.map(Settings, 
                    command="donetoday", 
                    db.name ="database", 
                    description="map of tasks done today", 
                    type="User", 
                    team=NA, 
                    today= 0, 
                    title=NA, 
                    completed="done", 
                    caption="Tasks done today", 
                    mss.empty="No tasks done today")

Settings <- add.messages(Settings, 
                         pic.folder="./Images/", 
                         mss.saved.pic="Picture saved!", 
                         mss.caption.pic="Please send the picture again with a caption.")
```
