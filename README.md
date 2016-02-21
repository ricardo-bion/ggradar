# ggradar




ggradar allows you to build radar charts with ggplot2. This package is based on [Paul Williamson's](http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html) code, with new aesthetics and compatibility with ggplot2 2.0.

It was inspired by [d3radaR](http://www.buildingwidgets.com/blog/2015/12/9/week-49-d3radarr), an htmlwidget built by [timelyportfolio](https://github.com/timelyportfolio).


## Install ggradar


```r
devtools::install_github("ricardo-bion/ggradar", 
                          dependencies=TRUE)
```

## Use ggradar


```r
library(ggradar)

suppressPackageStartupMessages(library(dplyr))
library(scales)

mtcars %>%
     add_rownames( var = "group" ) %>%
     mutate_each(funs(rescale), -group) %>%
     head() %>% select(1:10) -> mtcars_radar

ggradar(mtcars_radar) 
```

![](README_files/figure-html/unnamed-chunk-2-1.png)



```r
knitr::kable(mtcars_radar,format="markdown") 
```



|group             |       mpg| cyl|      disp|        hp|      drat|        wt|      qsec| vs| am|
|:-----------------|---------:|---:|---------:|---------:|---------:|---------:|---------:|--:|--:|
|Mazda RX4         | 0.4510638| 0.5| 0.2217511| 0.2049470| 0.5253456| 0.2830478| 0.2333333|  0|  1|
|Mazda RX4 Wag     | 0.4510638| 0.5| 0.2217511| 0.2049470| 0.5253456| 0.3482485| 0.3000000|  0|  1|
|Datsun 710        | 0.5276596| 0.0| 0.0920429| 0.1448763| 0.5023041| 0.2063411| 0.4892857|  1|  1|
|Hornet 4 Drive    | 0.4680851| 0.5| 0.4662010| 0.2049470| 0.1474654| 0.4351828| 0.5880952|  1|  0|
|Hornet Sportabout | 0.3531915| 1.0| 0.7206286| 0.4346290| 0.1797235| 0.4927129| 0.3000000|  0|  0|
|Valiant           | 0.3276596| 0.5| 0.3838863| 0.1872792| 0.0000000| 0.4978266| 0.6809524|  1|  0|

## Custom fonts

This package requires the Airbnb font and the extrafont package.


```r
# configured to work on a Mac, change directory to Unix or Windows
download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular Air-Light 3.46.45 PM.ttf", "/Library/Fonts/Circular Air-Light 3.46.45 PM.ttf", method="curl")

extrafont::font_import(pattern = 'Circular', prompt=FALSE)
```
