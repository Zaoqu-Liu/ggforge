# Get colors from palette

Main function for retrieving colors from palettes with intelligent
handling of discrete and continuous data.

## Usage

``` r
get_palette(
  x,
  n = 100,
  palette = "Paired",
  palcolor = NULL,
  type = "auto",
  keep_names = TRUE,
  alpha = 1,
  matched = FALSE,
  reverse = FALSE,
  NA_keep = FALSE,
  NA_color = "grey80",
  transparent = TRUE
)
```

## Arguments

- x:

  Vector of values to map to colors (character, factor, or numeric)

- n:

  Number of colors for continuous palettes

- palette:

  Name of the palette to use

- palcolor:

  Custom colors (overrides palette)

- type:

  Type of palette: "auto", "discrete", or "continuous"

- keep_names:

  Whether to keep names on color vector

- alpha:

  Transparency level (0-1)

- matched:

  Return colors matched to x values

- reverse:

  Reverse the color order

- NA_keep:

  Include color for NA values

- NA_color:

  Color for NA values

- transparent:

  Use true transparency vs color blending

## Value

Named vector of colors

## Examples

``` r
# Discrete palette
get_palette(c("A", "B", "C"), palette = "Paired")
#>         A         B         C 
#> "#A6CEE3" "#1F78B4" "#B2DF8A" 

# Continuous palette
get_palette(1:100, palette = "Spectral", type = "continuous")
#>    [1,1.99] (1.99,2.98] (2.98,3.97] (3.97,4.96] (4.96,5.95] (5.95,6.94] 
#>   "#5E4FA2"   "#5954A4"   "#555AA7"   "#5060AA"   "#4C66AC"   "#476BAF" 
#> (6.94,7.93] (7.93,8.92] (8.92,9.91] (9.91,10.9] (10.9,11.9] (11.9,12.9] 
#>   "#4371B2"   "#3E77B5"   "#3A7DB7"   "#3682BA"   "#3288BC"   "#378EBA" 
#> (12.9,13.9] (13.9,14.9] (14.9,15.8] (15.8,16.8] (16.8,17.8] (17.8,18.8] 
#>   "#3D94B7"   "#429AB5"   "#47A0B3"   "#4CA5B0"   "#52ABAE"   "#57B1AB" 
#> (18.8,19.8] (19.8,20.8] (20.8,21.8] (21.8,22.8] (22.8,23.8] (23.8,24.8] 
#>   "#5CB7A9"   "#61BDA6"   "#67C2A4"   "#6EC5A4"   "#75C8A4"   "#7CCAA4" 
#> (24.8,25.8] (25.8,26.7] (26.7,27.7] (27.7,28.7] (28.7,29.7] (29.7,30.7] 
#>   "#83CDA4"   "#8AD0A4"   "#91D2A4"   "#98D5A4"   "#9FD8A4"   "#A6DBA4" 
#> (30.7,31.7] (31.7,32.7] (32.7,33.7] (33.7,34.7] (34.7,35.6] (35.6,36.6] 
#>   "#ACDDA3"   "#B2E0A2"   "#B8E2A1"   "#BEE5A0"   "#C4E79E"   "#CAE99D" 
#> (36.6,37.6] (37.6,38.6] (38.6,39.6] (39.6,40.6] (40.6,41.6] (41.6,42.6] 
#>   "#D0EC9C"   "#D6EE9B"   "#DCF199"   "#E2F398"   "#E7F599"   "#E9F69D" 
#> (42.6,43.6] (43.6,44.6] (44.6,45.5] (45.5,46.5] (46.5,47.5] (47.5,48.5] 
#>   "#ECF7A1"   "#EEF8A5"   "#F1F9A9"   "#F3FAAD"   "#F6FBB1"   "#F8FCB5" 
#> (48.5,49.5] (49.5,50.5] (50.5,51.5] (51.5,52.5] (52.5,53.5] (53.5,54.5] 
#>   "#FBFDB9"   "#FDFEBD"   "#FEFDBC"   "#FEFAB7"   "#FEF7B1"   "#FEF4AC" 
#> (54.5,55.5] (55.5,56.4] (56.4,57.4] (57.4,58.4] (58.4,59.4] (59.4,60.4] 
#>   "#FEF0A7"   "#FEEDA2"   "#FEEA9C"   "#FEE797"   "#FEE492"   "#FEE18D" 
#> (60.4,61.4] (61.4,62.4] (62.4,63.4] (63.4,64.4] (64.4,65.3] (65.3,66.3] 
#>   "#FDDC88"   "#FDD784"   "#FDD27F"   "#FDCD7B"   "#FDC877"   "#FDC373" 
#> (66.3,67.3] (67.3,68.3] (68.3,69.3] (69.3,70.3] (70.3,71.3] (71.3,72.3] 
#>   "#FDBE6F"   "#FDB96A"   "#FDB466"   "#FDAF62"   "#FCA95E"   "#FBA25B" 
#> (72.3,73.3] (73.3,74.3] (74.3,75.2] (75.2,76.2] (76.2,77.2] (77.2,78.2] 
#>   "#FA9C58"   "#F99555"   "#F88F52"   "#F7884F"   "#F6824C"   "#F67B49" 
#> (78.2,79.2] (79.2,80.2] (80.2,81.2] (81.2,82.2] (82.2,83.2] (83.2,84.2] 
#>   "#F57446"   "#F46E43"   "#F16943"   "#EE6445"   "#EB5F46"   "#E85A47" 
#> (84.2,85.2] (85.2,86.1] (86.1,87.1] (87.1,88.1] (88.1,89.1] (89.1,90.1] 
#>   "#E45648"   "#E1514A"   "#DE4C4B"   "#DB474C"   "#D8434D"   "#D53E4E" 
#> (90.1,91.1] (91.1,92.1] (92.1,93.1] (93.1,94.1]   (94.1,95]     (95,96] 
#>   "#CF384D"   "#CA324C"   "#C42C4B"   "#BF2549"   "#B91F48"   "#B41947" 
#>     (96,97]     (97,98]     (98,99]    (99,100] 
#>   "#AE1345"   "#A90D44"   "#A30743"   "#9E0142" 

# Custom colors
get_palette(c("A", "B", "C"), palcolor = c("red", "blue", "green"))
#>       A       B       C 
#>   "red"  "blue" "green" 
```
