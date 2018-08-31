
<!-- README.md is generated from README.Rmd. Please edit that file -->

# different

<img align="right" src="man/figures/different-hexlogo.png" width="250px">
Tools for comparing and resolving differences between data frames.

## Installation

``` r
devtools::install_github("gerkelab/different@dev")
```

## Demonstration

Suppose you have two data sets that you would like to compare.

``` r
x
#> # A tibble: 997 x 11
#> # Groups:   id_01, id_02 [997]
#>    id_01 id_02 colname_01 colname_02 colname_04 colname_05 colname_06
#>    <int> <chr>      <dbl>      <int>      <dbl> <fct>      <fct>     
#>  1   148 maqt…       62.7        -32    26.7    c          a         
#>  2   397 bfqt…       55.4         38    46.5    b          h         
#>  3   167 oqxt…       19.7        -33    66.6    c          i         
#>  4   105 vyfo…       26.5        -41    56.0    a          e         
#>  5   583 wzzf…       52.8        -26     0.0360 g          a         
#>  6   693 ykuj…       42.8          2    89.5    f          h         
#>  7   637 kcxq…       18.2        -29    14.5    d          e         
#>  8   777 vcsx…       42.7         19    43.0    c          h         
#>  9   814 mrrz…       73.9         28    69.4    g          d         
#> 10   933 yusl…       25.2        -44    20.4    j          i         
#> # ... with 987 more rows, and 4 more variables: colname_07 <chr>,
#> #   colname_08 <chr>, colname_09 <chr>, colname_10 <fct>
```

``` r
y
#> # A tibble: 993 x 10
#> # Groups:   id_01, id_02 [993]
#>    id_01 id_02 colname_01 colname_02 colname_04 colname_06 colname_07
#>    <int> <chr>      <dbl> <chr>           <dbl> <fct>      <chr>     
#>  1   489 szgh…       9.58 -35              11.7 f          esuqju    
#>  2   198 twkh…      61.7  34               59.6 i          zhdoss    
#>  3   612 ttki…      91.4  7                75.6 h          wbjhhf    
#>  4   873 dsng…      66.7  41               77.5 h          wsudet    
#>  5    24 mnzo…      45.2  -32              63.4 h          yamuei    
#>  6    15 lpzc…      42.6  26               60.4 g          flhcox    
#>  7   757 ygah…      46.3  -42              36.1 b          xrtzih    
#>  8   909 dmjd…      23.9  50               92.2 d          lvudci    
#>  9   717 immn…      17.3  -13              16.0 c          csvjhr    
#> 10   196 ddzh…       5.07 34               72.8 i          orqjut    
#> # ... with 983 more rows, and 3 more variables: colname_08 <chr>,
#> #   colname_09 <chr>, colname_10 <fct>
```

Both have ID columns `id_01` and `id_02`, but we know they will differ
in their number of rows and columns and that the rows are unordered.

``` r
> z <- different::diff_compare(x, y, group_vars = c("id_01", "id_02"))
```

``` r
> z
<diff_tbl: x vs y>
✖ There were 231 differences across 8 cols and 148 rows
```

``` r
> summary(z)
── different: Comparison Summary ─────────────────────────────────────────────────────────
# Dimensions
    set    rows  cols
    ----- ----- -----
    x       997    11
    y       993    10

# Columns
● 1 column in x is not in y:
    `colname_05`
● 10 columns appear in both x and y
  ✔ 2 columns have identical entries: 
    `id_01`, `id_02`
  ✖ 8 columns have differences: 
    `colname_01`, `colname_02`, `colname_04`, `colname_06`, `colname_07`, `colname_08`, 
    `colname_09`, 
    `colname_10`

# Differences
✖ There were 231 differences across 8 cols and 148 rows
    variable   type.x    type.y    state    n_diff diff             
    -----      -----     -----     -----     ----- ------           
    colname_01 numeric   numeric   diff         29 <tibble [29 × 7]>
    colname_02 integer   character diff         30 <tibble [30 × 7]>
    colname_04 numeric   numeric   diff         30 <tibble [30 × 7]>
    colname_06 factor    factor    diff         26 <tibble [26 × 7]>
    colname_07 character character diff         30 <tibble [30 × 7]>
    colname_08 character character diff         30 <tibble [30 × 7]>
    colname_09 character character diff         30 <tibble [30 × 7]>
    colname_10 factor    factor    diff         26 <tibble [26 × 7]>
    id_01      integer   integer   same          0 <NULL>           
    id_02      character character same          0 <NULL>           
    colname_05 factor    <NA>      unique_x     NA <NULL>           
```

``` r
> plot(z)
```

<img src="man/figures/README-diff_tbl-plot-1.png" width="100%" />

You can also create an HTML document reporting all of the found
differences with `diff_report()`.

``` r
different::diff_report(x, y, keep_original = TRUE, use_DT = TRUE)
```
