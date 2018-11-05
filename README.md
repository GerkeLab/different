
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
#> # A tibble: 1,000 x 12
#>    id_01 id_02 col_01 col_02 col_03 col_04 col_05 col_06 col_07 col_08
#>    <fct> <fct> <chr>   <dbl>  <int> <fct>  <fct>  <chr>  <chr>   <int>
#>  1 bl    k     eoavuz  27.7      -9 hh     eo     oltinn vxhzev      2
#>  2 au    k     fskdcr  90.3      26 dw     hi     eojpbn suxasl    -40
#>  3 bi    f     dqrixl   8.37     -4 mt     kf     tmyung wiaxob     22
#>  4 ad    j     tkhgus  93.0      45 lb     ee     xmkuwu vnsauz     -7
#>  5 ah    r     adylmj  56.0      14 aq     ed     zrjlxc qnbrrg     47
#>  6 bf    l     paeklq  30.8      36 dc     gw     edqmhl hxcvfl      7
#>  7 bw    g     kolbvx  98.8      19 ah     kg     jswhwt pnzxms     50
#>  8 ax    h     bszqag  96.8      35 lw     mt     mzshfp cybgdo     -2
#>  9 bd    f     tobucc  84.8       4 as     lt     dciduu kyotzv     38
#> 10 bn    c     yarwco  26.4     -21 br     cz     imdaif ldawva     37
#> # ... with 990 more rows, and 2 more variables: col_09 <fct>, col_10 <chr>
```

``` r
y
#> # A tibble: 995 x 11
#>    id_01 id_02 col_09 col_03 col_01 col_07 col_02 col_08 col_04 col_06
#>    <fct> <fct> <chr>   <int> <chr>  <fct>  <chr>   <int> <fct>  <chr> 
#>  1 bf    a     av         31 aawmxz qwwsco 3.919…    -31 df     nkawia
#>  2 aw    f     hq        -12 hcdiwf axowcy 70.63…     46 ak     hvmcjj
#>  3 an    p     il         13 nzdcdm ydynrz 46.06…    -39 bv     bdpqbq
#>  4 ao    n     ae         48 dqhpji prbrcd 36.49…      7 hz     pjpjqu
#>  5 bp    d     gg         40 ahuhzg qcofmi 6.779…    -25 fi     ndpcph
#>  6 bv    o     mn        -35 cexpfa rutchu 76.75…     30 ij     wiczwh
#>  7 bs    h     ju        -41 msekzp zlbszy 13.69…     43 em     odkupg
#>  8 bw    n     fn        -33 oanvew vuxhvl 26.98…    -44 hd     lragnf
#>  9 ar    l     gf         44 gxfmxu shuwso 66.40…    -36 ao     ucuyhr
#> 10 am    m     il         -4 xvjspm bjoowl 17.52…     -1 ig     zkrxwn
#> # ... with 985 more rows, and 1 more variable: col_10 <chr>
```

Both have ID columns `id_01` and `id_02`, but we know they will differ
in their number of rows and columns and that the rows are
unordered.

``` r
> z <- different::diff_compare(x, y, keys = c("id_01", "id_02"), tolerance = 10^-10)
```

``` r
> z
<diff_tbl: x vs y>
✖ There were 135 differences across 9 cols and 94 rows
```

``` r
> summary(z)
── different: Comparison Summary ───────────────────────────────────────────────────────────────────────────────────────────────
# Dimensions
    set    rows  cols
    ----- ----- -----
    x      1000    12
    y       995    11

# Columns
● 1 column in x is not in y:
    `col_05`
● 11 columns appear in both x and y
  ✔ 2 columns have identical entries: 
    `id_01`, `id_02`
  ✖ 9 columns have differences: 
    `col_01`, `col_02`, `col_03`, `col_04`, `col_06`, `col_07`, `col_08`, `col_09`, `col_10`
# Differences
✖ There were 135 differences across 9 cols and 94 rows
    variable type.x    type.y    state    n_diff diff             
    -----    -----     -----     -----     ----- ------           
    col_01   character character diff         15 <tibble [15 × 7]>
    col_02   numeric   character diff         15 <tibble [15 × 7]>
    col_03   integer   integer   diff         15 <tibble [15 × 7]>
    col_04   factor    factor    diff         15 <tibble [15 × 7]>
    col_06   character character diff         15 <tibble [15 × 7]>
    col_07   character factor    diff         15 <tibble [15 × 7]>
    col_08   integer   integer   diff         15 <tibble [15 × 7]>
    col_09   factor    character diff         15 <tibble [15 × 7]>
    col_10   character character diff         15 <tibble [15 × 7]>
    id_01    factor    factor    same          0 <NULL>           
    id_02    factor    factor    same          0 <NULL>           
    col_05   factor    <NA>      unique_x     NA <NULL>           
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

By default the report will open inside the RStudio Viewer pane, but you
can also explicitly provide a path where the report will be saved using
the `output_file` argument.
