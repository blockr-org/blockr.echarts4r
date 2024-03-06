<!-- badges: start -->
<!-- badges: end -->

# blockr.echarts4r

[echarts4r](https://github.com/JohnCoene/echarts4r) for
[blockr](https://github.com/blockr-org/blockr)

:warning: this is still a work in progress, it is used as a 
stress-test for [blockr.generate](https://github.com/blockr-org/blockr.generate).
The code for the package is generated from `build.R`.

## Installation

You can install the development version of blockr.echarts4r from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("blockr-org/blockr.echarts4r")
```

## Example

``` r
library(blockr)
library(blockr.echarts4r)

stack <- new_stack(
  data_block,
  e_charts__block,
  e_scatter__block,
  e_line__block
)

serve_stack(stack)
```

