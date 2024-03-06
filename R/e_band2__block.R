#' @import blockr
new_e_band2__block <- function(data, ...){
  blockr::new_block(
    name = "e_band2__block",
    expr = quote(
      echarts4r::e_band2_(
        lower = .(lower),
        upper = .(upper),
        legend = .(legend),
        y_index = .(y_index),
        x_index = .(x_index),
        coord_system = .(coord_system)
      )
    ),
    fields = list(
      lower = blockr::new_string_field(),
      upper = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      coord_system = blockr::new_string_field("cartesian2d"),
      itemStyle = blockr::new_string_field()
    ),
    class = c("e_band2__block", "echarts_layer_block")
  )
}

#' @export
e_band2__block <- function(data, ...){
  blockr::initialize_block(new_e_band2__block(data, ...), data)
}

#' @method server_output e_band2__block
#' @export
server_output.e_band2__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_band2__block
#' @export
uiOutputBlock.e_band2__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_band2__block
#' @export
evaluate_block.e_band2__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_band2__block
#' @export
generate_server.e_band2__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_band2__block
#' @export
block_combiner.e_band2__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
