new_e_error_bar_block <- function(data, ...){
  blockr::new_block(
    name = "e_error_bar_block",
    expr = quote(
      echarts4r::e_error_bar(
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
      legend = blockr::new_switch_field(FALSE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      coord_system = blockr::new_string_field("cartesian2d")
    ),
    class = c("e_error_bar_block", "echarts_layer_block")
  )
}

#' @export
e_error_bar_block <- function(data, ...){
  blockr::initialize_block(new_e_error_bar_block(data, ...), data)
}

#' @method server_output e_error_bar_block
#' @export
server_output.e_error_bar_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_error_bar_block
#' @export
uiOutputBlock.e_error_bar_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_error_bar_block
#' @export
evaluate_block.e_error_bar_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_error_bar_block
#' @export
generate_server.e_error_bar_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_error_bar_block
#' @export
block_combiner.e_error_bar_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
