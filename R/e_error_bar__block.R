new_e_error_bar__block <- function(data, ...){
  blockr::new_block(
    name = "e_error_bar__block",
    expr = quote(
      echarts4r::e_error_bar_(
        lower = .(lower),
        upper = .(upper),
        legend = .(legend),
        y_index = .(y_index),
        x_index = .(x_index),
        coord_system = .(coord_system),
        renderer = .(renderer)
      )
    ),
    fields = list(
      lower = blockr::new_string_field(),
      upper = blockr::new_string_field(),
      legend = blockr::new_switch_field(FALSE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      coord_system = blockr::new_string_field("cartesian2d"),
      itemStyle = blockr::new_string_field(),
      renderer = blockr::new_string_field("renderErrorBar2")
    ),
    class = c("e_error_bar__block", "echarts_layer_block")
  )
}

e_error_bar__block <- function(data, ...){
  blockr::initialize_block(new_e_error_bar__block(data, ...), data)
}

#' @method server_output e_error_bar__block
#' @export
server_output.e_error_bar__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_error_bar__block
#' @export
uiOutputBlock.e_error_bar__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_error_bar__block
#' @export
evaluate_block.e_error_bar__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_error_bar__block
#' @export
generate_server.e_error_bar__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_error_bar__block
#' @export
block_combiner.e_error_bar__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
