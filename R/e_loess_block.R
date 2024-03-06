new_e_loess_block <- function(data, ...){
  blockr::new_block(
    name = "e_loess_block",
    expr = quote(
      echarts4r::e_loess(
        formula = .(formula),
        legend = .(legend),
        symbol = .(symbol),
        smooth = .(smooth),
        x_index = .(x_index),
        y_index = .(y_index)
      )
    ),
    fields = list(
      formula = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE),
      symbol = blockr::new_string_field("none"),
      smooth = blockr::new_switch_field(TRUE),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      model_args = blockr::new_string_field()
    ),
    class = c("e_loess_block", "echarts_layer_block")
  )
}

e_loess_block <- function(data, ...){
  blockr::initialize_block(new_e_loess_block(data, ...), data)
}

#' @method server_output e_loess_block
#' @export
server_output.e_loess_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_loess_block
#' @export
uiOutputBlock.e_loess_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_loess_block
#' @export
evaluate_block.e_loess_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_loess_block
#' @export
generate_server.e_loess_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_loess_block
#' @export
block_combiner.e_loess_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
