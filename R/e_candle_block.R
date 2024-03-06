new_e_candle_block <- function(data, ...){
  blockr::new_block(
    name = "e_candle_block",
    expr = quote(
      echarts4r::e_candle(
        opening = .(opening),
        closing = .(closing),
        low = .(low),
        high = .(high),
        bind = .(bind),
        legend = .(legend)
      )
    ),
    fields = list(
      opening = blockr::new_string_field(),
      closing = blockr::new_string_field(),
      low = blockr::new_string_field(),
      high = blockr::new_string_field(),
      bind = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE)
    ),
    class = c("e_candle_block", "echarts_layer_block")
  )
}

e_candle_block <- function(data, ...){
  blockr::initialize_block(new_e_candle_block(data, ...), data)
}

#' @method server_output e_candle_block
#' @export
server_output.e_candle_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_candle_block
#' @export
uiOutputBlock.e_candle_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_candle_block
#' @export
evaluate_block.e_candle_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_candle_block
#' @export
generate_server.e_candle_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_candle_block
#' @export
block_combiner.e_candle_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
