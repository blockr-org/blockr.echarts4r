new_e_candle__block <- function(data, ...){
  blockr::new_block(
    name = "e_candle__block",
    expr = quote(
      echarts4r::e_candle_(
        opening = .(opening),
        closing = .(closing),
        low = .(low),
        high = .(high),
        legend = .(legend)
      )
    ),
    fields = list(
      opening = blockr::new_string_field(),
      closing = blockr::new_string_field(),
      low = blockr::new_string_field(),
      high = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE)
    ),
    class = c("e_candle__block", "echarts_layer_block")
  )
}

e_candle__block <- function(data, ...){
  blockr::initialize_block(new_e_candle__block(data, ...), data)
}

#' @export
server_output.e_candle__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_candle__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_candle__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_candle__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_candle__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
