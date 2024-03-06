new_e_tooltip_choro_formatter_block <- function(data, ...){
  blockr::new_block(
    name = "e_tooltip_choro_formatter_block",
    expr = quote(
      echarts4r::e_tooltip_choro_formatter(
        digits = .(digits),
        currency = .(currency)
      )
    ),
    fields = list(
      style = blockr::new_string_field(),
      digits = blockr::new_numeric_field(0, -1000, 1000),
      currency = blockr::new_string_field("USD")
    ),
    class = c("e_tooltip_choro_formatter_block", "echarts_layer_block")
  )
}

#' @export
e_tooltip_choro_formatter_block <- function(data, ...){
  blockr::initialize_block(new_e_tooltip_choro_formatter_block(data, ...), data)
}

#' @method server_output e_tooltip_choro_formatter_block
#' @export
server_output.e_tooltip_choro_formatter_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_tooltip_choro_formatter_block
#' @export
uiOutputBlock.e_tooltip_choro_formatter_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_tooltip_choro_formatter_block
#' @export
evaluate_block.e_tooltip_choro_formatter_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_tooltip_choro_formatter_block
#' @export
generate_server.e_tooltip_choro_formatter_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_tooltip_choro_formatter_block
#' @export
block_combiner.e_tooltip_choro_formatter_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
