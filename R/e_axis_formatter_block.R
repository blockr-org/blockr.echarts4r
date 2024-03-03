new_e_axis_formatter_block <- function(data, ...){
  blockr::new_block(
    name = "e_axis_formatter_block",
    expr = quote(
      echarts4r::e_axis_formatter(
        digits = .(digits),
        currency = .(currency)
      )
    ),
    fields = list(
      style = blockr::new_string_field(),
      digits = blockr::new_numeric_field(0, -1000, 1000),
      currency = blockr::new_string_field("USD")
    ),
    class = c("e_axis_formatter_block", "echarts_layer_block")
  )
}

e_axis_formatter_block <- function(data, ...){
  blockr::initialize_block(new_e_axis_formatter_block(data, ...), data)
}

#' @export
server_output.e_axis_formatter_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_axis_formatter_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_axis_formatter_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_axis_formatter_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_axis_formatter_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
