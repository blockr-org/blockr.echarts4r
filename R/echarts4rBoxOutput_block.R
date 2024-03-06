#' @import blockr
new_echarts4rBoxOutput_block <- function(data, ...){
  blockr::new_block(
    name = "echarts4rBoxOutput_block",
    expr = quote(
      echarts4r::echarts4rBoxOutput(
        id = .(id),
        height = .(height)
      )
    ),
    fields = list(
      id = blockr::new_string_field(),
      height = blockr::new_numeric_field(150, -1000, 1000)
    ),
    class = c("echarts4rBoxOutput_block", "echarts_layer_block")
  )
}

#' @export
echarts4rBoxOutput_block <- function(data, ...){
  blockr::initialize_block(new_echarts4rBoxOutput_block(data, ...), data)
}

#' @method server_output echarts4rBoxOutput_block
#' @export
server_output.echarts4rBoxOutput_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock echarts4rBoxOutput_block
#' @export
uiOutputBlock.echarts4rBoxOutput_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block echarts4rBoxOutput_block
#' @export
evaluate_block.echarts4rBoxOutput_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server echarts4rBoxOutput_block
#' @export
generate_server.echarts4rBoxOutput_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner echarts4rBoxOutput_block
#' @export
block_combiner.echarts4rBoxOutput_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
