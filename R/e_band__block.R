new_e_band__block <- function(data, ...){
  blockr::new_block(
    name = "e_band__block",
    expr = quote(
      echarts4r::e_band_(
        min = .(min),
        max = .(max),
        stack = .(stack)
      )
    ),
    fields = list(
      min = blockr::new_string_field(),
      max = blockr::new_string_field(),
      stack = blockr::new_string_field("confidence-band"),
      symbol = blockr::new_string_field(),
      areaStyle = blockr::new_string_field(),
      legend = blockr::new_string_field()
    ),
    class = c("e_band__block", "echarts_layer_block")
  )
}

#' @export
e_band__block <- function(data, ...){
  blockr::initialize_block(new_e_band__block(data, ...), data)
}

#' @method server_output e_band__block
#' @export
server_output.e_band__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_band__block
#' @export
uiOutputBlock.e_band__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_band__block
#' @export
evaluate_block.e_band__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_band__block
#' @export
generate_server.e_band__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_band__block
#' @export
block_combiner.e_band__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
