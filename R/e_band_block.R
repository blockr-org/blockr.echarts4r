new_e_band_block <- function(data, ...){
  blockr::new_block(
    name = "e_band_block",
    expr = quote(
      echarts4r::e_band(
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
    class = c("e_band_block", "echarts_layer_block")
  )
}

e_band_block <- function(data, ...){
  blockr::initialize_block(new_e_band_block(data, ...), data)
}

#' @export
server_output.e_band_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_band_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_band_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_band_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_band_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
