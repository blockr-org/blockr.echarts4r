new_e_color_range__block <- function(data, ...){
  blockr::new_block(
    name = "e_color_range__block",
    expr = quote(
      echarts4r::e_color_range_(
        input = .(input),
        output = .(output)
      )
    ),
    fields = list(
      input = blockr::new_string_field(),
      output = blockr::new_string_field(),
      colors = blockr::new_string_field()
    ),
    class = c("e_color_range__block", "echarts_layer_block")
  )
}

#' @export
e_color_range__block <- function(data, ...){
  blockr::initialize_block(new_e_color_range__block(data, ...), data)
}

#' @method server_output e_color_range__block
#' @export
server_output.e_color_range__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_color_range__block
#' @export
uiOutputBlock.e_color_range__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_color_range__block
#' @export
evaluate_block.e_color_range__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_color_range__block
#' @export
generate_server.e_color_range__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_color_range__block
#' @export
block_combiner.e_color_range__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
