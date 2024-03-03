new_e_theme_block <- function(data, ...){
  blockr::new_block(
    name = "e_theme_block",
    expr = quote(
      echarts4r::e_theme()
    ),
    fields = list(
      name = blockr::new_string_field()
    ),
    class = c("e_theme_block", "echarts_layer_block")
  )
}

e_theme_block <- function(data, ...){
  blockr::initialize_block(new_e_theme_block(data, ...), data)
}

#' @export
server_output.e_theme_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_theme_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_theme_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_theme_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_theme_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
