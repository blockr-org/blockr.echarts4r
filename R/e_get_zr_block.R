new_e_get_zr_block <- function(data, ...){
  blockr::new_block(
    name = "e_get_zr_block",
    expr = quote(
      echarts4r::e_get_zr()
    ),
    fields = list(
      
    ),
    class = c("e_get_zr_block", "echarts_layer_block")
  )
}

e_get_zr_block <- function(data, ...){
  blockr::initialize_block(new_e_get_zr_block(data, ...), data)
}

#' @export
server_output.e_get_zr_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_get_zr_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_get_zr_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_get_zr_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_get_zr_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
