#' @import blockr
new_e_resize_block <- function(data, ...){
  blockr::new_block(
    name = "e_resize_block",
    expr = quote(
      echarts4r::e_resize(
        proxy = .(proxy)
      )
    ),
    fields = list(
      proxy = blockr::new_string_field()
    ),
    class = c("e_resize_block", "echarts_layer_block")
  )
}

#' @export
e_resize_block <- function(data, ...){
  blockr::initialize_block(new_e_resize_block(data, ...), data)
}

#' @method server_output e_resize_block
#' @export
server_output.e_resize_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_resize_block
#' @export
uiOutputBlock.e_resize_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_resize_block
#' @export
evaluate_block.e_resize_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_resize_block
#' @export
generate_server.e_resize_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_resize_block
#' @export
block_combiner.e_resize_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
