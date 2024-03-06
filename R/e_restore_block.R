#' @import blockr
new_e_restore_block <- function(data, ...){
  blockr::new_block(
    name = "e_restore_block",
    expr = quote(
      echarts4r::e_restore()
    ),
    fields = list(
      
    ),
    class = c("e_restore_block", "echarts_layer_block")
  )
}

#' @export
e_restore_block <- function(data, ...){
  blockr::initialize_block(new_e_restore_block(data, ...), data)
}

#' @method server_output e_restore_block
#' @export
server_output.e_restore_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_restore_block
#' @export
uiOutputBlock.e_restore_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_restore_block
#' @export
evaluate_block.e_restore_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_restore_block
#' @export
generate_server.e_restore_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_restore_block
#' @export
block_combiner.e_restore_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
