new_e_ring_g_block <- function(data, ...){
  blockr::new_block(
    name = "e_ring_g_block",
    expr = quote(
      echarts4r::e_ring_g()
    ),
    fields = list(
      
    ),
    class = c("e_ring_g_block", "echarts_layer_block")
  )
}

e_ring_g_block <- function(data, ...){
  blockr::initialize_block(new_e_ring_g_block(data, ...), data)
}

#' @method server_output e_ring_g_block
#' @export
server_output.e_ring_g_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_ring_g_block
#' @export
uiOutputBlock.e_ring_g_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_ring_g_block
#' @export
evaluate_block.e_ring_g_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_ring_g_block
#' @export
generate_server.e_ring_g_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_ring_g_block
#' @export
block_combiner.e_ring_g_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
