new_e_correlations_block <- function(data, ...){
  blockr::new_block(
    name = "e_correlations_block",
    expr = quote(
      echarts4r::e_correlations(
        visual_map = .(visual_map)
      )
    ),
    fields = list(
      visual_map = blockr::new_switch_field(TRUE)
    ),
    class = c("e_correlations_block", "echarts_layer_block")
  )
}

e_correlations_block <- function(data, ...){
  blockr::initialize_block(new_e_correlations_block(data, ...), data)
}

#' @method server_output e_correlations_block
#' @export
server_output.e_correlations_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_correlations_block
#' @export
uiOutputBlock.e_correlations_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_correlations_block
#' @export
evaluate_block.e_correlations_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_correlations_block
#' @export
generate_server.e_correlations_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_correlations_block
#' @export
block_combiner.e_correlations_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
