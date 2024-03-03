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

#' @export
server_output.e_correlations_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_correlations_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_correlations_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_correlations_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_correlations_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
