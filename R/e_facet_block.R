#' @import blockr
new_e_facet_block <- function(data, ...){
  blockr::new_block(
    name = "e_facet_block",
    expr = quote(
      echarts4r::e_facet(
        legend_pos = .(legend_pos),
        legend_space = .(legend_space)
      )
    ),
    fields = list(
      legend_pos = blockr::new_string_field("top"),
      legend_space = blockr::new_numeric_field(10, -1000, 1000),
      margin_trbl = blockr::new_string_field()
    ),
    class = c("e_facet_block", "echarts_layer_block")
  )
}

#' @export
e_facet_block <- function(data, ...){
  blockr::initialize_block(new_e_facet_block(data, ...), data)
}

#' @method server_output e_facet_block
#' @export
server_output.e_facet_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_facet_block
#' @export
uiOutputBlock.e_facet_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_facet_block
#' @export
evaluate_block.e_facet_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_facet_block
#' @export
generate_server.e_facet_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_facet_block
#' @export
block_combiner.e_facet_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
