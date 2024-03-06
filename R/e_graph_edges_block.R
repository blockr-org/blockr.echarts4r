#' @import blockr
new_e_graph_edges_block <- function(data, ...){
  blockr::new_block(
    name = "e_graph_edges_block",
    expr = quote(
      echarts4r::e_graph_edges(
        edges = .(edges),
        source = .(source),
        target = .(target),
        value = .(value),
        size = .(size),
        color = .(color)
      )
    ),
    fields = list(
      edges = blockr::new_string_field(),
      source = blockr::new_string_field(),
      target = blockr::new_string_field(),
      value = blockr::new_string_field(),
      size = blockr::new_string_field(),
      color = blockr::new_string_field()
    ),
    class = c("e_graph_edges_block", "echarts_layer_block")
  )
}

#' @export
e_graph_edges_block <- function(data, ...){
  blockr::initialize_block(new_e_graph_edges_block(data, ...), data)
}

#' @method server_output e_graph_edges_block
#' @export
server_output.e_graph_edges_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_graph_edges_block
#' @export
uiOutputBlock.e_graph_edges_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_graph_edges_block
#' @export
evaluate_block.e_graph_edges_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_graph_edges_block
#' @export
generate_server.e_graph_edges_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_graph_edges_block
#' @export
block_combiner.e_graph_edges_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
