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

e_graph_edges_block <- function(data, ...){
  blockr::initialize_block(new_e_graph_edges_block(data, ...), data)
}

#' @export
server_output.e_graph_edges_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_graph_edges_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_graph_edges_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_graph_edges_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_graph_edges_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
