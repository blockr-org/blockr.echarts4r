new_e_graph_nodes_block <- function(data, ...){
  blockr::new_block(
    name = "e_graph_nodes_block",
    expr = quote(
      echarts4r::e_graph_nodes(
        nodes = .(nodes),
        names = .(names),
        value = .(value),
        size = .(size),
        category = .(category),
        legend = .(legend)
      )
    ),
    fields = list(
      nodes = blockr::new_string_field(),
      names = blockr::new_string_field(),
      value = blockr::new_string_field(),
      size = blockr::new_string_field(),
      category = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE)
    ),
    class = c("e_graph_nodes_block", "echarts_layer_block")
  )
}

e_graph_nodes_block <- function(data, ...){
  blockr::initialize_block(new_e_graph_nodes_block(data, ...), data)
}

#' @export
server_output.e_graph_nodes_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_graph_nodes_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_graph_nodes_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_graph_nodes_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_graph_nodes_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
