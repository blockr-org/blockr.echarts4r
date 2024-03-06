#' @import blockr
new_e_graph_block <- function(data, ...){
  blockr::new_block(
    name = "e_graph_block",
    expr = quote(
      echarts4r::e_graph(
        layout = .(layout),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      layout = blockr::new_string_field("force"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_graph_block", "echarts_layer_block")
  )
}

#' @export
e_graph_block <- function(data, ...){
  blockr::initialize_block(new_e_graph_block(data, ...), data)
}

#' @method server_output e_graph_block
#' @export
server_output.e_graph_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_graph_block
#' @export
uiOutputBlock.e_graph_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_graph_block
#' @export
evaluate_block.e_graph_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_graph_block
#' @export
generate_server.e_graph_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_graph_block
#' @export
block_combiner.e_graph_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
