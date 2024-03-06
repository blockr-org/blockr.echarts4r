new_e_focus_adjacency_p_block <- function(data, ...){
  blockr::new_block(
    name = "e_focus_adjacency_p_block",
    expr = quote(
      echarts4r::e_focus_adjacency_p(
        proxy = .(proxy),
        index = .(index)
      )
    ),
    fields = list(
      proxy = blockr::new_string_field(),
      index = blockr::new_string_field()
    ),
    class = c("e_focus_adjacency_p_block", "echarts_layer_block")
  )
}

#' @export
e_focus_adjacency_p_block <- function(data, ...){
  blockr::initialize_block(new_e_focus_adjacency_p_block(data, ...), data)
}

#' @method server_output e_focus_adjacency_p_block
#' @export
server_output.e_focus_adjacency_p_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_focus_adjacency_p_block
#' @export
uiOutputBlock.e_focus_adjacency_p_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_focus_adjacency_p_block
#' @export
evaluate_block.e_focus_adjacency_p_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_focus_adjacency_p_block
#' @export
generate_server.e_focus_adjacency_p_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_focus_adjacency_p_block
#' @export
block_combiner.e_focus_adjacency_p_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
