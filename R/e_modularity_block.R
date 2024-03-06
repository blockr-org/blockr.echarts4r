new_e_modularity_block <- function(data, ...){
  blockr::new_block(
    name = "e_modularity_block",
    expr = quote(
      echarts4r::e_modularity(
        modularity = .(modularity)
      )
    ),
    fields = list(
      modularity = blockr::new_switch_field(TRUE)
    ),
    class = c("e_modularity_block", "echarts_layer_block")
  )
}

e_modularity_block <- function(data, ...){
  blockr::initialize_block(new_e_modularity_block(data, ...), data)
}

#' @method server_output e_modularity_block
#' @export
server_output.e_modularity_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_modularity_block
#' @export
uiOutputBlock.e_modularity_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_modularity_block
#' @export
evaluate_block.e_modularity_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_modularity_block
#' @export
generate_server.e_modularity_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_modularity_block
#' @export
block_combiner.e_modularity_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
