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

#' @export
server_output.e_modularity_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_modularity_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_modularity_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_modularity_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_modularity_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
