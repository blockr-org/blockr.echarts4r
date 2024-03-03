new_e_grid_block <- function(data, ...){
  blockr::new_block(
    name = "e_grid_block",
    expr = quote(
      echarts4r::e_grid()
    ),
    fields = list(
      
    ),
    class = c("e_grid_block", "echarts_layer_block")
  )
}

e_grid_block <- function(data, ...){
  blockr::initialize_block(new_e_grid_block(data, ...), data)
}

#' @export
server_output.e_grid_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_grid_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_grid_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_grid_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_grid_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}