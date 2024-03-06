new_e_get_data_block <- function(data, ...){
  blockr::new_block(
    name = "e_get_data_block",
    expr = quote(
      echarts4r::e_get_data()
    ),
    fields = list(
      
    ),
    class = c("e_get_data_block", "echarts_layer_block")
  )
}

#' @export
e_get_data_block <- function(data, ...){
  blockr::initialize_block(new_e_get_data_block(data, ...), data)
}

#' @method server_output e_get_data_block
#' @export
server_output.e_get_data_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_get_data_block
#' @export
uiOutputBlock.e_get_data_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_get_data_block
#' @export
evaluate_block.e_get_data_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_get_data_block
#' @export
generate_server.e_get_data_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_get_data_block
#' @export
block_combiner.e_get_data_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
