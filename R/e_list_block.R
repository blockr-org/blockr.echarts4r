#' @import blockr
new_e_list_block <- function(data, ...){
  blockr::new_block(
    name = "e_list_block",
    expr = quote(
      echarts4r::e_list(
        list = .(list),
        append = .(append)
      )
    ),
    fields = list(
      list = blockr::new_string_field(),
      append = blockr::new_switch_field(FALSE)
    ),
    class = c("e_list_block", "echarts_layer_block")
  )
}

#' @export
e_list_block <- function(data, ...){
  blockr::initialize_block(new_e_list_block(data, ...), data)
}

#' @method server_output e_list_block
#' @export
server_output.e_list_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_list_block
#' @export
uiOutputBlock.e_list_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_list_block
#' @export
evaluate_block.e_list_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_list_block
#' @export
generate_server.e_list_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_list_block
#' @export
block_combiner.e_list_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
