#' @import blockr
new_e_parallel__block <- function(data, ...){
  blockr::new_block(
    name = "e_parallel__block",
    expr = quote(
      echarts4r::e_parallel_(
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE),
      opts = blockr::new_string_field()
    ),
    class = c("e_parallel__block", "echarts_layer_block")
  )
}

#' @export
e_parallel__block <- function(data, ...){
  blockr::initialize_block(new_e_parallel__block(data, ...), data)
}

#' @method server_output e_parallel__block
#' @export
server_output.e_parallel__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_parallel__block
#' @export
uiOutputBlock.e_parallel__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_parallel__block
#' @export
evaluate_block.e_parallel__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_parallel__block
#' @export
generate_server.e_parallel__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_parallel__block
#' @export
block_combiner.e_parallel__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
