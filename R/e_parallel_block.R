new_e_parallel_block <- function(data, ...){
  blockr::new_block(
    name = "e_parallel_block",
    expr = quote(
      echarts4r::e_parallel(
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE),
      opts = blockr::new_string_field()
    ),
    class = c("e_parallel_block", "echarts_layer_block")
  )
}

e_parallel_block <- function(data, ...){
  blockr::initialize_block(new_e_parallel_block(data, ...), data)
}

#' @method server_output e_parallel_block
#' @export
server_output.e_parallel_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_parallel_block
#' @export
uiOutputBlock.e_parallel_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_parallel_block
#' @export
evaluate_block.e_parallel_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_parallel_block
#' @export
generate_server.e_parallel_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_parallel_block
#' @export
block_combiner.e_parallel_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
