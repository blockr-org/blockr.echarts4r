new_e_axis_stagger_block <- function(data, ...){
  blockr::new_block(
    name = "e_axis_stagger_block",
    expr = quote(
      echarts4r::e_axis_stagger()
    ),
    fields = list(
      
    ),
    class = c("e_axis_stagger_block", "echarts_layer_block")
  )
}

e_axis_stagger_block <- function(data, ...){
  blockr::initialize_block(new_e_axis_stagger_block(data, ...), data)
}

#' @method server_output e_axis_stagger_block
#' @export
server_output.e_axis_stagger_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_axis_stagger_block
#' @export
uiOutputBlock.e_axis_stagger_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_axis_stagger_block
#' @export
evaluate_block.e_axis_stagger_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_axis_stagger_block
#' @export
generate_server.e_axis_stagger_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_axis_stagger_block
#' @export
block_combiner.e_axis_stagger_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
