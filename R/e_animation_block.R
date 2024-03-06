#' @import blockr
new_e_animation_block <- function(data, ...){
  blockr::new_block(
    name = "e_animation_block",
    expr = quote(
      echarts4r::e_animation(
        show = .(show)
      )
    ),
    fields = list(
      show = blockr::new_switch_field(TRUE)
    ),
    class = c("e_animation_block", "echarts_layer_block")
  )
}

#' @export
e_animation_block <- function(data, ...){
  blockr::initialize_block(new_e_animation_block(data, ...), data)
}

#' @method server_output e_animation_block
#' @export
server_output.e_animation_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_animation_block
#' @export
uiOutputBlock.e_animation_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_animation_block
#' @export
evaluate_block.e_animation_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_animation_block
#' @export
generate_server.e_animation_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_animation_block
#' @export
block_combiner.e_animation_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
