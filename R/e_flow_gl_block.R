#' @import blockr
new_e_flow_gl_block <- function(data, ...){
  blockr::new_block(
    name = "e_flow_gl_block",
    expr = quote(
      echarts4r::e_flow_gl(
        y = .(y),
        sx = .(sx),
        sy = .(sy),
        color = .(color),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      y = blockr::new_string_field(),
      sx = blockr::new_string_field(),
      sy = blockr::new_string_field(),
      color = blockr::new_string_field(),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_flow_gl_block", "echarts_layer_block")
  )
}

#' @export
e_flow_gl_block <- function(data, ...){
  blockr::initialize_block(new_e_flow_gl_block(data, ...), data)
}

#' @method server_output e_flow_gl_block
#' @export
server_output.e_flow_gl_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_flow_gl_block
#' @export
uiOutputBlock.e_flow_gl_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_flow_gl_block
#' @export
evaluate_block.e_flow_gl_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_flow_gl_block
#' @export
generate_server.e_flow_gl_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_flow_gl_block
#' @export
block_combiner.e_flow_gl_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
