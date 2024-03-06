#' @import blockr
new_e_surface_block <- function(data, ...){
  blockr::new_block(
    name = "e_surface_block",
    expr = quote(
      echarts4r::e_surface(
        y = .(y),
        z = .(z),
        bind = .(bind),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      y = blockr::new_string_field(),
      z = blockr::new_string_field(),
      bind = blockr::new_string_field(),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_surface_block", "echarts_layer_block")
  )
}

#' @export
e_surface_block <- function(data, ...){
  blockr::initialize_block(new_e_surface_block(data, ...), data)
}

#' @method server_output e_surface_block
#' @export
server_output.e_surface_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_surface_block
#' @export
uiOutputBlock.e_surface_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_surface_block
#' @export
evaluate_block.e_surface_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_surface_block
#' @export
generate_server.e_surface_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_surface_block
#' @export
block_combiner.e_surface_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
