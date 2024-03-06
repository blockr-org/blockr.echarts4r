#' @import blockr
new_e_scatter_3d_block <- function(data, ...){
  blockr::new_block(
    name = "e_scatter_3d_block",
    expr = quote(
      echarts4r::e_scatter_3d(
        y = .(y),
        z = .(z),
        color = .(color),
        size = .(size),
        bind = .(bind),
        coord_system = .(coord_system),
        rm_x = .(rm_x),
        rm_y = .(rm_y),
        legend = .(legend)
      )
    ),
    fields = list(
      y = blockr::new_string_field(),
      z = blockr::new_string_field(),
      color = blockr::new_string_field(),
      size = blockr::new_string_field(),
      bind = blockr::new_string_field(),
      coord_system = blockr::new_string_field("cartesian3D"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE),
      legend = blockr::new_switch_field(FALSE)
    ),
    class = c("e_scatter_3d_block", "echarts_layer_block")
  )
}

#' @export
e_scatter_3d_block <- function(data, ...){
  blockr::initialize_block(new_e_scatter_3d_block(data, ...), data)
}

#' @method server_output e_scatter_3d_block
#' @export
server_output.e_scatter_3d_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_scatter_3d_block
#' @export
uiOutputBlock.e_scatter_3d_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_scatter_3d_block
#' @export
evaluate_block.e_scatter_3d_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_scatter_3d_block
#' @export
generate_server.e_scatter_3d_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_scatter_3d_block
#' @export
block_combiner.e_scatter_3d_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
