new_e_scatter_3d__block <- function(data, ...){
  blockr::new_block(
    name = "e_scatter_3d__block",
    expr = quote(
      echarts4r::e_scatter_3d_(
        y = .(y),
        z = .(z),
        coord_system = .(coord_system),
        rm_x = .(rm_x),
        rm_y = .(rm_y),
        legend = .(legend)
      )
    ),
    fields = list(
      y = blockr::new_string_field(),
      z = blockr::new_string_field(),
      coord_system = blockr::new_string_field("cartesian3D"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE),
      legend = blockr::new_switch_field(FALSE)
    ),
    class = c("e_scatter_3d__block", "echarts_layer_block")
  )
}

e_scatter_3d__block <- function(data, ...){
  blockr::initialize_block(new_e_scatter_3d__block(data, ...), data)
}

#' @export
server_output.e_scatter_3d__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_scatter_3d__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_scatter_3d__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_scatter_3d__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_scatter_3d__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
