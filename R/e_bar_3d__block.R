new_e_bar_3d__block <- function(data, ...){
  blockr::new_block(
    name = "e_bar_3d__block",
    expr = quote(
      echarts4r::e_bar_3d_(
        y = .(y),
        z = .(z),
        coord_system = .(coord_system),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      y = blockr::new_string_field(),
      z = blockr::new_string_field(),
      coord_system = blockr::new_string_field("cartesian3D"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_bar_3d__block", "echarts_layer_block")
  )
}

e_bar_3d__block <- function(data, ...){
  blockr::initialize_block(new_e_bar_3d__block(data, ...), data)
}

#' @method server_output e_bar_3d__block
#' @export
server_output.e_bar_3d__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_bar_3d__block
#' @export
uiOutputBlock.e_bar_3d__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_bar_3d__block
#' @export
evaluate_block.e_bar_3d__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_bar_3d__block
#' @export
generate_server.e_bar_3d__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_bar_3d__block
#' @export
block_combiner.e_bar_3d__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
