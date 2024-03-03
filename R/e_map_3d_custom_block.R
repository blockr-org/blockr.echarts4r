new_e_map_3d_custom_block <- function(data, ...){
  blockr::new_block(
    name = "e_map_3d_custom_block",
    expr = quote(
      echarts4r::e_map_3d_custom(
        id = .(id),
        value = .(value),
        height = .(height),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      id = blockr::new_string_field(),
      value = blockr::new_string_field(),
      height = blockr::new_string_field(),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_map_3d_custom_block", "echarts_layer_block")
  )
}

e_map_3d_custom_block <- function(data, ...){
  blockr::initialize_block(new_e_map_3d_custom_block(data, ...), data)
}

#' @export
server_output.e_map_3d_custom_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_map_3d_custom_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_map_3d_custom_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_map_3d_custom_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_map_3d_custom_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
