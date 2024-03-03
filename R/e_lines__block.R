new_e_lines__block <- function(data, ...){
  blockr::new_block(
    name = "e_lines__block",
    expr = quote(
      echarts4r::e_lines_(
        source_lon = .(source_lon),
        source_lat = .(source_lat),
        target_lon = .(target_lon),
        target_lat = .(target_lat),
        coord_system = .(coord_system),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      source_lon = blockr::new_string_field(),
      source_lat = blockr::new_string_field(),
      target_lon = blockr::new_string_field(),
      target_lat = blockr::new_string_field(),
      coord_system = blockr::new_string_field("geo"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_lines__block", "echarts_layer_block")
  )
}

e_lines__block <- function(data, ...){
  blockr::initialize_block(new_e_lines__block(data, ...), data)
}

#' @export
server_output.e_lines__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_lines__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_lines__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_lines__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_lines__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
