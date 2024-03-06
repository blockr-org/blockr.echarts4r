new_e_lines_block <- function(data, ...){
  blockr::new_block(
    name = "e_lines_block",
    expr = quote(
      echarts4r::e_lines(
        source_lon = .(source_lon),
        source_lat = .(source_lat),
        target_lon = .(target_lon),
        target_lat = .(target_lat),
        source_name = .(source_name),
        target_name = .(target_name),
        value = .(value),
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
      source_name = blockr::new_string_field(),
      target_name = blockr::new_string_field(),
      value = blockr::new_string_field(),
      coord_system = blockr::new_string_field("geo"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_lines_block", "echarts_layer_block")
  )
}

#' @export
e_lines_block <- function(data, ...){
  blockr::initialize_block(new_e_lines_block(data, ...), data)
}

#' @method server_output e_lines_block
#' @export
server_output.e_lines_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_lines_block
#' @export
uiOutputBlock.e_lines_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_lines_block
#' @export
evaluate_block.e_lines_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_lines_block
#' @export
generate_server.e_lines_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_lines_block
#' @export
block_combiner.e_lines_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
