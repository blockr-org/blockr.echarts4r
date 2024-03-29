#' @import blockr
new_e_geo_block <- function(data, ...){
  blockr::new_block(
    name = "e_geo_block",
    expr = quote(
      echarts4r::e_geo(
        map = .(map),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      map = blockr::new_string_field("world"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_geo_block", "echarts_layer_block")
  )
}

#' @export
e_geo_block <- function(data, ...){
  blockr::initialize_block(new_e_geo_block(data, ...), data)
}

#' @method server_output e_geo_block
#' @export
server_output.e_geo_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_geo_block
#' @export
uiOutputBlock.e_geo_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_geo_block
#' @export
evaluate_block.e_geo_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_geo_block
#' @export
generate_server.e_geo_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_geo_block
#' @export
block_combiner.e_geo_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
