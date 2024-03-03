new_e_scatter_gl_block <- function(data, ...){
  blockr::new_block(
    name = "e_scatter_gl_block",
    expr = quote(
      echarts4r::e_scatter_gl(
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
      coord_system = blockr::new_string_field("geo"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_scatter_gl_block", "echarts_layer_block")
  )
}

e_scatter_gl_block <- function(data, ...){
  blockr::initialize_block(new_e_scatter_gl_block(data, ...), data)
}

#' @export
server_output.e_scatter_gl_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_scatter_gl_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_scatter_gl_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_scatter_gl_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_scatter_gl_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
