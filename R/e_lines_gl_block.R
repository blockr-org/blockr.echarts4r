new_e_lines_gl_block <- function(data, ...){
  blockr::new_block(
    name = "e_lines_gl_block",
    expr = quote(
      echarts4r::e_lines_gl(
        coord_system = .(coord_system)
      )
    ),
    fields = list(
      coord_system = blockr::new_string_field("geo")
    ),
    class = c("e_lines_gl_block", "echarts_layer_block")
  )
}

#' @export
e_lines_gl_block <- function(data, ...){
  blockr::initialize_block(new_e_lines_gl_block(data, ...), data)
}

#' @method server_output e_lines_gl_block
#' @export
server_output.e_lines_gl_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_lines_gl_block
#' @export
uiOutputBlock.e_lines_gl_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_lines_gl_block
#' @export
evaluate_block.e_lines_gl_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_lines_gl_block
#' @export
generate_server.e_lines_gl_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_lines_gl_block
#' @export
block_combiner.e_lines_gl_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
