new_e_brush_block <- function(data, ...){
  blockr::new_block(
    name = "e_brush_block",
    expr = quote(
      echarts4r::e_brush(
        brush_link = .(brush_link)
      )
    ),
    fields = list(
      brush_link = blockr::new_string_field("all")
    ),
    class = c("e_brush_block", "echarts_layer_block")
  )
}

#' @export
e_brush_block <- function(data, ...){
  blockr::initialize_block(new_e_brush_block(data, ...), data)
}

#' @method server_output e_brush_block
#' @export
server_output.e_brush_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_brush_block
#' @export
uiOutputBlock.e_brush_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_brush_block
#' @export
evaluate_block.e_brush_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_brush_block
#' @export
generate_server.e_brush_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_brush_block
#' @export
block_combiner.e_brush_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
