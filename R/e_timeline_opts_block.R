#' @import blockr
new_e_timeline_opts_block <- function(data, ...){
  blockr::new_block(
    name = "e_timeline_opts_block",
    expr = quote(
      echarts4r::e_timeline_opts(
        axis_type = .(axis_type)
      )
    ),
    fields = list(
      axis_type = blockr::new_string_field("category")
    ),
    class = c("e_timeline_opts_block", "echarts_layer_block")
  )
}

#' @export
e_timeline_opts_block <- function(data, ...){
  blockr::initialize_block(new_e_timeline_opts_block(data, ...), data)
}

#' @method server_output e_timeline_opts_block
#' @export
server_output.e_timeline_opts_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_timeline_opts_block
#' @export
uiOutputBlock.e_timeline_opts_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_timeline_opts_block
#' @export
evaluate_block.e_timeline_opts_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_timeline_opts_block
#' @export
generate_server.e_timeline_opts_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_timeline_opts_block
#' @export
block_combiner.e_timeline_opts_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
