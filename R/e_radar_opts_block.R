new_e_radar_opts_block <- function(data, ...){
  blockr::new_block(
    name = "e_radar_opts_block",
    expr = quote(
      echarts4r::e_radar_opts(
        index = .(index)
      )
    ),
    fields = list(
      index = blockr::new_numeric_field(0, -1000, 1000)
    ),
    class = c("e_radar_opts_block", "echarts_layer_block")
  )
}

e_radar_opts_block <- function(data, ...){
  blockr::initialize_block(new_e_radar_opts_block(data, ...), data)
}

#' @method server_output e_radar_opts_block
#' @export
server_output.e_radar_opts_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_radar_opts_block
#' @export
uiOutputBlock.e_radar_opts_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_radar_opts_block
#' @export
evaluate_block.e_radar_opts_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_radar_opts_block
#' @export
generate_server.e_radar_opts_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_radar_opts_block
#' @export
block_combiner.e_radar_opts_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
