#' @import blockr
new_e_z_axis_block <- function(data, ...){
  blockr::new_block(
    name = "e_z_axis_block",
    expr = quote(
      echarts4r::e_z_axis(
        serie = .(serie),
        index = .(index),
        margin = .(margin)
      )
    ),
    fields = list(
      serie = blockr::new_select_field(function(data) {
    if (inherits(data$x$data, "list")) 
        return(colnames(data$x$data[[1]])[1])
    colnames(data$x$data)[1]
}, function(data) {
    if (inherits(data$x$data, "list")) 
        return(colnames(data$x$data[[1]]))
    colnames(data$x$data)
}),
      index = blockr::new_numeric_field(0, -1000, 1000),
      margin = blockr::new_numeric_field(0, -1000, 1000)
    ),
    class = c("e_z_axis_block", "echarts_layer_block")
  )
}

#' @export
e_z_axis_block <- function(data, ...){
  blockr::initialize_block(new_e_z_axis_block(data, ...), data)
}

#' @method server_output e_z_axis_block
#' @export
server_output.e_z_axis_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_z_axis_block
#' @export
uiOutputBlock.e_z_axis_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_z_axis_block
#' @export
evaluate_block.e_z_axis_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_z_axis_block
#' @export
generate_server.e_z_axis_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_z_axis_block
#' @export
block_combiner.e_z_axis_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
