#' @import blockr
new_e_pictorial__block <- function(data, ...){
  blockr::new_block(
    name = "e_pictorial__block",
    expr = quote(
      echarts4r::e_pictorial_(
        serie = .(serie),
        symbol = .(symbol),
        legend = .(legend),
        y_index = .(y_index),
        x_index = .(x_index)
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
      symbol = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000)
    ),
    class = c("e_pictorial__block", "echarts_layer_block")
  )
}

#' @export
e_pictorial__block <- function(data, ...){
  blockr::initialize_block(new_e_pictorial__block(data, ...), data)
}

#' @method server_output e_pictorial__block
#' @export
server_output.e_pictorial__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_pictorial__block
#' @export
uiOutputBlock.e_pictorial__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_pictorial__block
#' @export
evaluate_block.e_pictorial__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_pictorial__block
#' @export
generate_server.e_pictorial__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_pictorial__block
#' @export
block_combiner.e_pictorial__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
