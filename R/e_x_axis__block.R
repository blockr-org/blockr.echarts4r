new_e_x_axis__block <- function(data, ...){
  blockr::new_block(
    name = "e_x_axis__block",
    expr = quote(
      echarts4r::e_x_axis_(
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
    class = c("e_x_axis__block", "echarts_layer_block")
  )
}

e_x_axis__block <- function(data, ...){
  blockr::initialize_block(new_e_x_axis__block(data, ...), data)
}

#' @method server_output e_x_axis__block
#' @export
server_output.e_x_axis__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_x_axis__block
#' @export
uiOutputBlock.e_x_axis__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_x_axis__block
#' @export
evaluate_block.e_x_axis__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_x_axis__block
#' @export
generate_server.e_x_axis__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_x_axis__block
#' @export
block_combiner.e_x_axis__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
