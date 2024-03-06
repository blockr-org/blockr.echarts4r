new_e_pictorial_block <- function(data, ...){
  blockr::new_block(
    name = "e_pictorial_block",
    expr = quote(
      echarts4r::e_pictorial(
        serie = .(serie),
        symbol = .(symbol),
        bind = .(bind),
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
      bind = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000)
    ),
    class = c("e_pictorial_block", "echarts_layer_block")
  )
}

e_pictorial_block <- function(data, ...){
  blockr::initialize_block(new_e_pictorial_block(data, ...), data)
}

#' @method server_output e_pictorial_block
#' @export
server_output.e_pictorial_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_pictorial_block
#' @export
uiOutputBlock.e_pictorial_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_pictorial_block
#' @export
evaluate_block.e_pictorial_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_pictorial_block
#' @export
generate_server.e_pictorial_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_pictorial_block
#' @export
block_combiner.e_pictorial_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
