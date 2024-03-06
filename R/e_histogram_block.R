new_e_histogram_block <- function(data, ...){
  blockr::new_block(
    name = "e_histogram_block",
    expr = quote(
      echarts4r::e_histogram(
        serie = .(serie),
        breaks = .(breaks),
        name = .(name),
        legend = .(legend),
        bar_width = .(bar_width),
        x_index = .(x_index),
        y_index = .(y_index)
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
      breaks = blockr::new_string_field("Sturges"),
      name = blockr::new_string_field("histogram"),
      legend = blockr::new_switch_field(TRUE),
      bar_width = blockr::new_string_field("99%"),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      y_index = blockr::new_numeric_field(0, -1000, 1000)
    ),
    class = c("e_histogram_block", "echarts_layer_block")
  )
}

#' @export
e_histogram_block <- function(data, ...){
  blockr::initialize_block(new_e_histogram_block(data, ...), data)
}

#' @method server_output e_histogram_block
#' @export
server_output.e_histogram_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_histogram_block
#' @export
uiOutputBlock.e_histogram_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_histogram_block
#' @export
evaluate_block.e_histogram_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_histogram_block
#' @export
generate_server.e_histogram_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_histogram_block
#' @export
block_combiner.e_histogram_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
