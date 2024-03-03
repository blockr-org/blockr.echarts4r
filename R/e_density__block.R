new_e_density__block <- function(data, ...){
  blockr::new_block(
    name = "e_density__block",
    expr = quote(
      echarts4r::e_density_(
        serie = .(serie),
        breaks = .(breaks),
        legend = .(legend),
        x_index = .(x_index),
        y_index = .(y_index),
        smooth = .(smooth)
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
      legend = blockr::new_switch_field(TRUE),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      smooth = blockr::new_switch_field(TRUE)
    ),
    class = c("e_density__block", "echarts_layer_block")
  )
}

e_density__block <- function(data, ...){
  blockr::initialize_block(new_e_density__block(data, ...), data)
}

#' @export
server_output.e_density__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_density__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_density__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_density__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_density__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}