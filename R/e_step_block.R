new_e_step_block <- function(data, ...){
  blockr::new_block(
    name = "e_step_block",
    expr = quote(
      echarts4r::e_step(
        serie = .(serie),
        bind = .(bind),
        fill = .(fill),
        legend = .(legend),
        y_index = .(y_index),
        x_index = .(x_index),
        coord_system = .(coord_system)
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
      bind = blockr::new_string_field(),
      step = blockr::new_string_field(),
      fill = blockr::new_switch_field(FALSE),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      coord_system = blockr::new_string_field("cartesian2d")
    ),
    class = c("e_step_block", "echarts_layer_block")
  )
}

e_step_block <- function(data, ...){
  blockr::initialize_block(new_e_step_block(data, ...), data)
}

#' @export
server_output.e_step_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_step_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_step_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_step_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_step_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
