new_e_step__block <- function(data, ...){
  blockr::new_block(
    name = "e_step__block",
    expr = quote(
      echarts4r::e_step_(
        serie = .(serie),
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
      step = blockr::new_string_field(),
      fill = blockr::new_switch_field(FALSE),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      coord_system = blockr::new_string_field("cartesian2d")
    ),
    class = c("e_step__block", "echarts_layer_block")
  )
}

e_step__block <- function(data, ...){
  blockr::initialize_block(new_e_step__block(data, ...), data)
}

#' @method server_output e_step__block
#' @export
server_output.e_step__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_step__block
#' @export
uiOutputBlock.e_step__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_step__block
#' @export
evaluate_block.e_step__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_step__block
#' @export
generate_server.e_step__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_step__block
#' @export
block_combiner.e_step__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
