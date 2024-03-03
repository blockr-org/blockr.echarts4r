new_e_angle_axis__block <- function(data, ...){
  blockr::new_block(
    name = "e_angle_axis__block",
    expr = quote(
      echarts4r::e_angle_axis_(
        serie = .(serie),
        show = .(show)
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
      show = blockr::new_switch_field(TRUE)
    ),
    class = c("e_angle_axis__block", "echarts_layer_block")
  )
}

e_angle_axis__block <- function(data, ...){
  blockr::initialize_block(new_e_angle_axis__block(data, ...), data)
}

#' @export
server_output.e_angle_axis__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_angle_axis__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_angle_axis__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_angle_axis__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_angle_axis__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
