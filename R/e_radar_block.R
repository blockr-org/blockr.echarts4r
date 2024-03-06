#' @import blockr
new_e_radar_block <- function(data, ...){
  blockr::new_block(
    name = "e_radar_block",
    expr = quote(
      echarts4r::e_radar(
        serie = .(serie),
        max = .(max),
        legend = .(legend),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
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
      max = blockr::new_numeric_field(100, -1000, 1000),
      legend = blockr::new_switch_field(TRUE),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE),
      radar = blockr::new_string_field()
    ),
    class = c("e_radar_block", "echarts_layer_block")
  )
}

#' @export
e_radar_block <- function(data, ...){
  blockr::initialize_block(new_e_radar_block(data, ...), data)
}

#' @method server_output e_radar_block
#' @export
server_output.e_radar_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_radar_block
#' @export
uiOutputBlock.e_radar_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_radar_block
#' @export
evaluate_block.e_radar_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_radar_block
#' @export
generate_server.e_radar_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_radar_block
#' @export
block_combiner.e_radar_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
