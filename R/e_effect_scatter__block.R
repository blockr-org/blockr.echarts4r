#' @import blockr
new_e_effect_scatter__block <- function(data, ...){
  blockr::new_block(
    name = "e_effect_scatter__block",
    expr = quote(
      echarts4r::e_effect_scatter_(
        serie = .(serie),
        symbol_size = .(symbol_size),
        scale_js = .(scale_js),
        coord_system = .(coord_system),
        legend = .(legend),
        y_index = .(y_index),
        x_index = .(x_index),
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
      symbol_size = blockr::new_numeric_field(1, -1000, 1000),
      scale_js = blockr::new_string_field("function(data){ return data[3];}"),
      coord_system = blockr::new_string_field("cartesian2d"),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_effect_scatter__block", "echarts_layer_block")
  )
}

#' @export
e_effect_scatter__block <- function(data, ...){
  blockr::initialize_block(new_e_effect_scatter__block(data, ...), data)
}

#' @method server_output e_effect_scatter__block
#' @export
server_output.e_effect_scatter__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_effect_scatter__block
#' @export
uiOutputBlock.e_effect_scatter__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_effect_scatter__block
#' @export
evaluate_block.e_effect_scatter__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_effect_scatter__block
#' @export
generate_server.e_effect_scatter__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_effect_scatter__block
#' @export
block_combiner.e_effect_scatter__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
