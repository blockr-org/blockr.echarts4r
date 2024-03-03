new_e_scatter_block <- function(data, ...){
  blockr::new_block(
    name = "e_scatter_block",
    expr = quote(
      echarts4r::e_scatter(
        serie = .(serie),
        size = .(size),
        bind = .(bind),
        symbol_size = .(symbol_size),
        scale_js = .(scale_js),
        coord_system = .(coord_system),
        jitter_factor = .(jitter_factor),
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
      size = blockr::new_string_field(),
      bind = blockr::new_string_field(),
      symbol_size = blockr::new_numeric_field(1, -1000, 1000),
      scale_js = blockr::new_string_field("function(data){ return data[3];}"),
      coord_system = blockr::new_string_field("cartesian2d"),
      jitter_factor = blockr::new_numeric_field(0, -1000, 1000),
      legend = blockr::new_switch_field(TRUE),
      y_index = blockr::new_numeric_field(0, -1000, 1000),
      x_index = blockr::new_numeric_field(0, -1000, 1000),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_scatter_block", "echarts_layer_block")
  )
}

e_scatter_block <- function(data, ...){
  blockr::initialize_block(new_e_scatter_block(data, ...), data)
}

#' @export
server_output.e_scatter_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_scatter_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_scatter_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_scatter_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_scatter_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
