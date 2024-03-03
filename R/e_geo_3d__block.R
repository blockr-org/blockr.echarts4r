new_e_geo_3d__block <- function(data, ...){
  blockr::new_block(
    name = "e_geo_3d__block",
    expr = quote(
      echarts4r::e_geo_3d_(
        serie = .(serie),
        type = .(type),
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
      type = blockr::new_string_field("world"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_geo_3d__block", "echarts_layer_block")
  )
}

e_geo_3d__block <- function(data, ...){
  blockr::initialize_block(new_e_geo_3d__block(data, ...), data)
}

#' @export
server_output.e_geo_3d__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_geo_3d__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_geo_3d__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_geo_3d__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_geo_3d__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
