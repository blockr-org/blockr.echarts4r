new_e_map_block <- function(data, ...){
  blockr::new_block(
    name = "e_map_block",
    expr = quote(
      echarts4r::e_map(
        serie = .(serie),
        map = .(map),
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
      map = blockr::new_string_field("world"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_map_block", "echarts_layer_block")
  )
}

e_map_block <- function(data, ...){
  blockr::initialize_block(new_e_map_block(data, ...), data)
}

#' @export
server_output.e_map_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_map_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_map_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_map_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_map_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
