new_e_river_block <- function(data, ...){
  blockr::new_block(
    name = "e_river_block",
    expr = quote(
      echarts4r::e_river(
        serie = .(serie),
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
      legend = blockr::new_switch_field(TRUE),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_river_block", "echarts_layer_block")
  )
}

e_river_block <- function(data, ...){
  blockr::initialize_block(new_e_river_block(data, ...), data)
}

#' @method server_output e_river_block
#' @export
server_output.e_river_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_river_block
#' @export
uiOutputBlock.e_river_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_river_block
#' @export
evaluate_block.e_river_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_river_block
#' @export
generate_server.e_river_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_river_block
#' @export
block_combiner.e_river_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
