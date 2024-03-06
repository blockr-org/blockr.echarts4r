#' @import blockr
new_e_visual_map__block <- function(data, ...){
  blockr::new_block(
    name = "e_visual_map__block",
    expr = quote(
      echarts4r::e_visual_map_(
        serie = .(serie),
        calculable = .(calculable)
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
      calculable = blockr::new_switch_field(TRUE),
      type = blockr::new_string_field()
    ),
    class = c("e_visual_map__block", "echarts_layer_block")
  )
}

#' @export
e_visual_map__block <- function(data, ...){
  blockr::initialize_block(new_e_visual_map__block(data, ...), data)
}

#' @method server_output e_visual_map__block
#' @export
server_output.e_visual_map__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_visual_map__block
#' @export
uiOutputBlock.e_visual_map__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_visual_map__block
#' @export
evaluate_block.e_visual_map__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_visual_map__block
#' @export
generate_server.e_visual_map__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_visual_map__block
#' @export
block_combiner.e_visual_map__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
