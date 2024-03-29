#' @import blockr
new_e_charts__block <- function(data, ...){
  blockr::new_block(
    name = "e_charts__block",
    expr = quote(
      echarts4r::e_charts_(
        x = .(x),
        dispose = .(dispose),
        draw = .(draw),
        renderer = .(renderer),
        timeline = .(timeline),
        reorder = .(reorder)
      )
    ),
    fields = list(
      x = blockr::new_select_field(function(data) {
    colnames(data)[1]
}, function(data) {
    colnames(data)
}),
      dispose = blockr::new_switch_field(TRUE),
      draw = blockr::new_switch_field(TRUE),
      renderer = blockr::new_string_field("canvas"),
      timeline = blockr::new_switch_field(FALSE),
      reorder = blockr::new_switch_field(TRUE)
    ),
    class = c("e_charts__block")
  )
}

#' @export
e_charts__block <- function(data, ...){
  blockr::initialize_block(new_e_charts__block(data, ...), data)
}

#' @method server_output e_charts__block
#' @export
server_output.e_charts__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_charts__block
#' @export
uiOutputBlock.e_charts__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_charts__block
#' @export
evaluate_block.e_charts__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_charts__block
#' @export
generate_server.e_charts__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_charts__block
#' @export
block_combiner.e_charts__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
