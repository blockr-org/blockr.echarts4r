new_e_charts_block <- function(data, ...){
  blockr::new_block(
    name = "e_charts_block",
    expr = quote(
      echarts4r::e_charts(
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
    class = c("e_charts_block")
  )
}

e_charts_block <- function(data, ...){
  blockr::initialize_block(new_e_charts_block(data, ...), data)
}

#' @export
server_output.e_charts_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_charts_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_charts_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_charts_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_charts_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
