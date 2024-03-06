new_echarts4r_proxy_block <- function(data, ...){
  blockr::new_block(
    name = "echarts4r_proxy_block",
    expr = quote(
      echarts4r::echarts4r_proxy(
        id = .(id),
        x = .(x),
        timeline = .(timeline),
        reorder = .(reorder)
      )
    ),
    fields = list(
      id = blockr::new_string_field(),
      x = blockr::new_select_field(function(data) {
    colnames(data)[1]
}, function(data) {
    colnames(data)
}),
      timeline = blockr::new_switch_field(FALSE),
      session = blockr::new_string_field(),
      reorder = blockr::new_switch_field(TRUE)
    ),
    class = c("echarts4r_proxy_block", "echarts_layer_block")
  )
}

#' @export
echarts4r_proxy_block <- function(data, ...){
  blockr::initialize_block(new_echarts4r_proxy_block(data, ...), data)
}

#' @method server_output echarts4r_proxy_block
#' @export
server_output.echarts4r_proxy_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock echarts4r_proxy_block
#' @export
uiOutputBlock.echarts4r_proxy_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block echarts4r_proxy_block
#' @export
evaluate_block.echarts4r_proxy_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server echarts4r_proxy_block
#' @export
generate_server.echarts4r_proxy_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner echarts4r_proxy_block
#' @export
block_combiner.echarts4r_proxy_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
