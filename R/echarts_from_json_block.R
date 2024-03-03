new_echarts_from_json_block <- function(data, ...){
  blockr::new_block(
    name = "echarts_from_json_block",
    expr = quote(
      echarts4r::echarts_from_json(
        txt = .(txt),
        jswrapper = .(jswrapper)
      )
    ),
    fields = list(
      txt = blockr::new_string_field(),
      jswrapper = blockr::new_switch_field(FALSE)
    ),
    class = c("echarts_from_json_block", "echarts_layer_block")
  )
}

echarts_from_json_block <- function(data, ...){
  blockr::initialize_block(new_echarts_from_json_block(data, ...), data)
}

#' @export
server_output.echarts_from_json_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.echarts_from_json_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.echarts_from_json_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.echarts_from_json_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.echarts_from_json_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
