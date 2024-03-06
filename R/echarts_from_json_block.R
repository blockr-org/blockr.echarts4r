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

#' @export
echarts_from_json_block <- function(data, ...){
  blockr::initialize_block(new_echarts_from_json_block(data, ...), data)
}

#' @method server_output echarts_from_json_block
#' @export
server_output.echarts_from_json_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock echarts_from_json_block
#' @export
uiOutputBlock.echarts_from_json_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block echarts_from_json_block
#' @export
evaluate_block.echarts_from_json_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server echarts_from_json_block
#' @export
generate_server.echarts_from_json_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner echarts_from_json_block
#' @export
block_combiner.echarts_from_json_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
