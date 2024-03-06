new_echarts4rBox_block <- function(data, ...){
  blockr::new_block(
    name = "echarts4rBox_block",
    expr = quote(
      echarts4r::echarts4rBox(
        x = .(x),
        y = .(y),
        text = .(text),
        subtext = .(subtext),
        color = .(color),
        text_color = .(text_color),
        background_color = .(background_color)
      )
    ),
    fields = list(
      x = blockr::new_select_field(function(data) {
    colnames(data)[1]
}, function(data) {
    colnames(data)
}),
      y = blockr::new_string_field(),
      text = blockr::new_string_field(""),
      subtext = blockr::new_string_field(""),
      type = blockr::new_string_field(),
      color = blockr::new_string_field("#ffffff"),
      text_color = blockr::new_string_field("#ffffff"),
      background_color = blockr::new_string_field("#293c55"),
      step = blockr::new_string_field(),
      title_args = blockr::new_string_field(),
      tooltip = blockr::new_string_field()
    ),
    class = c("echarts4rBox_block", "echarts_layer_block")
  )
}

echarts4rBox_block <- function(data, ...){
  blockr::initialize_block(new_echarts4rBox_block(data, ...), data)
}

#' @method server_output echarts4rBox_block
#' @export
server_output.echarts4rBox_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock echarts4rBox_block
#' @export
uiOutputBlock.echarts4rBox_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block echarts4rBox_block
#' @export
evaluate_block.echarts4rBox_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server echarts4rBox_block
#' @export
generate_server.echarts4rBox_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner echarts4rBox_block
#' @export
block_combiner.echarts4rBox_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
