new_e_show_loading_block <- function(data, ...){
  blockr::new_block(
    name = "e_show_loading_block",
    expr = quote(
      echarts4r::e_show_loading(
        hide_overlay = .(hide_overlay),
        text = .(text),
        color = .(color),
        text_color = .(text_color),
        mask_color = .(mask_color),
        zlevel = .(zlevel)
      )
    ),
    fields = list(
      hide_overlay = blockr::new_switch_field(TRUE),
      text = blockr::new_string_field("loading"),
      color = blockr::new_string_field("#c23531"),
      text_color = blockr::new_string_field("#000"),
      mask_color = blockr::new_string_field("rgba(255, 255, 255, 0.8)"),
      zlevel = blockr::new_numeric_field(0, -1000, 1000)
    ),
    class = c("e_show_loading_block", "echarts_layer_block")
  )
}

e_show_loading_block <- function(data, ...){
  blockr::initialize_block(new_e_show_loading_block(data, ...), data)
}

#' @export
server_output.e_show_loading_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_show_loading_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_show_loading_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_show_loading_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_show_loading_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
