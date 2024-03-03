new_e_sankey_block <- function(data, ...){
  blockr::new_block(
    name = "e_sankey_block",
    expr = quote(
      echarts4r::e_sankey(
        source = .(source),
        target = .(target),
        value = .(value),
        layout = .(layout),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      source = blockr::new_string_field(),
      target = blockr::new_string_field(),
      value = blockr::new_string_field(),
      layout = blockr::new_string_field("none"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_sankey_block", "echarts_layer_block")
  )
}

e_sankey_block <- function(data, ...){
  blockr::initialize_block(new_e_sankey_block(data, ...), data)
}

#' @export
server_output.e_sankey_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_sankey_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_sankey_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_sankey_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_sankey_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
