new_e_funnel_block <- function(data, ...){
  blockr::new_block(
    name = "e_funnel_block",
    expr = quote(
      echarts4r::e_funnel(
        values = .(values),
        labels = .(labels),
        legend = .(legend),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      values = blockr::new_string_field(),
      labels = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_funnel_block", "echarts_layer_block")
  )
}

e_funnel_block <- function(data, ...){
  blockr::initialize_block(new_e_funnel_block(data, ...), data)
}

#' @method server_output e_funnel_block
#' @export
server_output.e_funnel_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_funnel_block
#' @export
uiOutputBlock.e_funnel_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_funnel_block
#' @export
evaluate_block.e_funnel_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_funnel_block
#' @export
generate_server.e_funnel_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_funnel_block
#' @export
block_combiner.e_funnel_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
