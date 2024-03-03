new_e_funnel__block <- function(data, ...){
  blockr::new_block(
    name = "e_funnel__block",
    expr = quote(
      echarts4r::e_funnel_(
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
    class = c("e_funnel__block", "echarts_layer_block")
  )
}

e_funnel__block <- function(data, ...){
  blockr::initialize_block(new_e_funnel__block(data, ...), data)
}

#' @export
server_output.e_funnel__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_funnel__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_funnel__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_funnel__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_funnel__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
