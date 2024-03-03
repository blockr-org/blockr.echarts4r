new_e_flow_gl__block <- function(data, ...){
  blockr::new_block(
    name = "e_flow_gl__block",
    expr = quote(
      echarts4r::e_flow_gl_(
        y = .(y),
        sx = .(sx),
        sy = .(sy),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      y = blockr::new_string_field(),
      sx = blockr::new_string_field(),
      sy = blockr::new_string_field(),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_flow_gl__block", "echarts_layer_block")
  )
}

e_flow_gl__block <- function(data, ...){
  blockr::initialize_block(new_e_flow_gl__block(data, ...), data)
}

#' @export
server_output.e_flow_gl__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_flow_gl__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_flow_gl__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_flow_gl__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_flow_gl__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
