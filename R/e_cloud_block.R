new_e_cloud_block <- function(data, ...){
  blockr::new_block(
    name = "e_cloud_block",
    expr = quote(
      echarts4r::e_cloud(
        word = .(word),
        freq = .(freq),
        color = .(color),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      word = blockr::new_string_field(),
      freq = blockr::new_string_field(),
      color = blockr::new_string_field(),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_cloud_block", "echarts_layer_block")
  )
}

e_cloud_block <- function(data, ...){
  blockr::initialize_block(new_e_cloud_block(data, ...), data)
}

#' @export
server_output.e_cloud_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_cloud_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_cloud_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_cloud_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_cloud_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
