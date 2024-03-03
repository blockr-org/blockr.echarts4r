new_e_list_block <- function(data, ...){
  blockr::new_block(
    name = "e_list_block",
    expr = quote(
      echarts4r::e_list(
        list = .(list),
        append = .(append)
      )
    ),
    fields = list(
      list = blockr::new_string_field(),
      append = blockr::new_switch_field(FALSE)
    ),
    class = c("e_list_block", "echarts_layer_block")
  )
}

e_list_block <- function(data, ...){
  blockr::initialize_block(new_e_list_block(data, ...), data)
}

#' @export
server_output.e_list_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_list_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_list_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_list_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_list_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
