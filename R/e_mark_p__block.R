new_e_mark_p__block <- function(data, ...){
  blockr::new_block(
    name = "e_mark_p__block",
    expr = quote(
      echarts4r::e_mark_p_(
        type = .(type),
        serie_index = .(serie_index)
      )
    ),
    fields = list(
      type = blockr::new_string_field(),
      serie_index = blockr::new_string_field()
    ),
    class = c("e_mark_p__block", "echarts_layer_block")
  )
}

e_mark_p__block <- function(data, ...){
  blockr::initialize_block(new_e_mark_p__block(data, ...), data)
}

#' @export
server_output.e_mark_p__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_mark_p__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_mark_p__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_mark_p__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_mark_p__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}