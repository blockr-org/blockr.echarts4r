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

#' @method server_output e_mark_p__block
#' @export
server_output.e_mark_p__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_mark_p__block
#' @export
uiOutputBlock.e_mark_p__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_mark_p__block
#' @export
evaluate_block.e_mark_p__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_mark_p__block
#' @export
generate_server.e_mark_p__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_mark_p__block
#' @export
block_combiner.e_mark_p__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
