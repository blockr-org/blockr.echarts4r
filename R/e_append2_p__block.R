new_e_append2_p__block <- function(data, ...){
  blockr::new_block(
    name = "e_append2_p__block",
    expr = quote(
      echarts4r::e_append2_p_(
        proxy = .(proxy),
        x = .(x),
        y = .(y),
        z = .(z),
        symbol_size = .(symbol_size)
      )
    ),
    fields = list(
      proxy = blockr::new_string_field(),
      x = blockr::new_select_field(function(data) {
    colnames(data)[1]
}, function(data) {
    colnames(data)
}),
      y = blockr::new_string_field(),
      z = blockr::new_string_field(),
      symbol_size = blockr::new_numeric_field(1, -1000, 1000)
    ),
    class = c("e_append2_p__block", "echarts_layer_block")
  )
}

e_append2_p__block <- function(data, ...){
  blockr::initialize_block(new_e_append2_p__block(data, ...), data)
}

#' @export
server_output.e_append2_p__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_append2_p__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_append2_p__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_append2_p__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_append2_p__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
