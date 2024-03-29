#' @import blockr
new_e_append1_p__block <- function(data, ...){
  blockr::new_block(
    name = "e_append1_p__block",
    expr = quote(
      echarts4r::e_append1_p_(
        proxy = .(proxy),
        x = .(x),
        y = .(y)
      )
    ),
    fields = list(
      proxy = blockr::new_string_field(),
      x = blockr::new_select_field(function(data) {
    colnames(data)[1]
}, function(data) {
    colnames(data)
}),
      y = blockr::new_string_field()
    ),
    class = c("e_append1_p__block", "echarts_layer_block")
  )
}

#' @export
e_append1_p__block <- function(data, ...){
  blockr::initialize_block(new_e_append1_p__block(data, ...), data)
}

#' @method server_output e_append1_p__block
#' @export
server_output.e_append1_p__block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_append1_p__block
#' @export
uiOutputBlock.e_append1_p__block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_append1_p__block
#' @export
evaluate_block.e_append1_p__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_append1_p__block
#' @export
generate_server.e_append1_p__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_append1_p__block
#' @export
block_combiner.e_append1_p__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
