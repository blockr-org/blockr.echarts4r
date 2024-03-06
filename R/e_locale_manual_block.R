new_e_locale_manual_block <- function(data, ...){
  blockr::new_block(
    name = "e_locale_manual_block",
    expr = quote(
      echarts4r::e_locale_manual(
        locale = .(locale),
        path = .(path)
      )
    ),
    fields = list(
      locale = blockr::new_string_field(),
      path = blockr::new_string_field()
    ),
    class = c("e_locale_manual_block", "echarts_layer_block")
  )
}

e_locale_manual_block <- function(data, ...){
  blockr::initialize_block(new_e_locale_manual_block(data, ...), data)
}

#' @method server_output e_locale_manual_block
#' @export
server_output.e_locale_manual_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_locale_manual_block
#' @export
uiOutputBlock.e_locale_manual_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_locale_manual_block
#' @export
evaluate_block.e_locale_manual_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_locale_manual_block
#' @export
generate_server.e_locale_manual_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_locale_manual_block
#' @export
block_combiner.e_locale_manual_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
