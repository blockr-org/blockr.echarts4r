#' @import blockr
new_e_lm_block <- function(data, ...){
  blockr::new_block(
    name = "e_lm_block",
    expr = quote(
      echarts4r::e_lm(
        formula = .(formula),
        legend = .(legend),
        symbol = .(symbol),
        smooth = .(smooth)
      )
    ),
    fields = list(
      formula = blockr::new_string_field(),
      legend = blockr::new_switch_field(TRUE),
      symbol = blockr::new_string_field("none"),
      smooth = blockr::new_switch_field(TRUE),
      model_args = blockr::new_string_field()
    ),
    class = c("e_lm_block", "echarts_layer_block")
  )
}

#' @export
e_lm_block <- function(data, ...){
  blockr::initialize_block(new_e_lm_block(data, ...), data)
}

#' @method server_output e_lm_block
#' @export
server_output.e_lm_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_lm_block
#' @export
uiOutputBlock.e_lm_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_lm_block
#' @export
evaluate_block.e_lm_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_lm_block
#' @export
generate_server.e_lm_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_lm_block
#' @export
block_combiner.e_lm_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
