#' @import blockr
new_e_morph_block <- function(data, ...){
  blockr::new_block(
    name = "e_morph_block",
    expr = quote(
      echarts4r::e_morph(
        callback = .(callback),
        default = .(default)
      )
    ),
    fields = list(
      callback = blockr::new_string_field(),
      default = blockr::new_numeric_field(1, -1000, 1000)
    ),
    class = c("e_morph_block", "echarts_layer_block")
  )
}

#' @export
e_morph_block <- function(data, ...){
  blockr::initialize_block(new_e_morph_block(data, ...), data)
}

#' @method server_output e_morph_block
#' @export
server_output.e_morph_block <- function (x, result, output) 
{
    echarts4r::renderEcharts4r(result())
}

#' @method uiOutputBlock e_morph_block
#' @export
uiOutputBlock.e_morph_block <- function (x, ns) 
{
    echarts4r::echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_morph_block
#' @export
evaluate_block.e_morph_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_morph_block
#' @export
generate_server.e_morph_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_morph_block
#' @export
block_combiner.e_morph_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
