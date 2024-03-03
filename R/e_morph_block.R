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

e_morph_block <- function(data, ...){
  blockr::initialize_block(new_e_morph_block(data, ...), data)
}

#' @export
server_output.e_morph_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_morph_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_morph_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_morph_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_morph_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
