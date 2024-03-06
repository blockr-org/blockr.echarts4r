new_e_labels_block <- function(data, ...){
  blockr::new_block(
    name = "e_labels_block",
    expr = quote(
      echarts4r::e_labels(
        show = .(show),
        position = .(position)
      )
    ),
    fields = list(
      show = blockr::new_switch_field(TRUE),
      position = blockr::new_string_field("top")
    ),
    class = c("e_labels_block", "echarts_layer_block")
  )
}

e_labels_block <- function(data, ...){
  blockr::initialize_block(new_e_labels_block(data, ...), data)
}

#' @method server_output e_labels_block
#' @export
server_output.e_labels_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_labels_block
#' @export
uiOutputBlock.e_labels_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_labels_block
#' @export
evaluate_block.e_labels_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_labels_block
#' @export
generate_server.e_labels_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_labels_block
#' @export
block_combiner.e_labels_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
