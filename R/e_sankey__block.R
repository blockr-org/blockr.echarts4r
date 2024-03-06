new_e_sankey__block <- function(data, ...){
  blockr::new_block(
    name = "e_sankey__block",
    expr = quote(
      echarts4r::e_sankey_(
        source = .(source),
        target = .(target),
        value = .(value),
        layout = .(layout),
        rm_x = .(rm_x),
        rm_y = .(rm_y)
      )
    ),
    fields = list(
      source = blockr::new_string_field(),
      target = blockr::new_string_field(),
      value = blockr::new_string_field(),
      layout = blockr::new_string_field("none"),
      rm_x = blockr::new_switch_field(TRUE),
      rm_y = blockr::new_switch_field(TRUE)
    ),
    class = c("e_sankey__block", "echarts_layer_block")
  )
}

e_sankey__block <- function(data, ...){
  blockr::initialize_block(new_e_sankey__block(data, ...), data)
}

#' @method server_output e_sankey__block
#' @export
server_output.e_sankey__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_sankey__block
#' @export
uiOutputBlock.e_sankey__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_sankey__block
#' @export
evaluate_block.e_sankey__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_sankey__block
#' @export
generate_server.e_sankey__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_sankey__block
#' @export
block_combiner.e_sankey__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
