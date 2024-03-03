new_e_draft_block <- function(data, ...){
  blockr::new_block(
    name = "e_draft_block",
    expr = quote(
      echarts4r::e_draft(
        text = .(text),
        size = .(size),
        opacity = .(opacity),
        color = .(color)
      )
    ),
    fields = list(
      text = blockr::new_string_field("DRAFT"),
      size = blockr::new_string_field("120px"),
      opacity = blockr::new_numeric_field(0.4, -1000, 1000),
      color = blockr::new_string_field("#d3d3d3")
    ),
    class = c("e_draft_block", "echarts_layer_block")
  )
}

e_draft_block <- function(data, ...){
  blockr::initialize_block(new_e_draft_block(data, ...), data)
}

#' @export
server_output.e_draft_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_draft_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_draft_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_draft_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_draft_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
