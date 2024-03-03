new_e_leaflet_block <- function(data, ...){
  blockr::new_block(
    name = "e_leaflet_block",
    expr = quote(
      echarts4r::e_leaflet(
        roam = .(roam)
      )
    ),
    fields = list(
      roam = blockr::new_switch_field(TRUE)
    ),
    class = c("e_leaflet_block", "echarts_layer_block")
  )
}

e_leaflet_block <- function(data, ...){
  blockr::initialize_block(new_e_leaflet_block(data, ...), data)
}

#' @export
server_output.e_leaflet_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_leaflet_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_leaflet_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_leaflet_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_leaflet_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
