new_e_leaflet_tile_block <- function(data, ...){
  blockr::new_block(
    name = "e_leaflet_tile_block",
    expr = quote(
      echarts4r::e_leaflet_tile(
        template = .(template)
      )
    ),
    fields = list(
      template = blockr::new_string_field("https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png")
    ),
    class = c("e_leaflet_tile_block", "echarts_layer_block")
  )
}

e_leaflet_tile_block <- function(data, ...){
  blockr::initialize_block(new_e_leaflet_tile_block(data, ...), data)
}

#' @method server_output e_leaflet_tile_block
#' @export
server_output.e_leaflet_tile_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_leaflet_tile_block
#' @export
uiOutputBlock.e_leaflet_tile_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_leaflet_tile_block
#' @export
evaluate_block.e_leaflet_tile_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_leaflet_tile_block
#' @export
generate_server.e_leaflet_tile_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_leaflet_tile_block
#' @export
block_combiner.e_leaflet_tile_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
