new_e_map_register_ui_block <- function(data, ...){
  blockr::new_block(
    name = "e_map_register_ui_block",
    expr = quote(
      echarts4r::e_map_register_ui(
        name = .(name),
        json = .(json),
        async = .(async)
      )
    ),
    fields = list(
      name = blockr::new_string_field(),
      json = blockr::new_string_field(),
      async = blockr::new_switch_field(FALSE)
    ),
    class = c("e_map_register_ui_block", "echarts_layer_block")
  )
}

e_map_register_ui_block <- function(data, ...){
  blockr::initialize_block(new_e_map_register_ui_block(data, ...), data)
}

#' @method server_output e_map_register_ui_block
#' @export
server_output.e_map_register_ui_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_map_register_ui_block
#' @export
uiOutputBlock.e_map_register_ui_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_map_register_ui_block
#' @export
evaluate_block.e_map_register_ui_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_map_register_ui_block
#' @export
generate_server.e_map_register_ui_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_map_register_ui_block
#' @export
block_combiner.e_map_register_ui_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
