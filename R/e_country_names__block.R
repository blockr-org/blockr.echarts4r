new_e_country_names__block <- function(data, ...){
  blockr::new_block(
    name = "e_country_names__block",
    expr = quote(
      echarts4r::e_country_names_(
        input = .(input),
        type = .(type)
      )
    ),
    fields = list(
      input = blockr::new_string_field(),
      type = blockr::new_string_field("iso2c")
    ),
    class = c("e_country_names__block", "echarts_layer_block")
  )
}

e_country_names__block <- function(data, ...){
  blockr::initialize_block(new_e_country_names__block(data, ...), data)
}

#' @export
server_output.e_country_names__block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @export
uiOutputBlock.e_country_names__block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @export
evaluate_block.e_country_names__block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @export
generate_server.e_country_names__block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @export
block_combiner.e_country_names__block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}