new_e_glm_block <- function(data, ...){
  blockr::new_block(
    name = "e_glm_block",
    expr = quote(
      echarts4r::e_glm(
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
    class = c("e_glm_block", "echarts_layer_block")
  )
}

e_glm_block <- function(data, ...){
  blockr::initialize_block(new_e_glm_block(data, ...), data)
}

#' @method server_output e_glm_block
#' @export
server_output.e_glm_block <- function (x, result, output) 
{
    renderEcharts4r(result())
}

#' @method uiOutputBlock e_glm_block
#' @export
uiOutputBlock.e_glm_block <- function (x, ns) 
{
    echarts4rOutput(ns("res"))
}

#' @method evaluate_block e_glm_block
#' @export
evaluate_block.e_glm_block <- function (x, data, ...) 
{
    stopifnot(...length() == 0L)
    eval(substitute(data %>% expr, list(expr = generate_code(x))), 
        list(data = data))
}

#' @method generate_server e_glm_block
#' @export
generate_server.e_glm_block <- function (...) 
{
    blockr:::generate_server_block(...)
}

#' @method block_combiner e_glm_block
#' @export
block_combiner.e_glm_block <- function (left, right, ...) 
{
    substitute(left %>% right, list(left = generate_code(left), 
        right = generate_code(right)))
}
