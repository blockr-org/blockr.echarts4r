library(blockr.generate)

generate_blocks(
  "echarts4r",
  functions = define(
    e_chart = define(input = "data.frame", classes = NA),
    e_charts = define(input = "data.frame", classes = NA),
    e_charts_ = define(input = "data.frame", classes = NA),
    group_by = define(ignore = TRUE),
    echarts4rOutput = define(ignore = TRUE),
    renderEcharts4r = define(ignore = TRUE),
    echarts4rOutputBox = define(ignore = TRUE),
    renderEcharts4rBox = define(ignore = TRUE)
  ),
  all_functions = define(
    input = "echarts4r",
    output = "echarts4r",
    classes = "echarts_layer",
    evaluate_function = function(x, data, ...) {
      stopifnot(...length() == 0L)
      eval(
        substitute(data %>% expr, list(expr = generate_code(x))),
        list(data = data)
      )
    },
    generate_server_function = function(...){
      blockr:::generate_server_block(...)
    },
    output_function = function(x, ns) {
      echarts4r::echarts4rOutput(ns("res"))
    },
    render_function = function(x, result, output) {
      echarts4r::renderEcharts4r(result())
    },
    block_combiner = function(left, right, ...) {
      substitute(
        left %>% right,
        list(left = generate_code(left), right = generate_code(right))
      )
    },
    e = define(ignore = TRUE),
    data = define(ignore = TRUE),
    x = blockr::new_select_field(
      function(data){
        colnames(data)[1]
      },
      function(data){
        colnames(data)
      }
    ),
    scale = define(ignore = TRUE),
    serie = blockr::new_select_field(function(data) {
      if (inherits(data$x$data, "list")) 
        return(colnames(data$x$data[[1]])[1])

      colnames(data$x$data)[1]
    },
    function(data) {
      if (inherits(data$x$data, "list")) 
        return(colnames(data$x$data[[1]]))
      colnames(data$x$data)
    })
  )
)
