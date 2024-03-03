library(blockr)
library(echarts4r)
devtools::load_all()

stack <- new_stack(
  data_block,
  e_charts__block,
  e_scatter__block,
  e_line__block
)

serve_stack(stack)
