.onLoad <- function(libname, pkgname){

  blockr::register_block(
    e_add_block,
    "e_add",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_add_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_add_nested_block,
    "e_add_nested",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_add_nested_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_add_unnested_block,
    "e_add_unnested",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_add_unnested_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_angle_axis_block,
    "e_angle_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_angle_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_angle_axis__block,
    "e_angle_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_angle_axis__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_animation_block,
    "e_animation",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_animation_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_append1_p_block,
    "e_append1_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_append1_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_append1_p__block,
    "e_append1_p_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_append1_p__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_append2_p_block,
    "e_append2_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_append2_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_append2_p__block,
    "e_append2_p_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_append2_p__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_arc_g_block,
    "e_arc_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_arc_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_area_block,
    "e_area",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_area_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_area__block,
    "e_area_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_area__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_aria_block,
    "e_aria",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_aria_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_arrange_block,
    "e_arrange",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_arrange_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis_block,
    "e_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis__block,
    "e_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis_3d_block,
    "e_axis_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis_formatter_block,
    "e_axis_formatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis_formatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis_labels_block,
    "e_axis_labels",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis_labels_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis_pointer_block,
    "e_axis_pointer",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis_pointer_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_axis_stagger_block,
    "e_axis_stagger",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_axis_stagger_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_band_block,
    "e_band",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_band_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_band__block,
    "e_band_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_band__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_band2_block,
    "e_band2",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_band2_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_band2__block,
    "e_band2_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_band2__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_bar_block,
    "e_bar",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_bar_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_bar__block,
    "e_bar_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_bar__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_bar_3d_block,
    "e_bar_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_bar_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_bar_3d__block,
    "e_bar_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_bar_3d__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_bezier_curve_g_block,
    "e_bezier_curve_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_bezier_curve_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_boxplot_block,
    "e_boxplot",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_boxplot_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_boxplot__block,
    "e_boxplot_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_boxplot__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_brush_block,
    "e_brush",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_brush_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_button_block,
    "e_button",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_button_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_calendar_block,
    "e_calendar",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_calendar_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_candle_block,
    "e_candle",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_candle_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_candle__block,
    "e_candle_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_candle__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_capture_block,
    "e_capture",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_capture_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_chart_block,
    "e_chart",
    "A block",
    input = "data.frame",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_chart_block")
  )

  blockr::register_block(
    e_charts_block,
    "e_charts",
    "A block",
    input = "data.frame",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_charts_block")
  )

  blockr::register_block(
    e_charts__block,
    "e_charts_",
    "A block",
    input = "data.frame",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_charts__block")
  )

  blockr::register_block(
    e_circle_g_block,
    "e_circle_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_circle_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_cloud_block,
    "e_cloud",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_cloud_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_cloud__block,
    "e_cloud_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_cloud__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_color_block,
    "e_color",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_color_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_color_range_block,
    "e_color_range",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_color_range_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_color_range__block,
    "e_color_range_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_color_range__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_common_block,
    "e_common",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_common_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_connect_block,
    "e_connect",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_connect_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_connect_group_block,
    "e_connect_group",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_connect_group_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_correlations_block,
    "e_correlations",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_correlations_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_country_names_block,
    "e_country_names",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_country_names_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_country_names__block,
    "e_country_names_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_country_names__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_data_block,
    "e_data",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_data_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_datazoom_block,
    "e_datazoom",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_datazoom_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_density_block,
    "e_density",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_density_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_density__block,
    "e_density_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_density__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_dims_block,
    "e_dims",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_dims_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_disconnect_group_block,
    "e_disconnect_group",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_disconnect_group_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_dispatch_action_p_block,
    "e_dispatch_action_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_dispatch_action_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_downplay_block,
    "e_downplay",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_downplay_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_downplay_p_block,
    "e_downplay_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_downplay_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_draft_block,
    "e_draft",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_draft_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_draw_p_block,
    "e_draw_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_draw_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_effect_scatter_block,
    "e_effect_scatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_effect_scatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_effect_scatter__block,
    "e_effect_scatter_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_effect_scatter__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_error_bar_block,
    "e_error_bar",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_error_bar_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_error_bar__block,
    "e_error_bar_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_error_bar__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_execute_block,
    "e_execute",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_execute_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_execute_p_block,
    "e_execute_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_execute_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_facet_block,
    "e_facet",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_facet_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_flip_coords_block,
    "e_flip_coords",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_flip_coords_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_flow_gl_block,
    "e_flow_gl",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_flow_gl_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_flow_gl__block,
    "e_flow_gl_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_flow_gl__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_focus_adjacency_block,
    "e_focus_adjacency",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_focus_adjacency_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_focus_adjacency_p_block,
    "e_focus_adjacency_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_focus_adjacency_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_format_axis_block,
    "e_format_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_format_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_format_x_axis_block,
    "e_format_x_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_format_x_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_format_y_axis_block,
    "e_format_y_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_format_y_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_funnel_block,
    "e_funnel",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_funnel_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_funnel__block,
    "e_funnel_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_funnel__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_gauge_block,
    "e_gauge",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_gauge_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_gauge__block,
    "e_gauge_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_gauge__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_geo_block,
    "e_geo",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_geo_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_geo_3d_block,
    "e_geo_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_geo_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_geo_3d__block,
    "e_geo_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_geo_3d__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_get_data_block,
    "e_get_data",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_get_data_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_get_zr_block,
    "e_get_zr",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_get_zr_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_glm_block,
    "e_glm",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_glm_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_globe_block,
    "e_globe",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_globe_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_graph_block,
    "e_graph",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_graph_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_graph_edges_block,
    "e_graph_edges",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_graph_edges_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_graph_gl_block,
    "e_graph_gl",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_graph_gl_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_graph_nodes_block,
    "e_graph_nodes",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_graph_nodes_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_graphic_g_block,
    "e_graphic_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_graphic_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_grid_block,
    "e_grid",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_grid_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_grid_3d_block,
    "e_grid_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_grid_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_group_block,
    "e_group",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_group_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_group_g_block,
    "e_group_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_group_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_heatmap_block,
    "e_heatmap",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_heatmap_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_heatmap__block,
    "e_heatmap_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_heatmap__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_hide_grid_lines_block,
    "e_hide_grid_lines",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_hide_grid_lines_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_hide_loading_block,
    "e_hide_loading",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_hide_loading_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_hidetip_block,
    "e_hidetip",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_hidetip_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_hidetip_p_block,
    "e_hidetip_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_hidetip_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_highlight_block,
    "e_highlight",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_highlight_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_highlight_p_block,
    "e_highlight_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_highlight_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_histogram_block,
    "e_histogram",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_histogram_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_histogram__block,
    "e_histogram_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_histogram__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_image_g_block,
    "e_image_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_image_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_inspect_block,
    "e_inspect",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_inspect_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_labels_block,
    "e_labels",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_labels_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_leaflet_block,
    "e_leaflet",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_leaflet_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_leaflet_tile_block,
    "e_leaflet_tile",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_leaflet_tile_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_legend_block,
    "e_legend",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_legend_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_legend_scroll_block,
    "e_legend_scroll",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_legend_scroll_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_legend_select_block,
    "e_legend_select",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_legend_select_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_legend_toggle_select_block,
    "e_legend_toggle_select",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_legend_toggle_select_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_legend_unselect_block,
    "e_legend_unselect",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_legend_unselect_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_line_block,
    "e_line",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_line_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_line__block,
    "e_line_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_line__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_line_3d_block,
    "e_line_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_line_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_line_3d__block,
    "e_line_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_line_3d__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_line_g_block,
    "e_line_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_line_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_lines_block,
    "e_lines",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lines_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_lines__block,
    "e_lines_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lines__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_lines_3d_block,
    "e_lines_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lines_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_lines_3d__block,
    "e_lines_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lines_3d__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_lines_gl_block,
    "e_lines_gl",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lines_gl_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_liquid_block,
    "e_liquid",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_liquid_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_liquid__block,
    "e_liquid_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_liquid__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_list_block,
    "e_list",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_list_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_lm_block,
    "e_lm",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lm_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_locale_block,
    "e_locale",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_locale_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_locale_manual_block,
    "e_locale_manual",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_locale_manual_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_loess_block,
    "e_loess",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_loess_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_block,
    "e_map",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map__block,
    "e_map_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_3d_block,
    "e_map_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_3d__block,
    "e_map_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_3d__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_3d_custom_block,
    "e_map_3d_custom",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_3d_custom_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_register_block,
    "e_map_register",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_register_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_register_p_block,
    "e_map_register_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_register_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_register_ui_block,
    "e_map_register_ui",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_register_ui_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_select_block,
    "e_map_select",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_select_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_toggle_select_block,
    "e_map_toggle_select",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_toggle_select_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_map_unselect_block,
    "e_map_unselect",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map_unselect_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_mapbox_block,
    "e_mapbox",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mapbox_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_mark_area_block,
    "e_mark_area",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mark_area_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_mark_line_block,
    "e_mark_line",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mark_line_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_mark_p_block,
    "e_mark_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mark_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_mark_p__block,
    "e_mark_p_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mark_p__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_mark_point_block,
    "e_mark_point",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mark_point_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_merge_block,
    "e_merge",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_merge_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_modularity_block,
    "e_modularity",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_modularity_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_morph_block,
    "e_morph",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_morph_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_off_block,
    "e_off",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_off_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_on_block,
    "e_on",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_on_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_parallel_block,
    "e_parallel",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_parallel_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_parallel__block,
    "e_parallel_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_parallel__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_pictorial_block,
    "e_pictorial",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pictorial_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_pictorial__block,
    "e_pictorial_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pictorial__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_pie_block,
    "e_pie",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pie_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_pie__block,
    "e_pie_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pie__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_pie_select_block,
    "e_pie_select",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pie_select_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_pie_unselect_block,
    "e_pie_unselect",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pie_unselect_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_polar_block,
    "e_polar",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_polar_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_polygon_g_block,
    "e_polygon_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_polygon_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_polyline_g_block,
    "e_polyline_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_polyline_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_radar_block,
    "e_radar",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_radar_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_radar__block,
    "e_radar_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_radar__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_radar_opts_block,
    "e_radar_opts",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_radar_opts_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_radius_axis_block,
    "e_radius_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_radius_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_radius_axis__block,
    "e_radius_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_radius_axis__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_rect_g_block,
    "e_rect_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_rect_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_remove_serie_block,
    "e_remove_serie",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_remove_serie_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_remove_serie_p_block,
    "e_remove_serie_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_remove_serie_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_resize_block,
    "e_resize",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_resize_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_restore_block,
    "e_restore",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_restore_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_ring_g_block,
    "e_ring_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_ring_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_river_block,
    "e_river",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_river_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_river__block,
    "e_river_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_river__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_rm_axis_block,
    "e_rm_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_rm_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_sankey_block,
    "e_sankey",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_sankey_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_sankey__block,
    "e_sankey_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_sankey__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scale_block,
    "e_scale",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scale_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scatter_block,
    "e_scatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scatter__block,
    "e_scatter_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scatter_3d_block,
    "e_scatter_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scatter_3d__block,
    "e_scatter_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter_3d__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scatter_gl_block,
    "e_scatter_gl",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter_gl_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_scatter_gl__block,
    "e_scatter_gl_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter_gl__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_sector_g_block,
    "e_sector_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_sector_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_show_loading_block,
    "e_show_loading",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_show_loading_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_showtip_block,
    "e_showtip",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_showtip_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_showtip_p_block,
    "e_showtip_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_showtip_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_single_axis_block,
    "e_single_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_single_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_step_block,
    "e_step",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_step_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_step__block,
    "e_step_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_step__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_sunburst_block,
    "e_sunburst",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_sunburst_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_sunburst__block,
    "e_sunburst_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_sunburst__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_surface_block,
    "e_surface",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_surface_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_surface__block,
    "e_surface_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_surface__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_svg_block,
    "e_svg",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_svg_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_svg__block,
    "e_svg_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_svg__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_svg_register_block,
    "e_svg_register",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_svg_register_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_text_g_block,
    "e_text_g",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_text_g_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_text_style_block,
    "e_text_style",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_text_style_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_theme_block,
    "e_theme",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_theme_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_theme_custom_block,
    "e_theme_custom",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_theme_custom_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_theme_register_block,
    "e_theme_register",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_theme_register_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_timeline_on_serie_block,
    "e_timeline_on_serie",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_timeline_on_serie_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_timeline_opts_block,
    "e_timeline_opts",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_timeline_opts_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_timeline_serie_block,
    "e_timeline_serie",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_timeline_serie_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_title_block,
    "e_title",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_title_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_toolbox_block,
    "e_toolbox",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_toolbox_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_toolbox_feature_block,
    "e_toolbox_feature",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_toolbox_feature_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tooltip_block,
    "e_tooltip",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tooltip_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tooltip_choro_formatter_block,
    "e_tooltip_choro_formatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tooltip_choro_formatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tooltip_item_formatter_block,
    "e_tooltip_item_formatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tooltip_item_formatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tooltip_pie_formatter_block,
    "e_tooltip_pie_formatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tooltip_pie_formatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tooltip_pointer_formatter_block,
    "e_tooltip_pointer_formatter",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tooltip_pointer_formatter_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tree_block,
    "e_tree",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tree_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_tree__block,
    "e_tree_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_tree__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_treemap_block,
    "e_treemap",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_treemap_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_treemap__block,
    "e_treemap_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_treemap__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_unfocus_adjacency_block,
    "e_unfocus_adjacency",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_unfocus_adjacency_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_unfocus_adjacency_p_block,
    "e_unfocus_adjacency_p",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_unfocus_adjacency_p_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_utc_block,
    "e_utc",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_utc_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_visual_map_block,
    "e_visual_map",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_visual_map_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_visual_map__block,
    "e_visual_map_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_visual_map__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_visual_map_range_block,
    "e_visual_map_range",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_visual_map_range_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_x_axis_block,
    "e_x_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_x_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_x_axis__block,
    "e_x_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_x_axis__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_x_axis_3d_block,
    "e_x_axis_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_x_axis_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_y_axis_block,
    "e_y_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_y_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_y_axis__block,
    "e_y_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_y_axis__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_y_axis_3d_block,
    "e_y_axis_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_y_axis_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_z_axis_block,
    "e_z_axis",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_z_axis_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_z_axis__block,
    "e_z_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_z_axis__block", "echarts_layer_block")
  )

  blockr::register_block(
    e_z_axis_3d_block,
    "e_z_axis_3d",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_z_axis_3d_block", "echarts_layer_block")
  )

  blockr::register_block(
    e_zoom_block,
    "e_zoom",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_zoom_block", "echarts_layer_block")
  )

  blockr::register_block(
    echarts_from_json_block,
    "echarts_from_json",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("echarts_from_json_block", "echarts_layer_block")
  )

  blockr::register_block(
    echarts4r_proxy_block,
    "echarts4r_proxy",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("echarts4r_proxy_block", "echarts_layer_block")
  )

  blockr::register_block(
    echarts4rBox_block,
    "echarts4rBox",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("echarts4rBox_block", "echarts_layer_block")
  )

  blockr::register_block(
    echarts4rBoxOutput_block,
    "echarts4rBoxOutput",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("echarts4rBoxOutput_block", "echarts_layer_block")
  )


  blockr::register_block(
    echarts4rProxy_block,
    "echarts4rProxy",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("echarts4rProxy_block", "echarts_layer_block")
  )



}
