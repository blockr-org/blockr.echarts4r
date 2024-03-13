.onLoad <- function(libname, pkgname){

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
    e_append1_p__block,
    "e_append1_p_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_append1_p__block", "echarts_layer_block")
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
    e_area__block,
    "e_area_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_area__block", "echarts_layer_block")
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
    e_band__block,
    "e_band_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_band__block", "echarts_layer_block")
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
    e_bar__block,
    "e_bar_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_bar__block", "echarts_layer_block")
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
    e_boxplot__block,
    "e_boxplot_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_boxplot__block", "echarts_layer_block")
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
    e_charts__block,
    "e_charts_",
    "A block",
    input = "data.frame",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_charts__block")
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
    e_color_range__block,
    "e_color_range_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_color_range__block", "echarts_layer_block")
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
    e_density__block,
    "e_density_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_density__block", "echarts_layer_block")
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
    e_error_bar__block,
    "e_error_bar_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_error_bar__block", "echarts_layer_block")
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
    e_funnel__block,
    "e_funnel_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_funnel__block", "echarts_layer_block")
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
    e_geo_3d__block,
    "e_geo_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_geo_3d__block", "echarts_layer_block")
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
    e_histogram__block,
    "e_histogram_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_histogram__block", "echarts_layer_block")
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
    e_line_3d__block,
    "e_line_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_line_3d__block", "echarts_layer_block")
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
    e_lines_3d__block,
    "e_lines_3d_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_lines_3d__block", "echarts_layer_block")
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
    e_map__block,
    "e_map_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_map__block", "echarts_layer_block")
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
    e_mark_p__block,
    "e_mark_p_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_mark_p__block", "echarts_layer_block")
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
    e_pictorial__block,
    "e_pictorial_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_pictorial__block", "echarts_layer_block")
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
    e_radar__block,
    "e_radar_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_radar__block", "echarts_layer_block")
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
    e_river__block,
    "e_river_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_river__block", "echarts_layer_block")
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
    e_scatter__block,
    "e_scatter_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter__block", "echarts_layer_block")
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
    e_scatter_gl__block,
    "e_scatter_gl_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_scatter_gl__block", "echarts_layer_block")
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
    e_sunburst__block,
    "e_sunburst_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_sunburst__block", "echarts_layer_block")
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
    e_svg__block,
    "e_svg_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_svg__block", "echarts_layer_block")
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
    e_treemap__block,
    "e_treemap_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_treemap__block", "echarts_layer_block")
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
    e_x_axis__block,
    "e_x_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_x_axis__block", "echarts_layer_block")
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
    e_z_axis__block,
    "e_z_axis_",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_z_axis__block", "echarts_layer_block")
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
    e_aria_block,
    "e_aria",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_aria_block", "echarts_layer_block")
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
    e_calendar_block,
    "e_calendar",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_calendar_block", "echarts_layer_block")
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
    e_draft_block,
    "e_draft",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_draft_block", "echarts_layer_block")
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
    e_geo_block,
    "e_geo",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_geo_block", "echarts_layer_block")
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
    e_grid_block,
    "e_grid",
    "A block",
    input = "echarts4r",
    output = "echarts4r",
    package = pkgname,
    classes = c("e_grid_block", "echarts_layer_block")
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
}
