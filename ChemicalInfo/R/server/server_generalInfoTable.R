
output$generalInfo <- render_gt(
  expr = gt(rv$generalInfoTable) %>%
    fmt_markdown(columns = "value") %>%
    cols_width(parameter ~ pct(25), value ~ pct(75)) %>%
    tab_options(column_labels.hidden = TRUE,
                table.align = "left",
                table.width = pct(100),
                table.font.size = "1.25") %>%
    tab_style(style = list(cell_borders(sides = "all", color = "#000000", style = "hidden", weight = px(1))),
              locations = list(cells_body())) %>%
    tab_header(title = md("**General Info**")) %>%
    opt_align_table_header(align = "left")
)
