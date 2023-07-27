library(tidyverse)
library(janitor)
library(kableExtra)
library(gt)
library(gtExtras)

gt_theme_mg <- function(data) {
  data %>%
    opt_row_striping() |>
    opt_table_lines(extent = "none") |>
    # opt_table_font(
    #   font = list(
    #     google_font(name = "Source Sans Pro")
    #   )
    # ) |>
    # opt_table_font(stack = "Source Sans Pro") |>
    tab_options(
      table.font.color = "black",
      table.font.names = "Source Sans Pro",
      data_row.padding = px(6),
      table.font.size = px(16), # ?12
      column_labels.font.size = px(16), # ?12
      table.align = "left",
      table_body.border.bottom.width = px(1.7),
      table.border.top.width = px(1.7),
      table_body.border.bottom.color = "#9A9EA1",
      table.border.top.color = "#9A9EA1",
      table_body.border.bottom.style = "solid",
      table.border.top.style = "solid",
      column_labels.border.bottom.color = "#9A9EA1",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1.3),
      column_labels.font.weight = "bold",
      column_labels.padding = px(3),
      heading.align = "left",
      footnotes.padding = px(0),
      footnotes.font.size = px(12),
      source_notes.padding = px(0),
    ) |>
    opt_horizontal_padding(scale = 2) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) |>
    opt_footnote_marks(marks = "letters") |>
    opt_footnote_spec(spec_ref = "^x", spec_ftr = "^x") |>
    opt_css(
      css = "
    #one .gt_footnote_marks {
    font-style: normal;
    font-weight: normal;
    font-size: 100%;
    vertical-align: 0px;
    }
  "
    )
}