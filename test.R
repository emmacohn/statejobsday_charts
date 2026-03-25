library(tidyverse)
library(DatawRappr)

# ── Data prep (mirrors nonfarm.qmd) ───────────────────────────────────────────

jobchange <- read_csv("/projects/ecohn/state_jobs_day/output/jobs_nonfarm.csv") |>
  filter(series_id != "NA") |>
  select(-series_id)

current_month <- tail(colnames(jobchange), 1)

jobcomp <- jobchange |>
  mutate(
    feb_2020     = .data[["2020-02-01"]],
    jan_2025     = .data[["2025-01-01"]],
    covid_change = ((.data[[current_month]] / feb_2020) - 1) * 100,
    trump_change = ((.data[[current_month]] / jan_2025) - 1) * 100,
    us_covid     = covid_change[geo_name == "United States"],
    us_trump     = trump_change[geo_name == "United States"]
  ) |>
  filter(geo_name != "United States") |>
  select(geo_name, covid_change, us_covid, trump_change, us_trump)

states <- sort(unique(jobcomp$geo_name))

# ── Function: create one chart per state, return id map ───────────────────────

create_state_charts <- function(states) {
  chart_ids <- tibble(state = character(), chart_id = character())

  for (state in states) {
    chart <- dw_create_chart(
      title = paste(state, "- Nonfarm job change"),
      type  = "d3-bars"        # grouped bar chart
    )
    chart_ids <- add_row(chart_ids, state = state, chart_id = chart$id)
    message("Created chart for ", state, " → ", chart$id)
  }

  write_csv(chart_ids, "dw_chart_ids.csv")
  message("Saved chart IDs to dw_chart_ids.csv")
  chart_ids
}

# ── Function: upload data to each chart ───────────────────────────────────────

populate_state_charts <- function(jobcomp, chart_ids) {
  for (i in seq_len(nrow(chart_ids))) {
    state    <- chart_ids$state[i]
    chart_id <- chart_ids$chart_id[i]

    # Format: one row per time period, columns = State / United States
    row <- filter(jobcomp, geo_name == state)
    chart_data <- tibble(
      Period          = c("Since COVID (Feb 2020)", "Since Trump (Jan 2025)"),
      State           = c(row$covid_change, row$trump_change),
      `United States` = c(row$us_covid, row$us_trump)
    )

    dw_data_to_chart(chart_data, chart_id = chart_id)

    dw_edit_chart(
      chart_id,
      title    = paste(state, "- % change in nonfarm employment"),
      intro    = paste("Most recent data:", current_month)
    )

    message("Populated chart for ", state)
  }
}

# ── Run ───────────────────────────────────────────────────────────────────────

# Step 1: create charts (only run once — saves IDs to dw_chart_ids.csv)
# chart_ids <- create_state_charts(states)

# Step 2: load saved IDs and populate (run each update cycle)
chart_ids <- read_csv("dw_chart_ids.csv")
populate_state_charts(jobcomp, chart_ids)
