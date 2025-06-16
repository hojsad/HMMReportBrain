##
load("fo_lt_intv_sr_tidy.RData")
load("summary_data.RData")
load("glasser_states_df_dif_tidy.RData")
load("glasser_states_df_tidy.RData")
load("psd_state_df_tidy.RData")
load("melt_psd.RData")
#load("indices.RData")
source("libraries.R")
source("functions.R")
# Define the user interface# Load the shiny library

library(shiny)
library(shinyBS)
library(bslib)

ui <- page_sidebar(
  title = "Brain States and Dynamic Connectivities",
  sidebar = sidebar(
    title = "Settings",
    selectInput(
      "State",
      "Select state",
      c(
        "State 1",
        "State 2",
        "State 3",
        "State 4",
        "State 5",
        "State 6"
      ),
      selected = "State 1",
      multiple = TRUE
    ),
    selectInput(
      "Group",
      "Select group",
      c(
        "LBD",
        "PD",
        "NC"#,
        #"CC",
        #"NINANC"
      ),
      selected = c("LBD"),
      multiple = TRUE
    ),
    selectInput(
      "Comparison",
      "Select comparison",
      c(
        "Open v.s. Close",
        "LBD v.s. NC",
        #"LBD v.s. NC(CamCan)",
        #"LBD v.s. NC(NINA)",
        "PD v.s. NC",
        #"PD v.s. NC(CamCan)",
        #"PD v.s. NC(NINA)",
        "LBD v.s. PD"
      ),
      selected = "LBD v.s. NC",
      multiple = TRUE
    ),
    selectInput(
      "Variable",
      "Select variable",
      c(
        "Fractional Occupancy",
        "Mean Life Time (s)",
        "Mean Interval (s)",
        "Switching Rate (Hz)"
      ),
      selected = "Fractional Occupancy",
      multiple = TRUE
    ),
    sliderInput(
      "freq",
      "Frequency Bands",
      min = 1,
      max = 45,
      value = c(1, 40)
    ),
  ),
  navset_card_tab(
    nav_panel("States", plotOutput("plot1", width = "100%")),
    nav_panel("Groups", plotOutput("plot2", width = "100%")),
    nav_panel("Groups Table", dataTableOutput("table1")),
    nav_panel("Comparisons", plotOutput("plot3", width = "100%")),
    nav_panel("Comparisons Table", dataTableOutput("table2")),
    nav_panel("Summary", plotOutput("plot4", width = "100%")),
    nav_panel("PSD", plotOutput("plot5", width = "100%")),
    nav_panel("Slides", 
              HTML('<iframe src="BrainStates.html" width="100%" height="600px" style="border:none;"></iframe>')
    )
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    glasser_states_df_tidy |>
      filter(Group %in% input$Group, State %in% input$State) |>
      power_plot()
  })
  output$plot2 <- renderPlot({
    glasser_states_df_tidy |>
      filter(Group %in% input$Group, State %in% input$State) |>
      power_plot2()
  })
  output$plot3 <- renderPlot({
    glasser_states_df_dif_tidy |>
      filter(Group %in% input$Comparison, State %in% input$State) |>
      power_diff_plot()
  })
  output$table2 <- renderDataTable({
    glasser_states_df_dif_tidy |>
      filter(Group %in% input$Comparison, State %in% input$State) |>
      select("Group", "ROI", "hemi", "State", "Difference") |>
      mutate(
        Difference = if_else(
          Difference %in% sort(unique(Difference), decreasing = TRUE),
          Difference,
          NA_real_
        )
      ) |>
      filter(Difference != "NA") |>
      distinct()
  })
  output$table1 <- renderDataTable({
    glasser_states_df_tidy |>
      filter(Group %in% input$Group, State %in% input$State) |>
      select("Group", "ROI", "hemi", "State", "Power") |>
      mutate(
        Power = if_else(
          Power %in% sort(unique(Power), decreasing = TRUE),
          Power,
          NA_real_
        )
      ) |>
      filter(Power != "NA") |>
      distinct()
  })
  output$plot4 <- renderPlot({
    summary_data |>
      filter(
        Group %in% input$Group,
        State %in% input$State,
        Variable %in% input$Variable,
        comparison %in% input$Comparison
      ) |>
      sum_plot()
  })
  output$plot5 <- renderPlot({
    psd_state_df_tidy |>
      filter(
        Group %in% input$Group,
        State %in% input$State,
        between(fr, input$freq[1], input$freq[2])
      ) |>
      psd_plot()
  })
  output$plot6 <- renderPlot({
    psd <- melt_psd |>
      filter(State %in% input$State) |>
      filter(fr %in% input$freq[1]:input$freq[2])
    id <- fo_lt_intv_sr_tidy |>
      filter(Variable == "Fractional Occupancy", Group %in% input$Group) |>
      select(
        "ID",
        "Group"
      )
    id_psd <- id |> left_join(psd)
    glasser |> left_join(id_psd) |> power_plot()
  })
}
shinyApp(ui, server)
