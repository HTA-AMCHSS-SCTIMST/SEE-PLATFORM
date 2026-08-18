mod_chips_ui <- function(id, bin_count = 10L) {
  ns <- shiny::NS(id)
  cols <- lapply(seq_len(bin_count), function(i) {
    shiny::div(
      class = "chip-bin",
      shiny::actionButton(ns(paste0("add_", i)), "+", class = "chip-btn add"),
      shiny::uiOutput(ns(paste0("stack_", i))),
      shiny::actionButton(ns(paste0("rm_", i)), intToUtf8(8722), class = "chip-btn rm"),
      shiny::uiOutput(ns(paste0("lab_", i)))
    )
  })
  shiny::tagList(
    shiny::div(
      class = "chip-bounds",
      shiny::tags$strong("Step 1 — Plausible range"),
      shiny::tags$p(class = "muted",
        "Enter the lowest and highest values you judge to be plausible. Equal-width bins are arranged between your limits."
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("lo"), "Lowest plausible limit", value = 0, min = 0, max = 1, step = 0.01)),
        shiny::column(6, shiny::numericInput(ns("hi"), "Upper plausible limit", value = 1, min = 0, max = 1, step = 0.01))
      )
    ),
    shiny::uiOutput(ns("status")),
    shiny::div(class = "chip-grid", cols),
    shiny::plotOutput(ns("hist"), height = 200)
  )
}

mod_chips_server <- function(id, bin_count = 10L, total_chips = 20L, lo0 = 0, hi0 = 1) {
  shiny::moduleServer(id, function(input, output, session) {
    chips <- shiny::reactiveVal(rep(0L, bin_count))

    lapply(seq_len(bin_count), function(i) {
      shiny::observeEvent(input[[paste0("add_", i)]], {
        v <- chips()
        if (sum(v) < total_chips) {
          v[i] <- v[i] + 1L
          chips(v)
        }
      })
      shiny::observeEvent(input[[paste0("rm_", i)]], {
        v <- chips()
        if (v[i] > 0) {
          v[i] <- v[i] - 1L
          chips(v)
        }
      })
      local({
        ii <- i
        output[[paste0("stack_", ii)]] <- shiny::renderUI({
          v <- chips()[ii]
          shiny::div(class = "chip-stack", lapply(seq_len(v), function(.) htmltools::div(class = "chip-dot")))
        })
        output[[paste0("lab_", ii)]] <- shiny::renderUI({
          b <- bins()[[ii]]
          shiny::tags$small(class = "muted", sprintf("%.2f–%.2f", b$from, b$to))
        })
      })
    })

    bins <- shiny::reactive({
      lo <- as.numeric(input$lo %||% lo0)
      hi <- as.numeric(input$hi %||% hi0)
      if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) {
        lo <- lo0
        hi <- hi0
      }
      build_bins(bin_count, lo, hi, as.list(chips()))
    })

    output$status <- shiny::renderUI({
      placed <- sum(chips())
      rem <- total_chips - placed
      shiny::div(
        class = "chip-status",
        shiny::span(HTML(sprintf("Step 2 — Place chips: <strong>%s</strong> / %s", placed, total_chips))),
        shiny::span(class = if (rem == 0) "ok" else "muted", sprintf("Remaining: %s", rem))
      )
    })

    output$hist <- shiny::renderPlot({
      b <- bins()
      df <- data.frame(
        mid = vapply(b, function(x) (x$from + x$to) / 2, numeric(1)),
        chips = vapply(b, function(x) as.integer(x$chips), integer(1)),
        width = vapply(b, function(x) x$to - x$from, numeric(1))
      )
      ggplot2::ggplot(df, ggplot2::aes(x = mid, y = chips, width = width * 0.92)) +
        ggplot2::geom_col(fill = "#b8892d", colour = "#8a6a22") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x = "Value", y = "Chips") +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    list(
      value = shiny::reactive({
        list(
          bins = bins(),
          totalChips = total_chips,
          lowerBound = as.numeric(input$lo %||% lo0),
          upperBound = as.numeric(input$hi %||% hi0),
          placed = sum(chips())
        )
      })
    )
  })
}
