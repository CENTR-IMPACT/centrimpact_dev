        fluidRow(
          style = "background-color: var(--bs-success);",
          phosphoricons::ph(
            name = "check-circle"),
          span("OK")
          ),
        fluidRow(
          style = "background-color: var(--bs-info);",
          phosphoricons::ph(
            name = "info"),
          span("Missing But Not Necessary")
        ),
        fluidRow(
          style = "background-color: var(--bs-warning);",
          phosphoricons::ph(
            name = "warning-circle"),
          span("Missing and Necessary")
        ),
        fluidRow(
          style = "background-color: var(--bs-danger);",
          phosphoricons::ph(
            name = "x-circle"),
          span("Error")
        )
      )
    )