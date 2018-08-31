compare_app_ui <- function() {
    ui <- dashboardPage(
        dashboardHeader(title = "Try to See it My Way"),

        dashboardSidebar(
            width = "300px",
            sidebarMenu(
                id = "sidebar",
                menuItem("Compare", tabName = "compare"),
                menuItem("Match Columns", tabName = "match"),
                menuItem("Difference", tabName = "diff")
            ),
            conditionalPanel(
                "input.sidebar == 'compare'",
                uiOutput('choose_vars'),
                # actionButton("add_choose_vars_to_matches", "Save Match",
                #              class = "btn btn-primary")
                ## Manually created button to apply `btn-primary` class
                tags$button(
                    id = "add_choose_vars_to_matches",
                    type = "button",
                    class = "btn btn-primary action-button",
                    `data-val` = shiny::restoreInput("add_choose_vars_to_matches", default = NULL),
                    "Save Match"
                )
            )
        ),

        dashboardBody(
            includeCSS(system.file("app_custom.css", package = "different")),
            useShinyjs(),
            shinytoastr::useToastr(),
            tabItems(
                tabItem(
                    tabName = "compare",
                    fluidRow(uiOutput("df_view_left")),
                    fluidRow(uiOutput("df_view_right"))
                ),
                tabItem(
                    tabName = "match",
                    fluidRow(
                        box(width = 8, uiOutput("df_cols_table"), class = "match-columns-box"),
                        box(width = 4, verbatimTextOutput("match_column_dictionary", TRUE))
                    )
                ),
                tabItem(
                    tabName = "diff",
                    fluidRow(
                        uiOutput("df_view_merged")
                    )
                )
            )
        )
    )
}

generate_table_of_selectors <- function(colnames_x, colnames_y, df_names) {
    tags$table(
        class = "match-columns-table",
        tags$thead(
            tags$tr(
                tags$th(class = "match-columns-left", df_names[1]),
                tags$th(class = "match-columns-status"),
                tags$th(class = "match-columns-right", df_names[2]),
                tags$th(class = "match-columns-action", "Use Name From")
            )
        ),
        purrr::imap(colnames_x, ~ tags$tr(
            tags$td(
                tags$code(.x),
                class = "match-columns-left"),
            tags$td(
                class = "match-columns-status",
                shinyjs::hidden(
                    tags$span(id = make_match_y_input(.y, "status_good_"),
                              icon("check", class = "text-success")),
                    tags$span(id = make_match_y_input(.y, "status_error_"),
                              icon("times", class = "text-danger"))
            )),
            tags$td(
                selectInput(make_match_y_input(.y), label = NULL,
                            choices = c("Drop" = "", colnames_y)),
                class = "match-columns-right"),
            tags$td(
                radioButtons(
                    make_match_y_input(.y, "match_keep_"), label = NULL,
                    choices = c("Left" = "left", "Right" = "right"),
                    inline = TRUE),
                class = "match-columns-action")
        ))
    )
}

make_match_y_input <- function(i, prefix = "match_y_") {
    sprintf("%s_%03d", prefix, i)
}

server <- function(df_left, df_right, df_names) {
    name_left <- df_names[1]
    name_right <- df_names[2]
    force(df_left); force(df_right)
    function(input, output, session) {
        # observe({
        #     input_names <- names(input)
        #     match_y_names <- input_names[grepl("match_y_", input_names)]
        #     if (!length(match_y_names)) return()
        #     purrr::set_names(match_y_names, names(df_left)) %>%
        #         purrr::map(~ input[[.]]) %>%
        #         purrr::keep(~ . != "") %>%
        #         str()
        # })

        # ---- Sidebar ----
        output$choose_vars <- renderUI({
            tagList(
                hr(),
                h5("Choose Columns", class = "sidebar-text"),
                selectInput("var_left", name_left, choices = c(
                    "Entire Data Frame" = "",
                    colnames(df_left)
                )),
                selectInput("var_right", name_right, choices = c(
                    "Entire Data Frame" = "",
                    colnames(df_right)
                ))
            )
        })

        # ---- Compare: View Data Tables ----
        make_data_table <- function(df) {
            DT::renderDataTable({
                DT::datatable(
                    df,
                    options = list(
                        dom = 'ftip',
                        pageLength = 6,
                        scrollX = TRUE,
                        fixedColumns = list(leftColumns = 2)
                    ),
                    extensions = c("FixedColumns"))
            })
        }
        output$df_left_dt <- make_data_table(df_left)
        output$df_right_dt <- make_data_table(df_right)

        # ---- Compare: Histogram ----
        auto_hist <- function(df, var) {
            if (is.numeric(df[[var]])) {
                ggplot2::ggplot(df, ggplot2::aes_string(x = var)) +
                    ggplot2::geom_histogram()
            } else {
                var_sym <- rlang::sym(var)
                df %>%
                    dplyr::group_by(!! var_sym) %>%
                    dplyr::count(sort = TRUE) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(!!var := forcats::fct_reorder(!!var_sym, n)) %>%
                    ggplot2::ggplot(ggplot2::aes_string(x = var, y = "n")) +
                    ggplot2::geom_col() +
                    ggplot2::coord_flip()

            }
        }
        output$df_left_plot <- renderPlot({
            req(input$var_left)
            auto_hist(df_left, input$var_left)
        })
        output$df_right_plot <- renderPlot({
            req(input$var_right)
            auto_hist(df_right, input$var_right)
        })

        # ---- Compare: Summary Table ----
        skim_output <- function(df, var) {
            skimr::skim_to_wide(df[[var]])
        }
        output$df_left_skim <- renderTable({
            req(input$var_left)
            skim_output(df_left, input$var_left)
        })
        output$df_right_skim <- renderTable({
            req(input$var_right)
            skim_output(df_right, input$var_right)
        })
        output$df_view_left <- renderUI({
            if (!isTruthy(input$var_left) || input$var_left == "") {
                cat(input$var_left)
                box(DT::dataTableOutput("df_left_dt", width = "100%", height = "45%"), width = 12)
            } else {
                tagList(
                    box(plotOutput("df_left_plot"), width = 6),
                    box(
                        h5(code(input$var_left)),
                        tableOutput("df_left_skim"),
                        width = 6)
                )
            }
        })
        output$df_view_right <- renderUI({
            if (!isTruthy(input$var_right) || input$var_right == "") {
                box(DT::dataTableOutput("df_right_dt", width = "100%", height = "45%"), width = 12)
            } else {
                tagList(
                    box(plotOutput("df_right_plot"), width = 6),
                    box(
                        h5(code(input$var_right)),
                        tableOutput("df_right_skim"),
                        width = 6)
                )
            }
        })

        # ---- Match Columns: Table ----
        output$df_cols_table <- renderUI({
            generate_table_of_selectors(names(df_left), names(df_right), df_names)
        })

        # Toggle Save Match button state: requires both inputs
        observe({
            if (is.null(input$var_left) || is.null(input$var_right)) return()
            cat("left:  ", input$var_left, "\nright:", input$var_right, "\n")
            shinyjs::toggleCssClass(
                "add_choose_vars_to_matches",
                "disabled",
                input$var_left == "" || input$var_right == ""
            )
        })

        observeEvent(input$add_choose_vars_to_matches, {
            if (!make_match_y_input(1) %in% names(input)) {
                showModal(modalDialog(
                    title = "Match Columns not Initialized",
                    "Please open the", tags$em("Match Columns"), "tab first",
                    "to initialize column matching."
                ))
                return()
            }
            s_var_left  <- input$var_left
            s_var_right <- input$var_right
            if (s_var_left == "" || s_var_right == "") {
                showNotification("Please select two variables.", type = "error")
                shinytoastr::toastr_error("Please select two variables", position = "top-right", preventDuplicates = TRUE)
                return()
            }
            cd_current <- column_dictionary() %>%
                mutate(key = paste(left, right, sep = "_"))
            these_key <- paste(s_var_left, s_var_right, sep = "_")
            if (these_key %in% cd_current$key) {
                shinytoastr::toastr_info("The match has already been saved", position = "top-right", preventDuplicates = TRUE)
                return()
            }
            cd_not_these <- filter(cd_current, key != these_key)
            dup_assignment_left  <- s_var_left  %in% cd_not_these$left
            dup_assignment_right <- s_var_right %in% cd_not_these$right

            which_match_y <- which(names(df_left) == s_var_left)
            match_y_input <- make_match_y_input(which_match_y[1])
            updateSelectInput(session, match_y_input, selected = s_var_right)
            if (dup_assignment_left) {
                shinytoastr::toastr_warning(glue::glue("Replaced previous match for {s_var_left}"),
                                            position = "top-right", preventDuplicates = TRUE)
            }
            if (dup_assignment_right) {
                shinytoastr::toastr_warning(glue::glue("Saved match but {s_var_right} now has multiple assignments"),
                                            position = "top-right", preventDuplicates = TRUE)
            }
            if (!dup_assignment_right && !dup_assignment_left) {
                shinytoastr::toastr_success("Match saved", position = "top-right", preventDuplicates = TRUE)
            }
        })

        # Check that right columns not selected twice
        update_status <- function(i, n) {
            good_id <- make_match_y_input(i, "status_good_")
            errr_id <- make_match_y_input(i, "status_error_")
            shinyjs::toggle(good_id, condition = n == 1)
            shinyjs::toggle(errr_id, condition = n > 1)
        }
        observe({
            if (!make_match_y_input(1) %in% names(input)) return()
            matched_vars <- data_frame(
                i = 1:length(names(df_left)),
                input_name = purrr::map_chr(1:length(names(df_left)), make_match_y_input)
            )
            matched_vars$value <- purrr::map_chr(matched_vars$input_name, ~ input[[.]])
            dup_check <- matched_vars %>%
                group_by(value) %>%
                count() %>%
                mutate(ok = n == 1)
            matched_vars %>%
                left_join(dup_check, by = "value") %>%
                mutate(n = ifelse(value == "", 0, n)) %>%
                select(i, n) %>%
                purrr::pwalk(update_status)
        })

        column_dictionary <- reactive({
            if (!make_match_y_input(1) %in% names(input)) return(data_frame())
            mv <- data_frame(
                i = 1:length(names(df_left)),
                input_name = purrr::map_chr(1:length(names(df_left)), make_match_y_input),
                name_left = names(df_left)
            )
            mv$name_right <- purrr::map_chr(mv$input_name, ~ input[[.]])
            mv$choose <- purrr::map_chr(mv$i, ~ input[[make_match_y_input(., "match_keep_")]])
            mv %>%
                filter(name_right != "") %>%
                mutate(merged = ifelse(choose == "left", name_left, name_right)) %>%
                select(left = name_left, right = name_right, merged)
        })

        output$match_column_dictionary <- renderPrint({
            req(column_dictionary())
            if (!nrow(column_dictionary())) return(invisible())
            datapasta::tribble_paste(column_dictionary(), output_context = datapasta::console_context())
        })

        # ---- Merged ----
        merged_df <- reactive({
            if (!make_match_y_input(1) %in% names(input)) return(data_frame())
            if (!is.null(column_dictionary()) && !nrow(column_dictionary())) return(data_frame())
            left <- df_left[, column_dictionary()[["left"]]]
            names(left) <- column_dictionary()[["merged"]]
            right <- df_right[, column_dictionary()[["right"]]]
            names(right) <- column_dictionary()[["merged"]]
            x <- list(left = left, right = right)
            str(x)
            x
        })
        diff_df <- reactive({
            req(merged_df())
            # browser()
            tidy_diff(merged_df()$x, merged_df()$y, group_vars = "id", align = TRUE)
        })
        output$df_diff_summary <- renderPrint({
            summary(diff_df())
        })
        output$df_view_merged <- renderUI({
            verbatimTextOutput("df_diff_summary")
        })
    }
}


#' An App to Compare Two Data Frames
#'
#' @param x `<tbl|df>` Left tibble or data frame
#' @param y `<tbl|df>` Right tibble or data frame
#' @export
diff_app <- function(x, y, df_names = NULL) {
    stop("Deprecated. Needs to be updated for new `diff_tbl` class")
    library(shiny)
    library(shinydashboard)
    library(shinyjs)
    df_names <- df_names %||% paste(sys.call())
    shinyApp(ui = compare_app_ui(), server = server(x, y, df_names[2:3]))
}
