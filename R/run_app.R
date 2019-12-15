#' @title Shiny app that runs the Gryffindor Robo Advisor.
#' @description With the Gryffindor Robo Advisor potential invetsors have the
#' unique possibility to find an in vestment possibility that is hand-selected
#' based on the investors needs and preferences. Beside the usual stuff required
#' to set up a portfolio (e.g. investment goals, capital available, ...) the
#' Gryffindor Robo Advisor applies insights from finance academia, to help the
#' investor asses her/his risk preference in a stable way.
#' @export
#' @examples
#' runDemo()
runDemo <- function() {
    appDir <- system.file("shiny", "Gryffindor", package = "gryffindorrobo")
    if (appDir == "") {
        stop(
            "Could not find example directory.
            Try re-installing gryffindorrobo.",
            call. = FALSE
        )
    }

    shiny::runApp(appDir, display.mode = "normal")

}
