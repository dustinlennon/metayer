#' @include wrapped-factory.R wrapped-with-logger.R
NULL

#' wrapped pander functions
#' 
#' These functions are wrapped versions of those in the [pander](https://github.com/Rapporter/pander) package.
#' This allows us to inject logging metadata into the usual code flow.


#' @rdname wrapped_pander
#' @export
pander <- wrapped_factory("pander::pander", wrapped_with_logger)

#' @rdname wrapped_pander
#' @export
pander_return <- wrapped_factory("pander::pander_return", wrapped_with_logger)


md_text <- function(cr) {
  pander_out <- purrr::map(
    cr, 
    function(r) {
      pander::pander_return(r$result) %>%
        paste0(collapse = "\n")
    }
  ) %>%
    paste(collapse = "\n")

  log_debug(
    logger::formatter_json(
      "with_pander",
      pander_out
    )
  )

  pander_out
}

md_jupyter <- function(cr) {
  md_text(cr) %>%
    IRdisplay::display_markdown(data = .)
}

md_interactive <- function(cr) {
  md_text(cr) %>%
    cat()
}

md_knitr <- function(cr) {
  md_text(cr) %>%
    knitr::asis_output(pander_out)
}

with_pander <- function(code, .envir = parent.frame()) {
  .expr <- substitute(code)
  captured_result <- pander::evals(deparse(.expr), env = .envir)

  withr::defer(
    {
      for (cr in captured_result) {        
        for (m in cr$msg$errors) {
          with_logger(
            cnd_signal(error_cnd(message = m))
          )
        }

        for (m in cr$msg$warnings) {
          with_logger(
            cnd_signal(warning_cnd(message = m))
          )
        }

        for (m in cr$msg$messages) {
          with_logger(
            cnd_signal(message_cnd(message = m))
          )
        }
      }
    }
  )

  pubcontext(
    jupyter_code = md_jupyter(captured_result),
    knitr_code = md_knitr(captured_result),
    rstudio_code = NULL,
    interactive_code = md_interactive(captured_result),
    non_interactive_code = NULL,
    raise = FALSE,
    .envir = current_env()
  ) %>%
    invisible()
}
