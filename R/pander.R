#' @include wrapped-factory.R wrapped-with-logger.R
NULL

#' wrapped pander functions
#' 
#' These functions are wrapped versions of their analogues in the [pander](https://github.com/Rapporter/pander) package.
#' They imbue the original implementations with logger functionality.

#' @rdname wrapped_pander
#' @inherit pander::pander
#' @export
pander <- wrapped_factory("pander::pander", wrapped_with_logger)

#' @rdname wrapped_pander
#' @inherit pander::pander_return
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
  data <- md_text(cr)
  IRdisplay::display_markdown(data = data)
  invisible(NULL)
}

md_interactive <- function(cr) {
  md_text(cr) %>%
    cat()
  invisible(NULL)
}

md_knitr <- function(cr) {
  pander_out <- withr::with_options(
    list(
      knitr.in.progress = FALSE
    ),
    md_text(cr)
  )

  knitr::asis_output(pander_out)
}

#' Contextualized pander
#' 
#' @param code client code 
#' @param .envir environment in which to evalutes client code
#' @export
with_pander <- function(code, .envir = parent.frame()) {
  .expr <- substitute(code)
  captured_result <- pander::evals(deparse(.expr), env = .envir, cache = FALSE)

  with_logger(
    {
      for (cr in captured_result) {        
        for (m in cr$msg$errors) {
          cnd_signal(error_cnd(message = m))
        }

        for (m in cr$msg$warnings) {
          cnd_signal(warning_cnd(message = m))
        }

        for (m in cr$msg$messages) {
          cnd_signal(message_cnd(message = m))
        }
      }
    },
    .local_envir = .envir
  )

  result <- pubcontext(
    jupyter_code = md_jupyter(captured_result),
    knitr_code = md_knitr(captured_result),
    rstudio_code = NULL,
    interactive_code = md_interactive(captured_result),
    non_interactive_code = NULL,
    raise = FALSE,
    .envir = current_env(),
    .local_envir = .envir
  )
  
  if (is_null(result)) invisible(NULL) else result
}
