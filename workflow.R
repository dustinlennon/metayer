#' Initialize workflow
#' 
#' This function is protected from rm.all.
#' @param envir an environment in which to bind a self-reference
workflow <- function(envir = parent.frame()) {
  tryCatch(
    {
      cfg <- yaml::read_yaml(
        here::here("workflow.yml")
      )

      setwd(
        cfg$base_dir
      )

      .libPaths(
        here::here("library")
      )

      devtools::load_all()

      for (src in cfg$workflow_srcs) {
        source(
          here::here(src),
          local = envir
        )
      }

      rlang::env_bind(
        envir,
        workflow = sys.function()
      )

      `R.utils`::printf(
        ">>> initialized workflow\n",
        file = stderr()
      )

    },
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      `R.utils`::printf("%s\n", cnd)
    }
  )
}
