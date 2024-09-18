#' Internal.  Temporarily monkey patch a namespaced method
#' 
#' @inheritParams with_monkey_patch
#' @param expr an expression
.with_monkey_patch <- function(
    fqn,
    wrapper,
    expr,
    .envir = parent.frame()) {

  z <- stringr::str_split_1(fqn, pattern = "::")
  pkg_name <- z[[1]]
  method_name <- z[[2]]

  ns <- getNamespace(pkg_name)
  unpatched_env <- storage_env("metayer", "unpatched")
  method <- unpatched_env[[fqn]] %||% getExportedValue(ns, method_name)

  tryCatch(
    {
      unlockBinding(method_name, ns)
      ns[[method_name]] <- wrapped_factory(fqn, wrapper, func = method)
      lockBinding(method_name, ns)
      eval(expr, .envir)
    },
    finally = {
      unlockBinding(method_name, ns)
      ns[[method_name]] <- method
      lockBinding(method_name, ns)
    }
  )
}

#' Internal.  Temporarily monkey patch a namespaced method
#' 
#' @param fqn the fully qualified name, e.g. "rmarkdown::html_document"
#' @param wrapper a wrapper with signature function(cmd, args, func)
#' @param code user code
#' @param .envir the environment in which to evaluate expr
#' @export
with_monkey_patch <- function(
    fqn,
    wrapper,
    code,
    .envir = parent.frame()) {

  .with_monkey_patch(fqn, wrapper, substitute(code), .envir)
}
