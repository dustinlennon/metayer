r_config_active <- Sys.getenv("R_CONFIG_ACTIVE")
Sys.setenv(R_CONFIG_ACTIVE = "testing")

withr::defer(
  {
    if (isTRUE(nzchar(r_config_active))) {
      Sys.setenv(R_CONFIG_ACTIVE = r_config_active)
    } else {
      Sys.unsetenv("R_CONFIG_ACTIVE")
    }
  },
  teardown_env()
)
