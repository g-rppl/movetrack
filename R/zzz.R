.onAttach <- function(libname, pkgname) {
  check_cmdstan_toolchain(fix = TRUE)
}
