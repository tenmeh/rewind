#' @keywords internal
#' @aliases rewind-package
"_PACKAGE"

#' @importFrom R6 R6Class
#' @importFrom htmltools htmlDependency
NULL

# This line stops a NOTE. The NOTE says that the package imports utils but
# does not use it. rewind_dependency() uses utils to get the asset version.
utils::globalVariables(character(0))
