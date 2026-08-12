#' @keywords internal
#' @aliases rewind-package
"_PACKAGE"

#' @importFrom R6 R6Class
#' @importFrom htmltools htmlDependency
NULL

# Avoid a NOTE about utils being imported but seemingly unused; it is used by
# rewind_dependency() to stamp the asset version.
utils::globalVariables(character(0))
