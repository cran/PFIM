# User-facing vs developer stops.
#
# .pfimStop()  - reaches Evaluation / Optimization / run() callers.
# .pfimWarn()  - same audience.
# .pfimInternalStop() - package invariants; tests may hit these via :::.
#
# Arguments are concatenated exactly as stop() / warning() do.

#' User-facing abort (no call stack, no internal function name).
#' @noRd
#' @keywords internal
.pfimStop = function( ... ) {
  stop( "PFIM: ", ..., call. = FALSE )
}

#' User-facing warning (no call stack, no internal function name).
#' @noRd
#' @keywords internal
.pfimWarn = function( ... ) {
  warning( "PFIM: ", ..., call. = FALSE )
}

#' Developer abort for broken internals (still no \code{.fn:} leak).
#' @noRd
#' @keywords internal
.pfimInternalStop = function( ... ) {
  stop( ..., call. = FALSE )
}
