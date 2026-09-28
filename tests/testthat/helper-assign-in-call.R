# Named first argument on expect_* is not assignment (testthat rejects it).
# Assignment inside a brace block is fine. This helper flags only the bad form
# where the first formal looks like a name=... argument.

.pfimExpectNamedAssignRe = paste0(
  "expect_(warning|error|message|condition|no_warning|silent)",
  "\\s*\\(\\s*[A-Za-z.][A-Za-z0-9.]*\\s*="
)

.pfimTestFilesWithNamedAssign = function( files ) {
  # Skip this helper (its docs mention the banned pattern).
  files = files[ !grepl( "helper-assign-in-call[.]R$", files ) ]
  hits = vapply( files, function( f ) {
    txt = paste( readLines( f, warn = FALSE ), collapse = "\n" )
    grepl( .pfimExpectNamedAssignRe, txt, perl = TRUE )
  }, logical( 1L ) )
  files[ hits ]
}
