test_that( "R/ sources follow the package style rules", {
  rDir = testthat::test_path( "../../R" )
  skip_if_not( dir.exists( rDir ), "package R/ sources not available" )
  files = list.files( rDir, pattern = "\\.[Rr]$", full.names = TRUE )
  files = files[ basename( files ) != "RcppExports.R" ]

  violations = map( files, function( f ) {
    tokens = utils::getParseData( parse( f, keep.source = TRUE, encoding = "UTF-8" ) )
    tokens = tokens[ tokens$terminal, ]
    tokens = tokens[ order( tokens$line1, tokens$col1 ), ]
    nextText = c( tokens$text[ -1L ], "" )
    rules = list(
      "<- / <<-"     = tokens$token %in% c( "LEFT_ASSIGN", "RIGHT_ASSIGN" ) & tokens$text != "=",
      "@ access"     = tokens$token == "'@'",
      "[[\"name\"]]" = tokens$token == "LBB" & startsWith( nextText, "\"" ),
      "~ lambda"     = tokens$token == "'~'" & nextText %in% c( ".x", ".y", "list" ),
      "base apply"   = tokens$token == "SYMBOL_FUNCTION_CALL" &
        tokens$text %in% c( "lapply", "sapply", "vapply", "mapply", "Map", "Reduce", "Filter" )
    )
    hits = imap( rules, \( hit, rule ) if ( any( hit ) )
      paste0( basename( f ), ":", tokens$line1[ hit ], " ", rule ) )
    unlist( hits, use.names = FALSE )
  } ) |> unlist()

  expect_equal( violations, NULL )
} )
