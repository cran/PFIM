# FIM display labels (console / plotmath) and ggplot helpers.

#' @include Fim.R
#' @noRd
#' @keywords internal
NULL

# Greek labels (mu, beta, omega, gamma, sigma) for all FIM subclasses.

.greekConsole = c(
  mu    = "\u03bc_",
  beta  = "\u03b2_",
  omega = "\u03c9\u00B2_",
  gamma = "\u03b3\u00B2_",
  sigma = "\u03c3_"
)

.greekPlotmath = c(
  mu    = "mu",
  beta  = "beta",
  omega = "omega^2",
  gamma = "gamma^2",
  sigma = "sigma"
)

#' Plotmath facet labels for SE/RSE bar charts (device-independent Greek).
#' @noRd
#' @keywords internal
.pfimSeRseFacetLabel = function( metric, key ) {
  g = .greekPlotmath[[ key ]]
  if ( !.pfimIsNonEmptyScalar( g ) )
    .pfimInternalStop( "Unknown Greek key for facet label: ", key )
  # plotmath: literal metric text, then Greek symbol (parsed by label_parsed).
  paste0( "'", metric, "'~'  '~", g )
}

#' Plotmath subscript for a parameter suffix.
#' Always quoted so tokens like \code{gamma} stay literal text (not Greek).
#' @noRd
#' @keywords internal
.pfimPlotmathSubscript = function( suffix ) {
  paste0( "['", gsub( "'", "", as.character( suffix ), fixed = TRUE ), "']" )
}

#' Split residual-error suffix \code{inter_RespPK} / \code{slope_RespPD}.
#' @return \code{NULL} or \code{list(kind=, response=)}.
#' @noRd
#' @keywords internal
.pfimSigmaInterSlopeParts = function( suffix ) {
  s = as.character( suffix )
  if ( !grepl( "^(inter|slope)_", s, perl = TRUE ) ) return( NULL )
  list(
    kind     = sub( "^(inter|slope)_.*$", "\\1", s, perl = TRUE ),
    response = sub( "^(inter|slope)_", "", s, perl = TRUE )
  )
}

#' Split a console FIM label into its Greek key and parameter suffix.
#' @return \code{NULL} or \code{list(key=, suffix=)} (key is a \code{.greekConsole} name).
#' @noRd
#' @keywords internal
.pfimSplitGreekLabel = function( label ) {
  key = detect( names( .greekConsole ), \( k ) startsWith( label, .greekConsole[[ k ]] ) )
  if ( is.null( key ) ) return( NULL )
  list( key = key, suffix = substring( label, nchar( .greekConsole[[ key ]] ) + 1L ) )
}

#' Normalize internal/console parameter names to ASCII tokens for plotmath.
#' @noRd
#' @keywords internal
.pfimParamNameAscii = function( parameterName ) {
  # Map console Greek prefixes to ASCII tokens that plotmath can parse.
  name = as.character( parameterName )
  name = gsub( "\u03bc_",    "mu_",      name, fixed = TRUE )
  name = gsub( "\u03b2_",    "beta_",    name, fixed = TRUE )
  name = gsub( "\u03c9\u00b2_", "omega^2_", name, fixed = TRUE )
  name = gsub( "\u03b3\u00b2_", "gamma^2_", name, fixed = TRUE )
  name = gsub( "\u03c3\u00b2_", "sigma_", name, fixed = TRUE )
  name = gsub( "\u03c3_",       "sigma_", name, fixed = TRUE )
  name
}

#' Plotmath label for a FIM parameter name (mu, beta, omega^2, ...).
#'
#' Squared Greek uses \code{omega['Cl']^2}. Residual error matches the report
#' nesting \code{${\\sigma_{inter}}_{RespPD}$} as \code{sigma['inter']['RespPD']}
#' (device-safe plotmath, no Unicode).
#' @noRd
#' @keywords internal
.pfimParamPlotmathLabel = function( parameterName ) {
  name = .pfimParamNameAscii( parameterName )
  # Longer prefixes first so "omega^2_" is not matched as a bare "omega".
  prefixes = c( "omega^2_", "gamma^2_", "sigma^2_", "sigma_", "mu_", "beta_" )
  # Tokens before the subscript; squared forms add "^2" after the subscript.
  greek   = c( "omega",    "gamma",    "sigma",    "sigma",  "mu",  "beta" )
  squared = c( TRUE,       TRUE,       TRUE,       FALSE,    FALSE, FALSE )
  i       = detect_index( prefixes, \( x ) startsWith( name, x ) )
  if ( i == 0L ) return( name )
  suffix = substring( name, nchar( prefixes[[ i ]] ) + 1L )
  if ( greek[[ i ]] == "sigma" ) {
    parts = .pfimSigmaInterSlopeParts( suffix )
    if ( !is.null( parts ) ) {
      return( paste0(
        "sigma",
        .pfimPlotmathSubscript( parts$kind ),
        .pfimPlotmathSubscript( parts$response )
      ) )
    }
  }
  lab = paste0( greek[[ i ]], .pfimPlotmathSubscript( suffix ) )
  if ( squared[[ i ]] ) paste0( lab, "^2" ) else lab
}

#' Plotmath y-axis label for sensitivity plots: df/d parameter.
#' @noRd
#' @keywords internal
.pfimSensitivityYLab = function( parameterName ) {
  # plotmath fraction df / d(parameter) for sensitivity figure y-axis.
  paste0( "frac(df, d*", .pfimParamPlotmathLabel( parameterName ), ")" )
}

#' Parsed plotmath expression from a label string.
#' @noRd
#' @keywords internal
.pfimParsePlotmath = function( label ) {
  # First (only) expression from parse(); keep.source=FALSE for speed.
  parse( text = label, keep.source = FALSE )[[ 1L ]]
}

#' Caption / x-axis annotation for sensitivity plots with Greek parameter names.
#' @noRd
#' @keywords internal
.pfimPlotmathEscape = function( x )
  gsub( "'", "\\\\'", x, fixed = TRUE )

#' @noRd
#' @keywords internal
.pfimSensitivityCaption = function( designName, armName, outputName, parameterName ) {
  paste0(
    "paste('Design: ", .pfimPlotmathEscape( gsub( "_", " ", designName, fixed = TRUE ) ),
    "   Arm: ", .pfimPlotmathEscape( armName ),
    "   Output: ", .pfimPlotmathEscape( outputName ),
    "   Parameter: ', ", .pfimParamPlotmathLabel( parameterName ), ")"
  )
}

#' Full x-axis plotmath label: time unit atop sensitivity caption.
#' @noRd
#' @keywords internal
.pfimSensitivityXLab = function( unitXAxis, designName, armName, outputName, parameterName ) {
  paste0(
    "atop('Time (", .pfimPlotmathEscape( unitXAxis ), ")', ",
    .pfimSensitivityCaption( designName, armName, outputName, parameterName ),
    ")"
  )
}

#' Require complete SE/RSE rownames (shared by pop / Bayesian plot + report paths).
#' @noRd
#' @keywords internal
.pfimSeRownames = function( seDF, context ) {
  rn = rownames( seDF )
  if ( !length( rn ) || length( rn ) != nrow( seDF ) )
    .pfimInternalStop( sprintf( "%s: SEAndRSE rownames missing or incomplete.", context ) )
  rn
}

.greekLatex = c(
  mu    = "$\\mu_{",
  beta  = "$\\beta_{",
  omega = "$\\omega^2_{",
  gamma = "$\\gamma^2_{",
  sigma = "$\\sigma_{"
)

#' Strip a Greek name prefix from FIM column labels.
#' @noRd
#' @keywords internal
.stripGreekPrefix = function( names, prefix )
  sub( paste0( "^", prefix ), "", names )

#' Residual-error parameter labels (\code{inter_}/\code{slope_} blocks).
#' @noRd
#' @keywords internal
.sigmaNames = function( modelError, greekSigma ) {
  # Filter must match .pfimSigmaIsEstimable / residual_error_derivatives.
  modelError |>
    map( function( err ) {
      out = prop( err, "output" )
      c(
        if ( .pfimSigmaIsEstimable( prop( err, "sigmaInter" ), prop( err, "sigmaInterFixed" ) ) )
          paste0( greekSigma, "inter_", out ),
        if ( .pfimSigmaIsEstimable( prop( err, "sigmaSlope" ), prop( err, "sigmaSlopeFixed" ) ) )
          paste0( greekSigma, "slope_", out )
      )
    } ) |> unlist( use.names = FALSE )
}

#' Estimable residual-error values in \code{inter}/\code{slope} column order.
#' @noRd
#' @keywords internal
.sigmaValues = function( modelError ) {
  # Same order and filter as .sigmaNames so values align with column labels.
  modelError |>
    map( function( err ) {
      c(
        if ( .pfimSigmaIsEstimable( prop( err, "sigmaInter" ), prop( err, "sigmaInterFixed" ) ) )
          prop( err, "sigmaInter" ),
        if ( .pfimSigmaIsEstimable( prop( err, "sigmaSlope" ), prop( err, "sigmaSlopeFixed" ) ) )
          prop( err, "sigmaSlope" )
      )
    } ) |> unlist( use.names = FALSE )
}


#' Shared ggplot2 theme for PFIM figures (reports + vignettes).
#' Sans family, sizes tuned for HTML at >= 150 dpi (no fuzzy small type).
#' @noRd
#' @keywords internal
.pfimBaseTheme = function( baseSize = 13 ) {
  theme_bw( base_size = baseSize, base_family = "sans" ) +
    theme(
      legend.position  = "none",
      plot.title       = element_text( size = baseSize + 1L, face = "bold", hjust = 0.5 ),
      axis.title.x     = element_text( size = baseSize, face = "plain", margin = margin( t = 6 ) ),
      axis.title.y     = element_text( size = baseSize, face = "plain", margin = margin( r = 6 ) ),
      axis.text.x      = element_text( size = baseSize - 1L, angle = 90, vjust = 0.5, hjust = 1, color = "grey15" ),
      axis.text.y      = element_text( size = baseSize - 1L, color = "grey15" ),
      strip.text.x     = element_text( size = baseSize, face = "bold", color = "grey10" ),
      strip.background = element_rect( fill = "grey90", colour = "grey80" ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line( colour = "grey88", linewidth = 0.35 ),
      plot.margin      = margin( 8, 12, 8, 8 )
    )
}

#' Red secondary axis for design sampling times (response / SI plots).
#' @noRd
#' @keywords internal
.pfimSamplingAxisTheme = function( baseSize = 13 ) {
  .pfimBaseTheme( baseSize ) +
    theme(
      axis.title.x.top = element_text(
        color = "#C62828", size = baseSize, face = "bold", vjust = 1.8
      ),
      axis.text.x.top  = element_text(
        angle = 90, hjust = 0, vjust = 0.5, color = "#C62828",
        size = baseSize - 1L
      ),
      axis.text.x = element_text(
        size = baseSize - 1L, angle = 0, vjust = 0.5, color = "grey15"
      )
    )
}

#' SE/RSE bar-chart theme.
#' @noRd
#' @keywords internal
.pfimSeRseTheme = function() {
  .pfimBaseTheme( 13 ) +
    theme(
      axis.text.x = element_text(
        size = 11, angle = 45, hjust = 1, vjust = 1, color = "grey15"
      ),
      strip.text.x = element_text( size = 12, face = "bold" ),
      panel.spacing.x = unit( 0.9, "lines" ),
      panel.spacing.y = unit( 0.7, "lines" ),
      plot.margin = margin( 8, 14, 16, 10 )
    )
}

#' Common column width for PFIM bar charts (fraction of category slot).
#' @noRd
#' @keywords internal
.pfimBarWidth = 0.7

#' SE/RSE bar-chart rows grouped by Greek prefix, in \code{groups} order.
#'
#' Keeps the full console names (\code{mu_}, \code{omega^2_}, ...) so the
#' x-axis matches the SE/RSE report table; one facet label per row.
#' @param seDF SE/RSE table with \code{SE} and \code{RSE} columns.
#' @param rn Console row names of \code{seDF}.
#' @param metric \code{"SE"} or \code{"RSE"}.
#' @param groups Keys of \code{.greekConsole}, e.g. \code{c("mu", "beta")}.
#' @return \code{data.frame} with \code{Parameter}, \code{metric} and \code{cat}.
#' @noRd
#' @keywords internal
.fimSeRseGroupedFrame = function( seDF, rn, metric, groups ) {
  rows   = map( groups, \( group ) which( startsWith( rn, .greekConsole[ group ] ) ) )
  idx    = unlist( rows )
  values = if ( metric == "SE" ) seDF$SE else seDF$RSE
  cats   = map2( groups, rows, \( group, r ) rep( .pfimSeRseFacetLabel( metric, group ), length( r ) ) )
  df     = data.frame(
    Parameter        = rn[ idx ],
    y                = values[ idx ],
    cat              = as.character( unlist( cats ) ),
    stringsAsFactors = FALSE
  )
  names( df )[ 2L ] = metric
  df
}

#' Faceted SE/RSE bar chart shared by FIM plot methods.
#'
#' Only observed parameters are drawn (no pad ticks). Panel widths follow the
#' number of categories via \code{space = "free_x"} so bar widths stay aligned.
#' @noRd
#' @keywords internal
.fimSeRseBarPlot = function( df, metric, facetLevels ) {
  # Factor only for plotting order; restore character cols on p$data for tests.
  dfPlot = df
  dfPlot$cat = factor( as.character( dfPlot$cat ), levels = facetLevels )
  dfPlot$Parameter = factor(
    as.character( dfPlot$Parameter ),
    levels = unique( as.character( dfPlot$Parameter ) )
  )
  # Console FIM names stay in data; axis uses plotmath (PDF/Windows-safe Greek).
  p = ggplot( dfPlot, aes( x = .data$Parameter, y = .data[[ metric ]] ) ) +
    geom_col(
      width = .pfimBarWidth, fill = "grey35", show.legend = FALSE
    ) +
    ggplot2::facet_grid(
      cols     = ggplot2::vars( cat ),
      scales   = "free_x",
      space    = "free_x",
      labeller = ggplot2::label_parsed
    ) +
    scale_x_discrete(
      labels = \( br ) parse( text = map_chr( br, .pfimParamPlotmathLabel ), keep.source = FALSE )
    ) +
    labs( x = "Parameter", y = metric ) +
    scale_y_continuous( expand = expansion( mult = c( 0, 0.08 ) ) ) +
    .pfimSeRseTheme()
  p$data = df
  p
}

#' Print nested ggplot lists so knitr records one figure per plot.
#'
#' HTML reports store response / SI plots as design -> arm -> outcome (-> parameter)
#' lists. A bare \code{print(list)} does not draw grobs; this walks the tree.
#' @param x A ggplot, or a (possibly nested) list of ggplots.
#' @noRd
#' @keywords internal
.pfimPrintReportPlots = function( x ) {
  if ( inherits( x, "ggplot" ) ) {
    print( x )
  } else if ( is.list( x ) && !is.data.frame( x ) && length( x ) ) {
    walk( x, .pfimPrintReportPlots )
  }
  invisible( NULL )
}

#' @noRd
#' @keywords internal
.gradientMatrix = function( arm, cols, model = NULL ) {
  # Covariate/occasion models store gradients on the model, not the arm.
  mat = if ( !is.null( model ) && usesCovariateOccasionStructure( model ) ) {
    getArmEvaluationGradientsMatrix( model, arm )
  } else {
    raw = prop( arm, "evaluationGradients" )
    # May be one data.frame or a list of per-output frames to stack.
    df  = if ( is.data.frame( raw ) ) raw else do.call( rbind, raw )
    as.matrix( df )
  }
  mat[ , cols, drop = FALSE ]
}

#' Variance-parameter FIM block: \eqn{\frac12 \mathrm{Tr}(V^{-1} \mathrm{d}V_i V^{-1} \mathrm{d}V_j)}.
#' @noRd
#' @keywords internal
.computeMFVar_R = function( V_inv, dV_list ) {
  n = length( dV_list )
  if ( !n ) return( matrix( numeric( 0 ), 0L, 0L ) )
  # Precompute V^{-1} dV_i V^{-1}; Frobenius product with dV_j yields the (i,j) entry.
  T_mats = map( dV_list, \( x ) V_inv %*% x %*% V_inv )
  outer(
    seq_len( n ), seq_len( n ),
    Vectorize( function( i, j ) 0.5 * sum( T_mats[[ i ]] * dV_list[[ j ]] ) )
  )
}

#' @rdname dot-computeMFVar_R
#' @noRd
#' @keywords internal
.computeMFVar = function( V_inv, dV_list ) {
  n = length( dV_list )
  if ( !n ) return( matrix( numeric( 0 ), 0L, 0L ) )
  # C++ kernel needs dense matrices; assembly may still hand sparse Diagonal blocks.
  computeMFVar_Rcpp( as.matrix( V_inv ), map( dV_list, as.matrix ) )
}
