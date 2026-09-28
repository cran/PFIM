# Constraint-grid FIM enumeration for the Fedorov-Wynn and multiplicative optimizers.
#' @include Optimization.R
NULL

#' Stratified subsample of constraint-grid cells (deterministic, equispaced per dose).
#'
#' When \code{constraints.maxTasks} caps evaluations, each dose block keeps an
#' equispaced subset so Fedorov-Wynn / multiplicative algorithms still see
#' representative sampling combinations rather than only the first rows.
#' @name .pfimConstraintTaskIndices
#' @noRd
#' @keywords internal
.pfimStratifiedPick = function( pool, nPick ) {
  n = length( pool )
  if ( nPick >= n ) return( pool )
  if ( nPick <= 0L ) return( integer( 0 ) )
  # round(seq(...)) can collide; unique keeps indices strictly increasing.
  pos = unique( as.integer( round( seq( 1, n, length.out = nPick ) ) ) )
  pool[ pos ]
}

.pfimConstraintTaskIndices = function( totalIterations, numberOfDoses, nCombinations ) {
  maxTasks = pfim_get_option( "constraints.maxTasks", NULL )
  if ( is.null( maxTasks ) || !is.finite( maxTasks ) || maxTasks >= totalIterations )
    return( seq_len( totalIterations ) )
  maxTasks = max( 1L, as.integer( maxTasks ) )
  # Spread remainder across the first dose blocks so total == maxTasks.
  base      = maxTasks %/% numberOfDoses
  remainder = maxTasks %% numberOfDoses
  map( seq_len( numberOfDoses ), function( doseIdx ) {
    pool  = ( ( doseIdx - 1L ) * nCombinations + 1L ):( doseIdx * nCombinations )
    nPick = min( length( pool ), base + as.integer( doseIdx <= remainder ) )
    if ( nPick > 0L ) .pfimStratifiedPick( pool, nPick ) else integer( 0 )
  } ) |>
    list_c() |>
    unique() |>
    sort()
}

#' Enumerate FIMs on the dose and sampling grid.
#'
#' Enables \code{eval.batch} so nested \code{run(Evaluation)} calls share caches.
#' Flat index \code{fimIndex} maps to \code{(dose, sampling combo)} via integer
#' division; results are reshaped into the lists expected by FW / multiplicative
#' optimizers. Each cell returns both a packed triangle (\code{listFimsAlgoFW})
#' and a dense matrix (\code{listFimsAlgoMult}) - do not swap those keys.
#' @name generateFimsFromConstraints
#' @keywords internal
method( generateFimsFromConstraints, Optimization ) = function( optimization ) {
  showProgress = isTRUE( projectProp( optimization, "optimizerParameters" )$showProcess ) ||
    isTRUE( pfim_get_option( "verbose", FALSE ) )
  .pfimFimCacheBegin()
  pfim_set_option( fim.cache.hits = 0L )
  oldBatch = pfim_get_option( "eval.batch", FALSE )
  on.exit( pfim_set_option( eval.batch = oldBatch ), add = TRUE )
  pfim_set_option( eval.batch = TRUE )
  evaluation  = .evaluationFromProject( optimization )
  .pfimSetCacheEvaluation( evaluation )
  on.exit( pfim_set_option( fim.cache.evaluation = NULL ), add = TRUE )
  baseModel        = rebuildEvalModel( evaluation, finiteDifference = TRUE )
  baseFim          = defineFim( evaluation )
  designs          = projectProp( optimization, "designs" )
  designNames      = map_chr( designs, \( x ) prop( x, "name" ) )
  dosesForFIMs     = map( designs, \( x ) generateDosesCombination( x ) ) |> set_names( designNames )
  samplingsForFIMs = map( designs, \( x ) generateSamplingTimesCombination( x ) ) |>
    set_names( designNames )
  allResults = imap( set_names( designs, designNames ), function( design, designName ) {
    arms            = prop( design, "arms" )
    dosesForDesign  = dosesForFIMs[[ designName ]]
    numberOfDoses   = dosesForDesign$numberOfDoses
    combinationGrid = expand.grid( map( samplingsForFIMs[[ designName ]], seq_along ) )
    nCombinations   = nrow( combinationGrid )
    totalIterations = numberOfDoses * nCombinations
    taskIndices     = .pfimConstraintTaskIndices( totalIterations, numberOfDoses, nCombinations )
    nTasks          = length( taskIndices )
    if ( nTasks < totalIterations && showProgress )
      message( sprintf(
        "FIM evaluation: evaluating %d / %d constraint-grid cells (constraints.maxTasks)",
        nTasks, totalIterations
      ) )
    perDesignResults = map( taskIndices, function( fimIndex ) {
      # Linear index -> (dose combo, sampling combo) on the full Cartesian product.
      iterDose = ( fimIndex - 1L ) %/% nCombinations + 1L
      iterComb = ( fimIndex - 1L ) %% nCombinations + 1L
      .evaluateFimConstraintsCell(
        fimIndex         = match( fimIndex, taskIndices ),
        iterDose         = iterDose,
        iterComb         = iterComb,
        totalIterations  = nTasks,
        showProgress     = showProgress,
        evaluation       = evaluation,
        design           = design,
        arms             = arms,
        dosesForDesign   = dosesForDesign,
        samplingsForFIMs = samplingsForFIMs,
        designName       = designName,
        combinationGrid  = combinationGrid,
        baseModel        = baseModel,
        baseFim          = baseFim
      )
    } )
    walk( perDesignResults, function( cell ) {
      if ( !is.null( cell$cachedEvaluation ) )
        .pfimFimCacheRegister( cell$cachedEvaluation )
    } )
    perDesignResults
  } )
  list(
    listArms                    = map( allResults, \( x ) map( x, "armResult" ) ),
    dimFim                      = pluck( allResults, 1L, 1L, "dimFim" ),
    listFimsAlgoFW              = map( allResults, \( x ) map( x, "fisherMatrixForAlgoFW" ) ),
    listFimsAlgoMult            = map( allResults, \( x ) map( x, "fisherMatrix" ) ),
    samplingsForFedorovWynnAlgo = map( allResults, \( x ) map( x, "samplingsForFW" ) )
  )
}
