# inst/fixtures/gold-fim.R. gcsf_design1 is PopED get_rse(FIM_gcsf, poped_db_gcsf);
# cas1–cas6 are frozen PFIM results. Do not edit either to silence tests.

.pfimLoadGold = function() {
  path = system.file( "fixtures", "gold-fim.R", package = "PFIM" )
  if ( !nzchar( path ) || !file.exists( path ) )
    path = file.path( "inst", "fixtures", "gold-fim.R" )
  e = new.env( parent = baseenv() )
  sys.source( path, envir = e )
  e$.pfimGold
}

.pfimGold = .pfimLoadGold()
