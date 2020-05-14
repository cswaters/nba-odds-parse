source('funcs.R')
source('libs.R')

path <- 'data/'
output_file <- 'nba_odds_complete.csv'
plan <- drake_plan(df = target(odds_pipeline(get_files(),
                                             get_seas_from_file())),
                   output = readr::write_csv(df, paste0(!!path, !!output_file)))
make(plan)