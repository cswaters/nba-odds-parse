# File functions

get_seas_from_file <- function(path='data', 
                               f_type ='.xlsx',
                               pat='\\d{4}-\\d{2}'){
  # extract season from filename
  dir(path = path,
      pattern = f_type) %>% 
    str_extract_all(pat) %>% 
    unlist() %>% 
    sort()
}

# get_seas_from_file()
get_files <- function(path='data', 
                      f_type ='.xlsx',
                      pat='\\d{4}-\\d{2}'){
  # get vector of file names
  dir(path = path,
      pattern = f_type,
      full.names = TRUE) %>% 
    sort()
}

# Date function
get_month <- function(col,chars){
  ifelse(chars == 4, 
         substr(col,1,2),
         substr(col, 1,1)
  )
}
get_day <- function(col,chars){
  ifelse(chars == 2, 
         substr(col,3,4),
         substr(col, 2,3)
  )
}
generate_date <- function(df){
  seas <- str_extract(df$season, '[:digit:]+') %>% 
    as.numeric()
  game_date <- df %>%
    transmute(
      n_chars = nchar(date),
      month = get_month(date, n_chars) %>% as.numeric(),
      day = get_day(date, nchar(month)) %>% as.numeric(),
      year = ifelse(as.numeric(month) >= 9, seas, seas + 1)
    ) %>% 
    transmute(date = paste0(year,'-',month,'-',day) %>% as.Date())
  
  df %>% 
    select(-date) %>% 
    bind_cols(game_date)
}

### Parse functions
fix_names <- function(.col){
  .col %>% 
    str_replace_all('AtlantaHawks','Atlanta') %>% 
    str_replace_all('BrooklynNets','Brooklyn') %>% 
    str_replace_all('CharlotteHornets','Charlotte') %>% 
    str_replace_all('ChicagoBulls','Chicago') %>% 
    str_replace_all('DetroitPistons','Detroit') %>% 
    str_replace_all('SacramentoKings','Sacramento') %>% 
    str_replace_all('Philadelphia76ers','Philadelphia') %>% 
    str_replace_all('Oklahoma City','OklahomaCity') %>% 
    str_replace_all('Seattle','OklahomaCity') %>% 
    str_replace_all('NewJersey','Brooklyn') %>% 
    str_replace_all('MilwaukeeBucks','Milwaukee') %>% 
    str_replace_all('MiamiHeat','Miami') %>% 
    str_replace_all('GoldenStateWarriors','GoldenState') %>% 
    str_replace_all('HoustonRockets','Houston') %>% 
    str_replace_all('LA Clippers','LAClippers') %>% 
    str_replace_all('MemphisGrizzlies','Memphis')
}
gen_gid <- function(df){
  is_data.frame(df)
  n_games <- nrow(df) / 2
  df$gid <- c(c(1:n_games), c(1:n_games)) %>% sort()
  df
}
convert_odds_numeric <- function(df) {
  is_data.frame(df)
  df %>%
    mutate(
      Open = ifelse(tolower(Open) == 'pk', 0, Open),
      Close = ifelse(tolower(Close) == 'pk', 0, Close),
      Open = ifelse(tolower(Open) == 'nl', NA, Open),
      Close = ifelse(tolower(Close) == 'nl', NA, Close),
      ML = ifelse(tolower(ML) == 'nl', NA, ML),
      `2H` = ifelse(tolower(`2H`) == 'pk', NA, `2H`),
      `2H` = ifelse(tolower(`2H`) == 'nl', NA, `2H`),
      Open = as.numeric(Open),
      Close = as.numeric(Close),
      ML = as.numeric(ML),
      `2H` = as.numeric(`2H`)
    )
}
fix_neutral_games <- function(df){
  is_even <- function(n) !(n %% 2)
  is_data.frame(df)
  neutral_gid <- df %>%
    filter(VH == 'N') %>%
    pull(gid) %>%
    unique()
  # even rotation are Home / odd rotation are away
  df[is_even(df$Rot), ]$VH <- 'H'
  df[!is_even(df$Rot), ]$VH <- 'V'
  df %>% 
    mutate(neutral = gid %in% neutral_gid %>% as.numeric())
}
odds_transform <- function(df, thresh = 60) {
  is_data.frame(df)
  v <- df %>% filter(VH == 'V')
  h <- df %>% filter(VH == 'H')
  h_odds <- h %>%
    select(
      h_Open = Open,
      h_Close = Close,
      h_ML = ML,
      h_2h = `2H`
    )
  v_odds <- v %>%
    transmute(
      odds_open_type = ifelse(Open > thresh, 'OU', 'Sprd'),
      odds_close_type = ifelse(Close > thresh, 'OU', 'Sprd'),
      odds_2h_type = ifelse(`2H` > thresh, 'OU', 'Sprd'),
      open_fav = ifelse(odds_open_type == 'OU', 'h', 'v'),
      close_fav = ifelse(odds_close_type == 'OU', 'h', 'v'),
      h2_fav = ifelse(odds_2h_type == 'OU', 'h', 'v'),
      v_Open = Open,
      v_Close = Close,
      v_ML = ML,
      v_2h = `2H`
    )
  
  odds_info <- h_odds %>%
    bind_cols(v_odds) %>%
    mutate(
      ou_open = ifelse(open_fav == 'h', v_Open, h_Open),
      ou_close = ifelse(close_fav == 'h', v_Close, h_Close),
      ou_h2 = ifelse(h2_fav == 'h', v_2h, h_2h)
    ) %>%
    mutate(
      sprd_open = ifelse(open_fav == 'h', h_Open, v_Open),
      sprd_close = ifelse(close_fav == 'h', h_Close, v_Close),
      sprd_h2 = ifelse(h2_fav == 'h', h_2h, v_2h)
    ) %>%
    select(
      h_ML,
      v_ML,
      sprd_open,
      open_fav,
      sprd_close,
      close_fav,
      ou_open,
      ou_close,
      sprd_h2,
      h2_fav,
      ou_h2
    )
  odds_info
}
col_select_rename <- function(df, keep_cols=NULL, prefix){
  df %>% 
    select(one_of(keep_cols)) %>% 
    set_names(
      paste0(prefix, names(.) %>% tolower())
    )
}
team_transform <- function(df, periods=c('1st','2nd','3rd','4th')) {
  is_data.frame(df)
  
  team_cols <- c(c('Rot', 'Team', 'Final'), periods)
  gm_cols <- c('gid', 'Date', 'neutral')
  
  v_info <- df %>% filter(VH == 'V') %>%
    select(-neutral) %>%
    col_select_rename(team_cols, 'v_')
  
  h_info <- df %>% filter(VH == 'H') %>%
    col_select_rename(team_cols, 'h_') %>%
    mutate(ot = ((h_1st + h_2nd + h_3rd + h_4th) < h_final) * 1)
  gm_info <- df %>% filter(VH == 'H') %>%
    col_select_rename(gm_cols, '')
  
  gm_info %>%
    bind_cols(h_info) %>%
    bind_cols(v_info)
}
repair_step <- function(df){
  gen_gid(df) %>% 
    convert_odds_numeric() %>% 
    fix_neutral_games() %>% 
    mutate(Team = fix_names(Team))
}
transform_step <- function(df){
  is_data.frame(df)
  df %>% 
    team_transform() %>% 
    bind_cols(odds_transform(df))
}
odds_pipeline <- function(files, seasons) {
  is_vector(files)
  is_vector(seasons)
  df_list <- map2(
    files,
    seasons,
    ~ read_excel(.x) %>%
      repair_step() %>%
      transform_step() %>%
      mutate(season = .y) %>%
      generate_date()
  )
  map(df_list, is_data.frame)
  df_list %>%
    bind_rows()
}