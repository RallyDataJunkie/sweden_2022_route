## ---- get_multi_stage_pace --------
get_multi_stage_pace = function(multi_stage_times, cars) {
  multi_stage_times %>%
    merge(stages[,c('stageId', 'distance',
                    'number', 'code')],
          by='stageId', all.x = TRUE) %>%
    mutate(elapsedDurationS = elapsedDurationMs / 1000,
           pace = elapsedDurationS / distance) %>%
    merge(cars[,c('entryId','drivername',
                  'code', 'groupname')],
          by='entryId',
          suffixes=c('','_driver')) %>%
    #filter(groupname=='WRC') %>%
    select(c('stageId', 'number', 'code_driver', 'entryId',
             'elapsedDurationS', 'pace', 'code'))  %>%
    arrange(number, elapsedDurationS)
  
}

## ---- get_stage_codes --------
get_stage_codes = function(stages){
  # Create a stage code mapping function
  stages_lookup_code = get_stages_lookup(stages, 'stageId', 'code')
  stage_code_map = function(stageId)
    stages_lookup_code[[as.character(stageId)]]
  
  # Map stage ID column names to stage codes
  stage_codes = unlist(purrr::map(stage_list,
                                  function (x) stage_code_map(x)))
  stage_codes 
}

## ---- get_splits_wide --------
get_splits_wide = function(splits){
  driver_splits = get_driver_splits(splits)
  
  split_cols =  get_split_cols(splits)
  splits_cols = c('entryId', 'splitPointId', 'elapsedDurationS')
  
  splits_wide = driver_splits %>% 
    group_by(entryId) %>%
    select(all_of(splits_cols)) %>%
    tidyr::spread(key = splitPointId,
                  value = elapsedDurationS) %>%
    select(all_of(c('entryId', split_cols))) %>%
    # If we don't cast, it's a
    # non-rankable rowwise df
    as.data.frame()
  
  splits_wide
}

## ---- get_split_duration --------
get_split_duration = function(df, split_cols,
                              retId=TRUE, id_col='entryId') {
  
  # [-1] drops the first column, [-ncol()] drops the last
  df_ = df[,split_cols][-1] - df[,split_cols][-ncol(df[,split_cols])]
  
  # The split time to the first split is simply the first split time
  df_[split_cols[1]] = df[split_cols[1]]
  
  if (retId) {
    # Add in the entryId column
    df_[[id_col]] = df[[id_col]]
    
    # Return the dataframe in a sensible column order
    df_ %>% select(all_of(c(id_col, all_of(split_cols))))
  } else {
    df_
  }
  
}

## ---- map_stage_codes --------
map_stage_codes = function(df, stage_list) {
  # Get stage codes lookup id->code
  stages_lookup_code = get_stages_lookup(stages, 'stageId', 'code')
  
  #https://stackoverflow.com/a/34299333/454773
  plyr::rename(df, replace = stages_lookup_code,
               warn_missing = FALSE)
}

## ---- map_driver_names --------
map_driver_names = function(df, cars, retain_code=TRUE){
  df = df %>%
    merge(cars[,c('entryId','code')],
          by='entryId')

  if (!retain_code)
    # Limit columns and set column order
     df = df %>% select(-'entryId')
  
    # Move last column to first
    df %>% select('code', everything())
}

## ---- map_split_codes --------
map_split_codes = function(df, splits_list) {
  # Get stage codes lookup id->code
  splits_lookup_code = get_stages_lookup(splits_locations,
                                         'splitPointId', 'splitname')
  
  #https://stackoverflow.com/a/34299333/454773
  plyr::rename(df, replace = splits_lookup_code,
               warn_missing = FALSE)
}

## ---- map controls ----
map_controlId = function(df, controls){
  df %>%
    merge(controls[,c('controlId','type', 'code', 'location')],
          by='controlId', suffixes=c('', '2'))  %>%
    rename(control = code2)
}

## ---- relabel_times_df --------
relabel_times_df = function(df, stage_list, cars) {
  df %>%  
    map_stage_codes(stage_list) %>%
    map_driver_names(cars)
}

## ---- relabel_times_df2 --------
relabel_times_df2 = function(df, s_list, cars, typ='stage') {
  if (typ=='split')
    df = df %>% map_split_codes(s_list)
  else
    df = df %>% map_stage_codes(s_list)
  
  df %>%
    map_driver_names(cars)
}

## ---- get_split_locations --------
get_split_locations = function(splits){
  get_split_label = function(x){
    paste0('split_', splits_locations[splits_locations$splitPointId==x,
                                      'number'])
  }
  
  splits_locations = splits$splitPoints
  splits_locations$splitname = sapply(splits_locations$splitPointId,
                                      get_split_label)
  splits_locations %>%
    arrange(number)
}
