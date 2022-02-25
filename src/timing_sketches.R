get_split_label = function(x){
  paste0('split_', splits_locations[splits_locations$splitPointId==x,
                                    'number'])
}

get_split_cols = function(splits){
  split_cols =  as.character(arrange(splits$splitPoints,
                                     distance)$splitPointId)
  split_cols
}

get_splits_wide = function(splits, entries=NA){
  driver_splits = get_driver_splits(splits)
  
  # Filter on entry IDs
  if (!is.na(entries))
    driver_splits = driver_splits %>% filter(entryId %in% entries)
  
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

widen_splits_stage_times = function(splits_wide, stage_times,
                                    id_col='entryId'){
  
  results_cols = c('elapsedDurationMs', id_col,  'diffFirstMs', 'position')
  
  splits_wide = splits_wide %>%
    merge(stage_times[,results_cols],
          by = 'entryId') %>%
    mutate(split_N = elapsedDurationMs/1000)
  
  splits_wide
}

map_split_codes = function(df, splits_list) {
  # Get stage codes lookup id->code
  splits_lookup_code = get_stages_lookup(splits_locations,
                                         'splitPointId', 'splitname')
  
  #https://stackoverflow.com/a/34299333/454773
  plyr::rename(df, replace = splits_lookup_code,
               warn_missing = FALSE)
}


get_split_duration = function(df, split_cols,
                              retId=TRUE, id_col='entryId') {
  
  # Drop names if they are set
  split_cols = as.character(split_cols)
  
  # [-1] drops the first column, [-ncol()] drops the last
  df_ = df[,split_cols][-1] - df[,split_cols][-ncol(df[,split_cols])]
  
  # The split time to the first split is simply the first split time
  df_[split_cols[1]] = df[split_cols[1]]
  
  if (retId) {
    # Add in the entryId column
    df_[[id_col]] = df[[id_col]]
    
    # Return the dataframe in a sensible column order
    df_ %>% select(c(all_of(id_col), all_of(split_cols)))
  } else {
    df_
  }
  
}

get_split_rank = function(df, split_cols){
  # We need to drop any list names
  split_names = as.character(split_names)
  
  df %>% mutate(across( all_of(split_cols), dense_rank ))
}

xnormalize = function(x, max_x=120){
  # Normalise to the full range of values about 0
  # O will map to 0.5 in the normalised range
  
  # Try to set a maxval for the normalizer
  # This is okay for color limits but NOT as a true normalizer
  
  #x = normalize(x) * max_x
  # Set the max absolute value; assume seconds so 2 mins?
  x = ifelse(abs(x)>max_x, sign(x)*max_x, x)
  #normalize(x)
  
  max_Limit =  max(abs(x))
  x = c(x, -max_Limit, max_Limit)
  normalize(x)[1:(length(x)-2)]
}

xnormalize_orig = function(x){
  # Normalise to the full range of values about 0
  # O will map to 0.5 in the normalised range
  x = c(x, -max(abs(x)), max(abs(x)))
  normalize(x)[1:(length(x)-2)]
}

color_tile2 <- function (...) {
  formatter("span", style = function(x) {
    # Handle NA
    x = ifelse(is.na(x), -1e-9, x)
    max_x = max(x)
    style(display = "block",
          'text-align' = 'center',
          padding = "0 4px", 
          `border-radius` = "4px",
          `font.weight` = ifelse(x==-1e-9,"normal",
                                 ifelse(abs(x)> 0.3*max_x, "bold", "normal")),
          color = ifelse(x==-1e-9,'lightgrey', ifelse(abs(x)> 0.3*max_x, 'white',
                                         ifelse(x==0,'lightgrey','black'))),
          `background-color` = ifelse(x==-1e-9,'lightgrey',
                                      csscolor(matrix(as.integer(colorRamp(...)(xnormalize(as.numeric(x)))), 
                                                      byrow=TRUE, 
                                                      dimnames=list(c("green","red","blue"), NULL),
                                                      nrow=3)))
          )
  })}


ultimate_widen = function(df, col, valname){
  df %>% select(splitname, all_of(col)) %>%
    pivot_wider(names_from = splitname,
                values_from = col) %>%
    mutate(code=valname)
  }

ultimate_rebaser = function(df, ultimate_df, split_names,
                              ultimate_col ){
    df %>%
      select(code, all_of(split_names)) %>% 
      bind_rows(ultimate_df) %>%
      rebase(ultimate_col, split_names, id_col='code')
  }

#https://www.displayr.com/formattable/
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))

new_color_bar <- function(color = "lightgreen", ...){
  formatter("span",
            style = function(x) style(
              display = "inline-block",
              direction = "rtl", 
              `unicode-bidi` = "plaintext",
              "border-radius" = "4px",
              "background-color" = color,
              width = percent(proportion(abs(as.numeric(x)), ...))
            ))
}

bg = function(start, end, color, ...) {
  paste("linear-gradient(90deg,transparent ",percent(start),",",
        color, percent(start), ",", color, percent(end),
        ", transparent", percent(end),")")
} 

color_bar2 =  function (color = "lightgray", fun = "proportion", ...) 
{
  fun <- match.fun(fun)
  formatter("span", style = function(x) style(display = "inline-block",
                                              `unicode-bidi` = "plaintext", 
                                              "background" = bg(1-fun(as.numeric(x), ...), 1, color), "width"="100%" ))
}

pm_color_bar2 <- function(color1 = "lightgreen", color2 = "pink", ...){
  formatter("span",
            style = function(x) style(
              display = "inline-block",
              color = ifelse(x> 0,'green',ifelse(x<0,'red','lightgrey')),
              "text-align" = ifelse(x > 0, 'left', ifelse(x<0, 'right', 'center')),
              "width"='100%',
              "background" = bg(ifelse(x >= 0, 0.5,xnormalize(x)),
                                ifelse(x >= 0,xnormalize(x),0.5),
                                ifelse(x >= 0, color1, color2))
            ))
}
