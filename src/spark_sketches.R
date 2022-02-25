generate_spark_bar = function(df, col, typ='Gap'){
  df %>% gather(key ="Stage",
                value =!!typ, all_of(stage_codes)) %>%
    group_by(code) %>%
    summarize(spk_ = spk_chr(-!!as.symbol(typ), type ="bar"))
}

highlight_first =  function (...) 
{
  formatter("span",
            style = function(x) ifelse(x==1,
                                       style(display = "block", 
                                             padding = "0 4px",
                                             `color` = "black",
                                             `column-width`='4em',
                                             `border-radius` = "4px",
                                             `background-color` = 'lightgrey'),
                                       style()))
}

coldiffs = function(df, cols, dropfirst=FALSE, firstcol=NULL){
  cols = as.character(cols)
  # [-1] drops the first column, [-ncol()] drops the last
  df_ = df[,cols][-1] - df[,cols][-ncol(df[,cols])]
  
  # The split time to the first split is simply the first split time
  df_[cols[1]] = df[cols[1]]
  # Return the dataframe in a sensible column order
  df_ = df_ %>% select(all_of(cols))
  
  if (!is.null(firstcol))
    df_[, cols[1]] = firstcol
  
  if (dropfirst)
    df_[,cols][-1]
  else
    df_
}

spark_df = function(df){
  # We need to create an htmlwidget form of the table
  out = as.htmlwidget(formattable(df))
  
  # The table also has a requirement on the sparkline package
  out$dependencies = c(out$dependencies,
                       htmlwidgets:::widget_dependencies("sparkline",
                                                         "sparkline"))
  out
}