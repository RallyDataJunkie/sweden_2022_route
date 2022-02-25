library(ggplot2)
library(directlabels)

## ---- pace_map_highlight --------
pace_map_highlight = function(sub_df, m, n){
  # If we don't grab the actual value
  # the referenced value is used...
  m_ = m
  geom_rect(data=sub_df,
            aes(xmin= start_dist + (m_-1) * (cum_dist - start_dist)/n,
                xmax= start_dist + m_ * (cum_dist - start_dist)/n,
                ymin = ifelse(pace>0,0,pace),
                ymax = ifelse(pace>0,pace,0),
                fill = pace>0, alpha=0.7))
}

## ---- pace_map_highlight_many --------
pace_map_highlight_many = function(df, g, codes, id_col='code_driver'){
  n = length(codes)
  for (m in 1:n){
    sub_df = df[df[id_col]==codes[m],]
    g = g + pace_map_highlight(sub_df, m, n)
  }
  
  g
}


## ---- pace_map --------
pace_map = function(pace_long, limits=NULL,
                    labels=TRUE, drivers=NULL, lines=TRUE,
                    xstart='start_dist', xend='cum_dist',
                    pace='pace', typ='bar', pace_label_offset=0.03,
                    label_dodge = 15, label_size=1,
                    id_col='code_driver', yflip=FALSE){
  
  # There are downstream dependiencies with colnames baked in atm...
  pace_long$start_dist = pace_long[[xstart]]
  pace_long$cum_dist = pace_long[[xend]]
  pace_long$pace = pace_long[[pace]]
  
  
  g0 = ggplot(pace_long, aes_string(group=id_col, label=id_col)) +
    geom_hline(yintercept = 0,
               colour='lightgrey', linetype='dotted') +
    geom_segment(aes(x=start_dist, xend=cum_dist,
                     y=pace, yend=pace),
                 color = 'lightgrey')
  
  if (lines) {
    lines_df = data.frame(cum_dist=unique(pace_long$cum_dist))
    g0 =g0 + geom_vline(data=lines_df, aes(xintercept = cum_dist),
                        color='lightgrey', linetype='dotted')
  }
  
  if (labels){
    g0 = g0 + geom_text(aes(x= (start_dist+cum_dist)/2,
                            y=pace+pace_label_offset),
                        position = position_dodge(label_dodge),
                        size=label_size)
  }
  if (!is.null(drivers) ){
    if (typ=='bar'){
      g0 = pace_map_highlight_many(pace_long, g0,
                                   c(drivers), id_col=id_col)
    } else if (typ=='highlight')
    {
      focus = pace_long[pace_long[id_col] %in% c(drivers),]
      g0 = g0 + geom_segment(data=focus,
                             aes(x=start_dist, xend=cum_dist,
                                 y=pace, yend=pace, color = pace>0))
    }
  }
  
  g0 = g0 + coord_cartesian(ylim=limits)
  
  if (yflip)
    g0 = g0 + scale_y_reverse()
  
  g0 + theme_classic() + theme(legend.position="none")
}

## ---- off_the_pace_chart --------
off_the_pace_chart = function(pace_long, highlight=NULL,
                              label_typ='dl',
                              dist='cum_dist', t='totalDurationGapS',
                              code='code_driver', ylim=NULL, yflip=FALSE){
  
  g_otp = ggplot(pace_long, aes_string(x=dist, y=t,
                                       color=code)) +
    geom_line() +
    # Retain the points outside the limits
    # by using coord_cartesian()
    # We can also flip the coordinate axis
    coord_cartesian(ylim=ylim) + theme_classic()
  
  #off_the_pace_end =  pace_long[pace_long[dist] == max(pace_long[dist]),]
  off_the_pace_end = pace_long[which.max(pace_long[[dist]]),]
  if (!is.null(highlight))
    g_otp = g_otp + gghighlight::gghighlight(code_driver %in% c(highlight),
                                             unhighlighted_params=list(alpha=0.1))
  else if (label_typ=='dl')
    g_otp = g_otp + geom_dl(aes_string(label = code, x=dist),
                            # cex is text label size
                            method = list('last.bumpup', cex = 0.5))
  else 
    g_otp = g_otp + ggrepel::geom_text_repel(data = off_the_pace_end,
                                             aes_string(label = code),
                                             size = 3)
  if (yflip)
    g_otp = g_otp + scale_y_reverse()
  
  g_otp + theme(legend.position="none")
}

## ---- spark_df --------
spark_df = function(df){
  # We need to create an htmlwidget form of the table
  out = as.htmlwidget(formattable(df))
  
  # The table also has a requirement on the sparkline package
  out$dependencies = c(out$dependencies,
                       htmlwidgets:::widget_dependencies("sparkline",
                                                         "sparkline"))
  out
}