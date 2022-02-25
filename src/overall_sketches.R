get_pos_chart = function(df_long, code, embed=FALSE,
                         height=30, aspect_ratio=1, size=5) {
  # Get the data for the specified driver
  subdf = df_long[df_long['code']==code,]
  
  ymax = max(10.6, max(subdf$Pos)+0.1)
  
  g = ggplot(subdf,
             aes(x=as.integer(Stage), y=Pos, group=code)) +
    geom_step(direction='mid', color='blue', size=size) +
    geom_hline(yintercept=0.8, linetype='dotted',
               size=size, color='black') +
    geom_hline(yintercept=3.35, linetype='dotted', 
               size=size, color='black') +
    geom_hline(yintercept=10.5, color='darkgrey',
               size=size) +
    #scale_y_continuous(trans = "reverse") +
    scale_y_reverse( lim=c(ymax, 0.8)) +
    theme_void() + scale_x_continuous(expand=c(0,0)) #+
  #theme(aspect.ratio=0.1)
  
  if (embed)
    gt::ggplot_image(g, height = height, aspect_ratio=aspect_ratio)
  else
    g
}
