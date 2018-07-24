dataset_boxplot_fcn=function(obj,opt) {
  # n is column number of p-values
  varn=colnames(Xt)[i]
  temp=data.frame(x=SM[[factor_name]],y=Xt[,i])
  str=formatC(signif(RESULTS[i,n],digits=3),digits=3)
  p<-ggplot(na.omit(temp), aes(x, y, color=x)) +
    geom_boxplot() +
    xlab(NULL) +
    ylab(varn)

  if (interaction)
  {
    p=p+ggtitle(varn, subtitle=paste0('p(interaction) = ', str))
  } else
  {
    p=p+ggtitle(varn, subtitle=paste0('p = ', str))
  }
  p=p+  scale_colour_Publication()+ theme_Publication(base_size = 12) +
    guides(colour=guide_legend(title=NULL))
  return(p)
}
