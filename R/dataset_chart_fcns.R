

dataset_boxplot_fcn=function(obj,opt) {

  # assume obj is a dataset object
  # get data
  Xt=data(obj)
  # column name
  varn=opt$feature_to_plot
  # get requested column
  Xt=Xt[[opt$feature_to_plot]]

  # meta data
  SM=sample.meta(D)[ ,1]
  # get color pallete using pmp
  clrs=pmp::createClassAndColors(class = SM)
  SM=clrs$class

  #prep the plot
  temp=data.frame(x=SM,y=Xt)

  p<-ggplot(na.omit(temp), aes(x, y, color=x)) +
    geom_boxplot() +
    xlab(NULL) +
    ylab(varn) +
    ggtitle(varn) +
    scale_colour_manual(values=clrs$manual_colors,name=opt$factor_name) +
    theme_Publication(base_size = 12) +
    theme(legend.position="none")

  if (opt$label_outliers) {
    L=levels(SM)

    outliers=numeric()
    for (lvl in L) {
      s=fivenum(Xt[SM==lvl])
      rng=s[4]-s[2]
      wsk=s[c(2,4)]+c(-1.5*rng,1.5*rng)
      outliers=c(outliers,which((Xt>wsk[2] | Xt[SM==lvl]<wsk[1]) & SM==lvl))
    }
    outlier_df=temp[outliers,]
    names=paste0('  ',rownames(data(D)))
    p=p+geom_text(data=outlier_df,aes(x, y, color=x, label=names[outliers]),hjust='left')
  }

  return(p)
}
