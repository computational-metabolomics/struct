

dataset_boxplot_fcn=function(obj,opt) {

  # assume obj is a dataset object
  # get data
  Xt=data(obj)
  # column name
  varn=opt$feature_to_plot
  # get requested column
  Xt=Xt[[opt$feature_to_plot]]
  # meta data
  SM=sample.meta(obj)[ ,1]

  # remove NA
  SM=SM[!is.na(Xt)]
  Xt=Xt[!is.na(Xt)]

  # count number of values
  L=levels(SM)
  count=numeric(length(L))
  for (i in 1:length(L)) {
    count[i]=sum(SM==L[i])
  }

  # get color pallete using pmp
  clrs=pmp::createClassAndColors(class = SM)
  SM=clrs$class

  #prep the plot
  temp=data.frame(x=SM,y=Xt)
  p<-ggplot(temp, aes(x, y, color=x)) +
    geom_boxplot() +
    xlab(NULL) +
    ylab(varn) +
    ggtitle(varn) +
    scale_colour_manual(values=clrs$manual_colors,name=opt$factor_name) +
    theme_Publication(base_size = 12) +
    theme(legend.position="none")

  if (opt$show_counts) {
      newlabels=as.character(count)
      newlabels=paste0(as.character(L),'\n(n = ',newlabels,')')
      p=p+scale_x_discrete(labels=newlabels)
  }

  if (opt$label_outliers) {
    outliers=numeric()
    for (l in L) {
      IN=which(SM==l)
      outliers=c(outliers,IN[which( Xt[IN]>(quantile(Xt[IN], 0.75) + 1.5*IQR(Xt[IN]))) ] )
      outliers=c(outliers,IN[which( Xt[IN]<(quantile(Xt[IN], 0.25) - 1.5*IQR(Xt[IN]))) ] )
      }
    outlier_df=temp[outliers,]
    outlier_df$out_label=paste0('  ',rownames(data(obj)))[outliers]
    p=p+geom_text(data=outlier_df,aes(group=x,color=x,label=out_label),hjust='left')
  }

  return(p)
}
