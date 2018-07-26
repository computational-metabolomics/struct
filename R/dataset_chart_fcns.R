

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
    xlab(opt$factor) +
    ylab('') +
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


missing_value_histogram=function(obj,opt) {

  # assume obj is a dataset object

  # get data
  Xt=data(obj)
  # meta data
  SM=sample.meta(obj)[ ,1]

  if (opt$by_sample)
  {
    # count NS per sample
    count=apply(Xt,1,function(x) {sum(is.na(x))/length(x)*100})
    txt='Missing values per sample'
  } else {
    # count NS per feature
    count=apply(Xt,2,function(x) {sum(is.na(x))/length(x)*100})
    txt='Missing values per feature'
  }

  A=data.frame(x=count)
  p=ggplot (data=A, aes(x)) + geom_histogram()+
    xlab ("missing values, %")+ ggtitle(txt)+
    xlim (0,100)+
    scale_colour_Publication()+ theme_Publication(base_size = 12)

  return(p)
}

missing_value_boxplot=function(obj,opt) {

  # assume obj is a dataset object

  # get data
  Xt=data(obj)
  # meta data
  SM=sample.meta(obj)[ ,1]

  L=levels(SM)

  if (opt$by_sample)
  {
    # count NS per sample
    count=apply(Xt,1,function(x) {sum(is.na(x))/length(x)*100})
    result=matrix(0,nrow=nrow(Xt),ncol=2)
    result[,1]=count
    result[,2]=SM
    txt='Missing values per sample'
    # get color pallete using pmp
    clrs=pmp::createClassAndColors(class = SM)
    A=data.frame(x=clrs$class,y=result[,1])
  } else {
    for (i in 1:length(L)) {
      # count NS per sample
      count=apply(Xt[SM==L[i],,drop=FALSE],2,function(x) {sum(is.na(x))/sum(SM==L[i])*100})
      temp=data.frame(y=count,x=L[i])
      if (i==1) {
        result=temp
      } else {
        result=rbind(result,temp)
      }
    }
      txt='Missing values per feature'
      # get color pallete using pmp
      clrs=pmp::createClassAndColors(class = as.factor(result$x))
      A=data.frame(x=clrs$class,y=result$y)
  }


  p=ggplot (data=A, aes(x, y, color=x)) +
    geom_boxplot() +
     ggtitle(txt) +
    xlab(opt$factor) +
    ylim (0,100)+
    scale_colour_manual(values=clrs$manual_colors,name=opt$factor_name) +
    theme_Publication(base_size = 12) +
    ylab ("missing values, %") +
    coord_flip()+
    theme(legend.position="none")

  if (opt$show_counts) {
    L=levels(A$x)
    num=numeric(length(L))
    for (i in 1:length(L)) {
      num[i]=sum(A$x==L[i])
    }
    newlabels=as.character(num)
    newlabels=paste0(as.character(L),'\n(n = ',newlabels,')')
    p=p+scale_x_discrete(labels=newlabels)
  }

  return(p)
}
