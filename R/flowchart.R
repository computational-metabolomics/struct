
library(structtoolbox)

M = mean_centre()

out=struct_obj_box(M,mar=0.02)

struct_obj_box=function(M,mar=0.02) {

    msg=character(0)
    if (is(M,'parameter_class')) {
        p=param.ids(M)
        for  (i in seq_len(length(p))) {
            msg[i]=paste0(param.name(M,p[i]),' = ', param.value(M,p[i]) )
        }
    }

    msg2=character(0)
    if (is(M,'outputs_class')) {
        o=output.ids(M)
        for  (i in seq_len(length(o))) {
            msg2[i]=paste0(output.name(M,o[i]))
        }
    }

    c=chart.names(M,'obj')
    msg3=character(0)
    for  (i in seq_len(length(c))) {
        msg3[i]=paste0(name(c[[i]]))
    }

    msg=paste(msg,collapse='\n')
    msg2=paste(msg2,collapse='\n')
    msg3=paste(msg3,collapse='\n')

    MSG=character(0)
    if (nchar(msg)>0) MSG=c(MSG,msg)
    if (nchar(msg2)>0) MSG=c(MSG,msg2)
    if (nchar(msg3)>0) MSG=c(MSG,msg3)

    out=struct_box(ttl=name(M),msg=MSG,mar)
    return(out)
}

struct_box=function(ttl,msg,mar=0.02,mid=c(0.5,0.5)) {

    # if no plot device, then make one
    if (dev.cur() == 1) {
        plot.new()
    }

    #nlines=length(unlist(strsplit(msg,'\n')))

    height=strheight(ttl,font=2) +
        sum(strheight(msg)) +
        (1*(length(msg)*mar)) + # msgs
        (2*mar) + # title
        (length(msg))*mar # dividers


    height_ttl=strheight(ttl)

    width=max(strwidth(ttl,font=2),strwidth(msg))

    # coords
    xleft   = mid[1]-mar-(width/2)
    xright  = mid[1]+mar+(width/2)
    ybottom = mid[2]-(height/2)
    ytop    = mid[2]+(height/2)

    # full box
    rect(xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom)

    # title box
    rect(xleft=xleft,ybottom=ytop-height_ttl-(2*mar),ytop=ytop,xright=xright,col='#F1C965')

    # title
    text(x=mid[1],y=ytop-mar-(height_ttl/2),labels = ttl,font=2)

    # messages
    msg_top=ytop-(1*mar)                  # top margin
    msg_top=msg_top-strheight(height_ttl) # title
    msg_top=msg_top-(1*mar)               # bottom margin
    for (i in 1:length(msg)) {
        msg_top=msg_top-(1*mar) # top margin
        msg_top=msg_top-strheight(msg[i]) # msg height

        text(x=xleft+mar,y=msg_top,labels = msg[i],adj=c(0,0))
        msg_top=msg_top-mar # bottom margin

        #rect(xleft=xleft+mar,xright=-mar,ybottom=msg_top,ytop=msg_top+mar+mar+strheight(msg[i]))

        if (i<(length(msg))) {
            segments(x0=xleft+mar,y0 = msg_top,x1=xright-mar,y1=msg_top)
            #msg_top=msg_top-mar # bottom margin
        }
    }
    #s=seq(from=0,by=strheight('M'),length=nlines)

    out=list(xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,
        ttl_x=mid[1],ttl_y=ytop-mar-(height_ttl/2)
    )
    return(out)
}



