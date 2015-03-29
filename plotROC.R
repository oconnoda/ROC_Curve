
# Plots the ROC of the given prediction object {ROCR}
#  p: prediction object
#  cutoff: threshold for probabilty predictions
#  filename: optional name for interactive SVG image of the plot
# Spring 2015, David O'Connor
plot.ROC = function(p,cutoff=0.5,filename=NA,...) {
  require(ROCR)
  require(RJSONIO)
  require(ggplot2)
  require(gridSVG)
  require(grid)
  
  # linear interpolation of x 
  interp = function(x,xvalues,yvalues) {
    if (x <= min(xvalues)) {
      return(yvalues[length(yvalues)])
    }
    if (x >= max(xvalues)) {
      return(yvalues[1])
    }
    i = 1
    while(x < xvalues[i]) {
      i = i + 1
    }
    return(yvalues[i-1]+ (yvalues[i]-yvalues[i-1]) *  
             (xvalues[i-1]-x) / (xvalues[i-1]-xvalues[i]))
  }
  
  auc.color.fill = rgb(216,216,255,128,maxColorValue=255)
  auc.color.text = rgb(0,0,0.6)
  bar.color.neg = rgb(179,179,179,179,maxColorValue=255)
  bar.color.pos = rgb(0,0,0.6,0.7)

  # grab some performance metrics
  auc = performance(p,"auc")@y.values[[1]]
  acc = performance(p,"acc")
  roc = performance(p,"tpr","fpr")
  roc@alpha.values[[1]][1]=1
  roc.df = data.frame(fpr=roc@x.values[[1]],tpr=roc@y.values[[1]],cutoff=roc@alpha.values[[1]])

  # draw the plots
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(3, 3)))
  v1 = viewport(layout.pos.col=c(1,2), layout.pos.row=c(1,2))
  p1= ggplot(data=roc.df) +
    xlab(roc@x.name) + ylab(roc@y.name) + ggtitle("ROC Curve") +
    geom_ribbon(aes(x=fpr,ymin=0,ymax=tpr),fill=auc.color.fill) +
    geom_line(aes(x=fpr,y=tpr),color=rgb(0,0,1,maxColorValue=255)) +
    annotate("segment",x=0,xend=1,y=0,yend=1,linetype=2,color="grey50") +
    annotate("text",x=0.96,y=0.05,size=4,hjust=1,label=paste("AUC =",format(auc,digits=2)),col=auc.color.text) +
    annotate("point",x=interp(cutoff,roc@alpha.values[[1]],roc@x.values[[1]]),y=interp(cutoff,roc@alpha.values[[1]],roc@y.values[[1]]),colour="red",size=3) +
    coord_cartesian(xlim = c(0, 1),ylim=c(0,1))
  
  v2 = viewport(layout.pos.col=c(3), layout.pos.row=c(1))
  baseline = max(c(p@n.pos[[1]],p@n.neg[[1]]))/(p@n.pos[[1]] + p@n.neg[[1]])
  p2 = ggplot(aes(xmin=0,ymin=0,xmax=1,ymax=1),data=data.frame(x=acc@x.values[[1]],y=acc@y.values[[1]])) +
    xlab(acc@x.name) + ylab(acc@y.name) +
    geom_line(aes(x=x,y=y),color=rgb(0,0,2,maxColorValue=255)) +
    annotate("segment",x=0,xend=1,y=baseline,yend=baseline,linetype=2,color=auc.color.text) +
    annotate("text",x=0.5,y=baseline,vjust=1.5,size=4,label="baseline",color=auc.color.text) +
    geom_vline(xintercept=cutoff,color="red",size=0.4) +
    coord_cartesian(xlim = c(0, 1))
  p2 = p2 + theme_classic()

  v3 = viewport(layout.pos.col=c(3), layout.pos.row=c(2))
  p3 = ggplot(aes(xmin=0,ymin=0,xmax=1,ymax=1),data=data.frame(x=roc@alpha.values[[1]],y=roc@y.values[[1]])) +
    xlab(roc@alpha.name) + ylab(roc@y.name) + 
    geom_line(aes(x=x,y=y),color=rgb(0,0,3,maxColorValue=255)) +
    geom_vline(xintercept=cutoff,color="red",size=0.4) +
    coord_cartesian(xlim = c(0, 1))
  p3 = p3 + theme_classic()

  v4 = viewport(layout.pos.col=c(3), layout.pos.row=c(3))
  p4 = ggplot(aes(xmin=0,ymin=0,xmax=1,ymax=1),data=data.frame(x=roc@alpha.values[[1]],y=roc@x.values[[1]])) +
    xlab(roc@alpha.name) + ylab(roc@x.name) + 
    geom_line(aes(x=x,y=y),color=rgb(0,0,4,maxColorValue=255)) +
    geom_vline(xintercept=cutoff,color="red",size=0.4) +
    coord_cartesian(xlim = c(0, 1))
  p4 = p4 + theme_classic()

  sp = p@predictions[[1]]
  sp = sort(sp,decreasing=TRUE)
  v5 = viewport(layout.pos.col=c(1,2), layout.pos.row=c(3))
  p5 = ggplot(aes(ymin=0,ymax=1),data=data.frame(x=1:length(sp),y=sp,col=p@labels[[1]])) +
    xlab(paste("Predictions  (N = ",length(sp),")",sep="")) +
    ylab("Probability") + 
    geom_segment(aes(x=x,y=0,xend=x,yend=y,colour=col),size=max(c(100/length(sp),0.15))) +
    scale_colour_manual(breaks=c("1","0"),labels=c("Positive Outcome","Negative Outcome"),
          values = c(bar.color.neg,bar.color.pos)) +
    guides(colour=guide_legend(title=NULL)) +
    geom_hline(yintercept=cutoff,color="red",size=0.4) +
    coord_cartesian(ylim = c(0, 1)) + 
    theme(legend.position="top",legend.background=element_rect(fill=rgb(1,1,1,0.2)),
          legend.key.size=unit(0.7,"char"),
          legend.direction="horizontal",
          plot.margin = unit(c(0,4,2,3),"mm"),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  print(p1,vp=v1)
  print(p2,vp=v2)
  print(p3,vp=v3)
  print(p4,vp=v4)
  print(p5,vp=v5)

  # add animation
  jroc.df = toJSON(roc.df)
  grid.script(script=paste("roc =", jroc.df),inline=TRUE)
  code = "
function interp(t,e,n){if(e[0]<e[e.length-1])return interp(t,e.reverse(),n.reverse());if(t<=e[e.length-1])return n[n.length-1];if(t>=e[0])return n[0];for(i=0;t<e[i];)i+=1;return n[i-1]+(n[i]-n[i-1])*(e[i-1]-t)/(e[i-1]-e[i])}function animAttr(t,e,i){t.setAttribute('to',i),t.setAttribute('from',e),t.beginElement()}function animLine(t,e,i,n){'x'==n?(x=viewportConvertPos(grobViewport(t),i,1,from='npc',to='svg').x,y1=viewportConvertPos(grobViewport(t),0,0,from='npc',to='svg').y,y2=viewportConvertPos(grobViewport(t),1,1,from='npc',to='svg').y,newpoints=x+','+y1+' '+x+','+y2,animAttr(e,document.getElementById(t).getAttribute('points'),newpoints)):(y=viewportConvertPos(grobViewport(t),1,i,from='npc',to='svg').y,x1=viewportConvertPos(grobViewport(t),0,0,from='npc',to='svg').x,x2=viewportConvertPos(grobViewport(t),1,1,from='npc',to='svg').x,newpoints=x1+','+y+' '+x2+','+y,animAttr(e,document.getElementById(t).getAttribute('points'),newpoints))}function moveCutoff(t){var e=document.getElementById(dot_id),i=document.getElementById('line1a'),n=document.getElementById('line2a'),r=document.getElementById('line3a'),o=document.getElementById('line4a'),l=document.getElementById('dota'),d=document.getElementById('dotb');animLine(line1_id,i,t,'x'),animLine(line2_id,n,t,'x'),animLine(line3_id,r,t,'x'),animLine(line4_id,o,t,'y');var u={x:interp(t,roc.cutoff,roc.fpr),y:interp(t,roc.cutoff,roc.tpr)},m=viewportConvertPos(grobViewport(dot_id),u.x,u.y,from='npc',to='svg');animAttr(l,e.getAttribute('x'),m.x),animAttr(d,e.getAttribute('y'),m.y)}function toSVG(t){var e=t.target.ownerSVGElement.createSVGPoint();e.x=t.clientX,e.y=t.clientY;var i=t.target.getScreenCTM(),n=e.matrixTransform(i.inverse());return n}function click2(t){var e=toSVG(t);moveCutoff(viewportConvertPos(grobViewport(t.target.id),e.x,e.y,from='svg',to='npc').x)}function click3(t){var e=toSVG(t);moveCutoff(viewportConvertPos(grobViewport(t.target.id),e.x,e.y,from='svg',to='npc').y)}function click(t){var e=toSVG(t),i=viewportConvertPos(grobViewport(t.target.id),e.x,e.y,from='svg',to='npc');moveCutoff(interp(i.x,roc.fpr,roc.cutoff))}function showNode(t){if(1==t.nodeType){if(t.hasAttribute('fill')){var e=t.getAttribute('fill');if(e&&'rgb(255,0,0)'==e){dot_id=t.getAttribute('id');var i=t.parentElement.parentElement;i.onclick=click,i.setAttribute('cursor','crosshair'),i=i.parentElement;var n=i.getAttribute('clip-path');n=n.slice(5,-1),n=document.getElementById(n),n=n.firstElementChild;var r=n.getAttribute('x');r=Number(r)-5,n.setAttribute('x',r),r=n.getAttribute('y'),r=Number(r)-5,n.setAttribute('y',r),r=n.getAttribute('width'),r=Number(r)+10,n.setAttribute('width',r),r=n.getAttribute('height'),r=Number(r)+10,n.setAttribute('height',r)}else e&&'rgb(216,216,255)'==e&&(dline5_id=t.getAttribute('id'))}if(t.hasAttribute('stroke')){if(e=t.getAttribute('stroke'),e&&'rgb(255,0,0)'==e)if(''==line1_id){line1_id=t.getAttribute('id');var i=t.parentElement.parentElement.parentElement;i.onclick=click2,i.setAttribute('cursor','crosshair')}else if(''==line2_id){line2_id=t.getAttribute('id');var i=t.parentElement.parentElement.parentElement;i.onclick=click2,i.setAttribute('cursor','crosshair')}else if(''==line3_id){line3_id=t.getAttribute('id');var i=t.parentElement.parentElement.parentElement;i.onclick=click2,i.setAttribute('cursor','crosshair')}else if(''==line4_id){line4_id=t.getAttribute('id');var i=t.parentElement.parentElement.parentElement;i.setAttribute('cursor','crosshair'),i.onclick=click3}e&&'rgb(0,0,1)'==e&&(dline1_id=t.getAttribute('id')),e&&'rgb(0,0,2)'==e&&(dline2_id=t.getAttribute('id')),e&&'rgb(0,0,3)'==e&&(dline3_id=t.getAttribute('id')),e&&'rgb(0,0,4)'==e&&(dline4_id=t.getAttribute('id')),e&&'rgb(179,179,179)'==e&&''==dline6_id&&(dline6_id=t.parentElement.parentElement.getAttribute('id'))}}}function walkDOM(t,e){for(e(t),t=t.firstChild;t;)walkDOM(t,e),t=t.nextSibling}function makeAnim(t,e,i)
  {var n=document.createElementNS('http://www.w3.org/2000/svg','animate');n.setAttributeNS(null,'attributeName',i),n.setAttributeNS(null,'fill','freeze'),n.setAttributeNS(null,'begin','indefinite'),n.setAttribute('dur','200ms'),n.setAttributeNS('http://www.w3.org/1999/xlink','href','#'+e),n.setAttributeNS(null,'id',t),document.documentElement.appendChild(n)}function initDOM(){walkDOM(document.documentElement,showNode),makeAnim('dota',dot_id,'x'),makeAnim('dotb',dot_id,'y'),makeAnim('line1a',line1_id,'points'),makeAnim('line2a',line2_id,'points'),makeAnim('line3a',line3_id,'points'),makeAnim('line4a',line4_id,'points'),initAnim(),document.documentElement.setAttribute('width','100%'),document.documentElement.setAttribute('height','100%'),document.documentElement.setAttribute('preserveAspectRatio','xMinYMid meet')}function animPolyline(t,e,i,n){var r=document.createElementNS('http://www.w3.org/2000/svg','animate');r.setAttributeNS(null,'attributeName',n),r.setAttributeNS(null,'fill','freeze'),r.setAttributeNS(null,'begin','400ms'),r.setAttribute('dur','500ms'),r.setAttributeNS(null,'from',e),r.setAttributeNS(null,'to',i),r.setAttributeNS('http://www.w3.org/1999/xlink','href','#'+t),document.documentElement.appendChild(r)}function initAnim(){var t=dline1_id,e=document.getElementById(t);p1=viewportConvertPos(grobViewport(t),0,0,from='npc',to='svg'),p2=viewportConvertPos(grobViewport(t),1,1,from='npc',to='svg');var i,n='',r=roc.fpr.length,o=(p2.x-p1.x)/r,l=(p2.y-p1.y)/r;for(i=0;r>i;i++)n+=(p1.x+i*o).toFixed(2)+','+(p1.y+i*l).toFixed(2)+' ';var d=e.getAttribute('points');for(e.setAttribute('points',n),animPolyline(t,n,d,'points'),t=dline5_id,e=document.getElementById(t),i=r-1;i>=0;i--)n+=(p1.x+i*o).toFixed(2)+','+p1.y.toFixed(2)+' ';for(d=e.getAttribute('points'),e.setAttribute('points',n),animPolyline(t,n,d,'points'),t=dline2_id,p1=viewportConvertPos(grobViewport(t),0,0,from='npc',to='svg'),p2=viewportConvertPos(grobViewport(t),1,1,from='npc',to='svg'),o=(p2.x-p1.x)/(r/2),l=(p2.y-p1.y)/(r/2),e=document.getElementById(t),n='',i=0;r>i;i++)n+=r/2>i?p1.x.toFixed(2)+','+(p2.y-i*l).toFixed(2)+' ':(p1.x+i*l).toFixed(2)+','+p1.y.toFixed(2)+' ';for(d=e.getAttribute('points'),e.setAttribute('points',n),animPolyline(t,n,d,'points'),t=dline3_id,p1=viewportConvertPos(grobViewport(t),0,0,from='npc',to='svg'),p2=viewportConvertPos(grobViewport(t),1,1,from='npc',to='svg'),o=(p2.x-p1.x)/(r/2),l=(p2.y-p1.y)/(r/2),e=document.getElementById(t),n='',i=0;r>i;i++)n+=r/2>i?p1.x.toFixed(2)+','+(p2.y-i*l).toFixed(2)+' ':(p1.x+i*l).toFixed(2)+','+p1.y.toFixed(2)+' ';for(d=e.getAttribute('points'),e.setAttribute('points',n),animPolyline(t,n,d,'points'),t=dline4_id,p1=viewportConvertPos(grobViewport(t),0,0,from='npc',to='svg'),p2=viewportConvertPos(grobViewport(t),1,1,from='npc',to='svg'),o=(p2.x-p1.x)/(r/2),l=(p2.y-p1.y)/(r/2),e=document.getElementById(t),n='',i=0;r>i;i++)n+=r/2>i?p1.x.toFixed(2)+','+(p2.y-i*l).toFixed(2)+' ':(p1.x+i*l).toFixed(2)+','+p1.y.toFixed(2)+' ';d=e.getAttribute('points'),e.setAttribute('points',n),animPolyline(t,n,d,'points'),t=dline6_id,e=document.getElementById(t);var u=document.createElementNS('http://www.w3.org/2000/svg','animateTransform');u.setAttributeNS(null,'attributeName','transform'),u.setAttributeNS(null,'type','scale'),u.setAttributeNS(null,'fill','freeze'),u.setAttributeNS(null,'begin','400ms'),u.setAttribute('dur','500ms'),u.setAttributeNS(null,'from','1 ,0'),u.setAttributeNS(null,'to','1 ,1'),u.setAttributeNS('http://www.w3.org/1999/xlink','href','#'+t),e.setAttributeNS(null,'transform','scale(1 0)'),document.documentElement.appendChild(u);var u=document.createElementNS('http://www.w3.org/2000/svg','animate');u.setAttributeNS(null,'attributeName','opacity'),u.setAttributeNS(null,'fill','freeze'),u.setAttributeNS(null,'begin','0ms'),u.setAttribute('dur','400ms'),u.setAttributeNS(null,'from',0),u.setAttributeNS(null,'to',1),document.documentElement.setAttributeNS(null,'opacity',0),document.documentElement.appendChild(u)}var dot_id='',line1_id='',line2_id='',line3_id='',line4_id='',dline1_id='',dline2_id='',dline3_id='',dline4_id='',dline5_id='',dline6_id='';document.documentElement.onload=initDOM;
  
"
  grid.script(script=code,inline=TRUE)
  if (!is.na(filename)) {
    grid.export(name=filename,exportJS="inline",exportCoords="inline",progress=FALSE)
  }
}

