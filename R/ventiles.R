#' ventiles
#'
#'
#' @param tbla table with data. It has to have the variable and the target variables.
#' @param targets vector with the names of the targets.
#' @param score_name name of the score variable.
#' @param partition of group: quantile 0 to 1 by partition
#' @import dplyr
#' @export
#' @examples

#'x1 = rnorm(1000)
#'x2 = rnorm(1000)
#'z = 1 + 2 * x1 + 3 * x2
#'pr = 1/(1+exp(-z))
#'y = rbinom(1000,1,pr)
#'y1 = rbinom(1000,1,abs(pr-0.05))
#'tbla = data.frame(y=y,x1=x1,x2=x2, y1=y1)
#'f=formula(y~x1+x2)
#'lr <- glm(f, tbla, family = 'binomial')
#'tbla$prob<-predict(lr, tbla, type='response')
#'ventiles(tbla, targets=c('y', 'y1'), score_name = 'prob')


ventiles<-function(tbla, targets, score_name, partition=0.05){
  #score_name='prob'; targets=c('y', 'y1')
  a=tbla
  a$prob=as.numeric(as.character(a[, score_name]))
  cortes=unique(quantile(a$prob, probs=c(seq(0,1,partition))))
  #print(cortes)
  lim_sup=round(max(a$prob),0)
  #print(lim_sup)

  if(lim_sup<=1){lim_sup_f=1.1}
  if(lim_sup>20){lim_sup_f=100.1}
  if(lim_sup>200){lim_sup_f=1000.1}

  cortes_quedan=sort(unique(c(0,cortes[2: (length(cortes)-1) ], lim_sup_f )))
  #print(cortes_quedan)
  a$grupos=cut(a$prob, cortes_quedan, include.lowest = T, right=T)


  c=data.frame()
  for (j in 1:length(targets)){#j=1
    #print(j)
    i=targets[j]
    a$target<-a[, i]
    b=data.frame(a%>%group_by(grupos)%>%summarise(
                                                  tot=n(),
                                                  bajas=sum(target),
                                                  br=round(bajas/tot,2)))

    b$pos_ac<-round(cumsum(b$bajas)/sum(b$bajas),2)
    b$neg_ac<-round(cumsum(b$tot-b$bajas)/sum(b$tot-b$bajas),2)
    b$ks<-abs(b$pos_ac-b$neg_ac)
    b$pos_ac<-NULL
    b$neg_ac<-NULL
    br_name=paste0('br_',i )
    ks_name=paste0('ks_',i )
    br_tot=sum(b$bajas)/sum(b$tot)

    if(j==1){
    b1=b[, c('grupos', 'tot', 'br', 'ks')]
    colnames(b1)<-c('grupos', 'tot', br_name, ks_name)
    tot=sum(b1$tot)
    linea_f=data.frame(grupos='todos', tot=tot, br_name=br_tot, ks_name=max(b1[,ks_name]) )
    colnames(linea_f)<-c('grupos', 'tot', br_name, ks_name)
    c=rbind(b1, linea_f)
     }else {
      b1=b[,c('grupos','br', 'ks')]
      colnames(b1)<-c('grupos',br_name, ks_name)
      linea_f=data.frame(grupos='todos', br_name=br_tot, ks_name=max(b1[,ks_name]) )
      colnames(linea_f)<-c('grupos',  br_name, ks_name)
      b2=rbind(b1, linea_f)
      c=merge(c,b2, by='grupos', all.x=T)
      c=c[order(c$grupos),]
      }
  }
  nombres=colnames(c)
  nombres_ks=nombres[grep('ks_',nombres)]
  nombres_br=nombres[grep('br_',nombres)]
  c=c[, c('grupos', 'tot', nombres_br, nombres_ks)]

  c$min_prob=sapply(as.character(c$grupos), function(x) strsplit(x, '\\,')[[1]][1])
  c$max_prob=sapply(as.character(c$grupos), function(x) strsplit(x, '\\,')[[1]][2])

  c$min_prob=as.numeric(gsub('\\(', '', c$min_prob))
  c$max_prob=as.numeric(gsub('\\]', '', c$max_prob))
  return(c)
}



