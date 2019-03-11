#' ventiles
#'
#'
#' @param tbla table with data. It has to have the variable and the target variables.
#' @param targets vector with the names of the targets.
#' @param score_name name of the score variable.
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


ventiles<-function(tbla, targets, score_name){
  a=tbla
  a$prob=a[, score_name]
  cortes=quantile(a$prob, probs=c(seq(0,1,0.05)))
  a$grupos=cut(a$prob, c(0,cortes[2: (length(cortes)-1) ],1 ))

  c=data.frame()
  for (j in 1:length(targets)){#j=1
    #print(j)
    i=targets[j]
    a$target<-a[, i]
    b=data.frame(a%>%group_by(grupos)%>%summarise(tot=n(), bajas=sum(target), br=round(bajas/tot,3)))

    b$pos_ac<-round(cumsum(b$tot*b$br)/sum(b$tot*b$br),2)
    b$neg_ac<-round(cumsum(b$tot*(1-b$br))/sum(b$tot*(1-b$br)),2)
    b$ks<-abs(b$pos_ac-b$neg_ac)
    b$pos_ac<-NULL
    b$neg_ac<-NULL
    br_name=paste0('br_',i )
    ks_name=paste0('ks_',i )
    if(j==1){
    b1=b[, c('grupos', 'tot', 'br', 'ks')]
    colnames(b1)<-c('grupos', 'tot', br_name, ks_name)
    c=b1
    }else {
      b1=b[,c('grupos','br', 'ks')]
      colnames(b1)<-c('grupos',br_name, ks_name)
      c=merge(c,b1, by='grupos', all.x=T)

      }
  }
  nombres=colnames(c)
  nombres_ks=nombres[grep('ks_',nombres)]
  nombres_br=nombres[grep('br_',nombres)]
  c=c[, c('grupos', 'tot', nombres_br, nombres_ks)]
  return(c)

  }
