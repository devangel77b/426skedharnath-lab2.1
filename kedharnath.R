library(ggplot2)
raw <- read.csv("kedharnath.csv",header=TRUE)

library(dplyr)
data <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2,
 mhat = m2.kg/(m1.kg+m2.kg+mc.kg),
 ahat = ameas.ms2/9.81,
 )

apredicted.ms2 <- function(m2,m1,mc){
  9.81*m2/(m2+m1+mc)
}

fig <- ggplot(data)+geom_point(aes(x=m2.kg,y=ameas.ms2))+
    ylim(0,4)+xlim(0,0.5)+
    geom_function(fun=apredicted.ms2,args=list(m1=0.280,mc=0.5),color='blue')+
    xlab('$m_2$ (\\unit{\\kilo\\gram})')+
    ylab('$a$ (\\unit{\\meter\\per\\second\\squared})')+
    theme_bw(base_size=8)

library(svglite)
svglite('fig3a.svg',width=3,height=2,pointsize=8)
print(fig)
dev.off()


fig2 <- ggplot(data)+geom_point(aes(x=mhat,y=ameas.ms2))+
     ylim(0,5)+xlim(0,0.5)+
     geom_abline(slope=8.95,color='blue')+
     xlab('$\\frac{m_2}{m_1+m_2+m_c}$')+
     ylab('$a$ (\\unit{\\meter\\per\\second\\squared})')+
     theme_bw(base_size=8)
svglite('fig3.svg',width=3,height=2,pointsize=8)
print(fig2)
dev.off()

library(xtable)
results <- summarize(
	mean.t = mean(t.s),
	sd.t = sd(t.s),
	mean.a = mean(ameas.ms2),
	sd.a = sd(ameas.ms2),
	group_by(data,m2.kg)
	)
print(xtable(results),include.rownames=FALSE,file='table1raw.tex')



fit <- lm(ameas.ms2~mhat-1,data)
summary(fit)
