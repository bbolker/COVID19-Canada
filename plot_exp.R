library(ggplot2);theme_set(theme_bw())
library(directlabels)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(colorspace)
library(metR) ## contour labels (better choice?)

L <- load(".clean.RData")


dd_long <- (ddclean
    ## FIXME: fragile, depends on order
    %>% select(Province, Date, bestTotal:newConfirmations,
               bestTotal:prop)
    %>% tidyr::pivot_longer(-c(Date,Province,prop))
    %>% group_by(Province,name)
    ## FIXME: do we still want all the numbers?
    %>% mutate(lab=paste0(Province,": ",round(tail(value,1))))
    %>% ungroup()
)

lab_data <- (dd_long
    %>% group_by(Province,name)
    %>% summarise(Date=max(Date),
                  lab=tail(lab,1))
)

dm <- list(dl.trans(x=x+0.2),cex=0.5,last.bumpup)
## http://directlabels.r-forge.r-project.org/examples.html
## n.b. can't mess with x aesthetic in geom_dl
(ggplot(dd_long, aes(x=Date, y=value, color=Province))
    + scale_colour_discrete_qualitative()
    + scale_y_continuous(trans="log2",expand=expansion(mult=0.1,add=1))
    + scale_x_date()
    + geom_dl(aes(label=lab),method=dm)
    + expand_limits(x=max(dd_long$Date)+12)
    + geom_line()
    + theme(legend.position = "none", axis.title.y=element_blank()
            ## , plot.title = element_text(vjust=-10,hjust=0.1,size=10)
            )
    + facet_wrap(~name,scale="free_y")
)

ggsave("plot_exp.png",width=8,height=4)


dd_tests <- (ddclean
	%>% select(Date, Province, newTests, newConfirmations, prop)
)

print(didnotupdate <- (dd_tests
	%>% filter(is.na(newTests))
)
)

dd_tests <- dd_tests %>% filter(newTests>0)



ddslopes <- data.frame(x=c(1,1,1,1)
	, y = c(0.02,0.05,0.1,0.15)
	, xend = c(10000, 10000, 1000/0.1, 1000/0.15)
	, yend = c(200,500,1000,1000)
)

print(ddslopes)


lseq <- function(lmax) {
    10^seq(0,lmax,length=31)
}
propdat <- (expand.grid(newConfirmations=lseq(3),newTests=lseq(4))
    %>% mutate(prop=newConfirmations/newTests)
)

big_prov <- (dd_tests
    %>% group_by(Province)
    %>% summarise_if(is.numeric,max,na.rm=TRUE)
    %>% arrange(newTests)
    %>% filter(newTests>1000)
    %>% pull(Province)
)
dd_big <- filter(dd_tests, Province %in% big_prov)
c_brks <- c(0.005,0.01,0.05,0.1)
## https://stackoverflow.com/questions/41324489/smoothing-continuous-2d-points
sdf <- 6
dd_smooth <- (dd_big
    %>% na.omit()
    %>% select(-prop)
    %>% group_by(Province)
    %>% mutate(t=as.numeric(Date-min(Date)),
               newTests=smooth.spline(t, newTests, df = sdf)$y,
               newConfirmations=smooth.spline(t, newConfirmations, df = sdf)$y
               )
)
## ggplot(dd_smooth,aes(newTests,newConfirmations,colour=Province))+ geom_path()
test_plot <- (ggplot(dd_big, aes(x=newTests,y=newConfirmations,
                                   colour=Province))
    + geom_point(alpha=0.5, aes(shape=Province))
    + geom_path(data=dd_smooth,lwd=1.5,alpha=0.8)
    ## can't squish or we'll mess up contour calc
    + scale_x_log10(limits=c(10,NA))
    + scale_y_log10(limits=c(1,NA),oob=scales::squish)
    + geom_contour(data=propdat,aes(z=prop),breaks=c_brks,colour="gray")
    ## can we adjust placement/get fewer labels per contour?
    + geom_text_contour(data=propdat, aes(z=prop),breaks=c_brks,colour="blue")
    + geom_dl(method="smart.grid",aes(label=Province))
    + scale_color_discrete_qualitative()
    + theme(legend.position="none")
)
print(test_plot)
ggsave(test_plot, file="test_plot.pdf")
## https://stackoverflow.com/questions/41324489/smoothing-continuous-2d-points

##     + geom_segment(data=ddslopes,aes(x=x,y=y,xend=xend,yend=yend,color=y))
##     + geom_text(data=ddslopes,aes(x=x,y=y,label=y,color=y),hjust=0.5)
##     ## + geom_text(aes(y=newConfirmations,label=Province),vjust=-0.5,hjust=-0.2)
##     + xlim(c(1,7000))
##     + ylim(c(0.01,800))
##     + scale_x_log10() ## breaks=c(1,dd_tests$newTests[-nrow(dd_tests)], 10000))
##     + scale_y_log10() ## breaks=c(0.001,dd_tests$newConfirmations))
##     + theme(axis.text.x = element_text(angle = 65,vjust=0.65,hjust=1)
##           , panel.grid.minor = element_blank())
##     + scale_colour_gradient(low = "blue", high = "red", na.value = NA)
##     + labs(x= "New Tests", y = "New Confirmations")

## print(test_plot)
