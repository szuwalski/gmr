#' Plot predicted spawning stock biomass (ssb)
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M List object(s) created by read_admb function
#' @param xlab the x-label of the figure
#' @param ylab the y-label of the figure
#' @param ylim is the upper limit of the figure
#' @param alpha the opacity of the ribbon
#' @return Plot of model estimates of spawning stock biomass 
#' @export
#' 
plot_kobe <- function(M, ylab = "Fishing mortality", xlab = "SSB (tonnes)", ylim = NULL, alpha = 0.1,
                      sex="Male",fleet_in="Pot",ref_ind=1)
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  
  mdf <- .get_ssb_df(M)
  mdf_1 <- .get_F_df(M)
  
  use_F<-filter(mdf_1$F,fleet==fleet_in)
  in_plot<-merge(use_F,mdf)
  
  p<-ggplot(data=in_plot,aes(x=ssb,y=F,col=Model))+
    geom_path() +
    geom_text(data=in_plot,aes(x=ssb,y=F,label=year,col=Model),size=2)+
    .THEME+
    expand_limits(x = 0, y = 0) +
    geom_hline(yintercept=M[[ref_ind]]$sd_fmsy[1],linetype='dashed') +
    geom_vline(xintercept=M[[ref_ind]]$spr_bmsy,linetype='dashed')
  print(p)
 
}
