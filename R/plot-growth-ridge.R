



plot_growth_ridges<-function()
{
  
  n  <- length(M)
  mdf <- NULL
  if(length(M[[1]]$growth_matrix_1_1)>0)
  {
    for(i in 1:n)
    {
      A <- M[[i]]
      g_mat<-A$growth_matrix_1_1
      rownames(g_mat)<-A$mid_points
      colnames(g_mat)<-A$mid_points
      in_g<-data.frame(melt(g_mat))
      colnames(in_g)<-c("Postmolt","Premolt","Density")
    
      df <- data.frame(Model = names(M)[i],
                       Sex   = as.factor(A$iMoltIncSex),
                       molt_inc   = A$dMoltInc,
                       premolt  = A$dPreMoltSize,
                       postmolt = A$dPreMoltSize + A$dMoltInc,
                       type = 'obs')
      
      p <- ggplot(in_g)
      p <- p + geom_density_ridges(aes(x=Postmolt, y=Premolt, height = Density, group=Premolt,
                                       fill=stat(y),alpha=.9999),stat = "identity",scale=3) +
        scale_fill_viridis_c()+
        .THEME +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90)) +
        geom_point(data=df,aes(y=premolt,x=postmolt))
      
      print(p)
      
    }
  
  }
}
  