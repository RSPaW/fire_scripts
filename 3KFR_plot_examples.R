## code below creates all of the plots for the work shown
## to Ben and Ian as of the 21st July 2021. Add to this if further plots are
## to be created.

## Bart Huntley 21 July 2021

library(tidyverse)

df <- read_csv("./output_stats/burnt_area_stats.csv")


pks <- unique(df$park)
v <- unique(df$vegetype)
brks <- unique(df$year)


for(i in seq_along(pks)){
  dat <- filter(df, park == pks[i] & season != "cal_yr")
  pk <- pks[i]
  for(j in seq_along(v)){
     pdf <- filter(dat, vegetype == v[j])
     vg <- v[j]
     if(vg == "Eucalypt"){
       p <- ggplot(pdf) +
         geom_col(aes(year, burnt_area_ha, fill = season, colour = season),
                  position = position_dodge()) +
         scale_colour_manual(values = c("black", "black")) +
         scale_fill_manual(values = c("olivedrab1", "olivedrab4")) +
         scale_x_continuous(name = "", breaks = brks) +
         theme_bw() +
         theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
         labs(title = paste0(pk, " Area Burnt - ", vg),
              subtitle = "Annual area burnt by season",
              caption = "Source data: NAFI",
              y = "Hectares (ha)")
       pk <- str_split(pk, " ")[[1]][1]
       pname <- paste0("./output_stats/", pk, "_", vg, ".png")
       ggsave(filename = pname, p)
     } else {
       p <- ggplot(pdf) +
         geom_col(aes(year, burnt_area_ha, fill = season, colour = season),
                  position = position_dodge()) +
         scale_colour_manual(values = c("black", "black")) +
         scale_fill_manual(values = c("khaki2", "khaki4") ) +
         scale_x_continuous(name = "", breaks = brks) +
         theme_bw() +
         theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
         labs(title = paste0(pk, " Area Burnt - ", vg),
              subtitle = "Annual area burnt by season",
              caption = "Source data: NAFI",
              y = "Hectares (ha)")
       pk <- str_split(pk, " ")[[1]][1]
       pname <- paste0("./output_stats/", pk, "_", vg, ".png")
       ggsave(filename = pname, p)
     }
     
  }
}


# proportion
for(i in seq_along(pks)){
  dat <- filter(df, park == pks[i] & season != "cal_yr")
  pk <- pks[i]
  for(j in seq_along(v)){
    pdf <- filter(dat, vegetype == v[j])
    vg <- v[j]
    if(vg == "Eucalypt"){
      p <- ggplot(pdf) +
        geom_col(aes(year, burnt_area_prop, fill = season, colour = season),
                 position = position_dodge()) +
        scale_colour_manual(values = c("black", "black")) +
        scale_fill_manual(values = c("olivedrab1", "olivedrab4")) +
        scale_x_continuous(name = "", breaks = brks) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
        labs(title = paste0(pk, " Area Burnt - ", vg),
             subtitle = "Annual proportion burnt by season",
             caption = "Source data: NAFI",
             y = "Proportion (%)")
      pk <- str_split(pk, " ")[[1]][1]
      pname <- paste0("./output_stats/", pk, "_", vg, "_prop.png")
      ggsave(filename = pname, p)
    } else {
      p <- ggplot(pdf) +
        geom_col(aes(year, burnt_area_prop, fill = season, colour = season),
                 position = position_dodge()) +
        scale_colour_manual(values = c("black", "black")) +
        scale_fill_manual(values = c("khaki2", "khaki4") ) +
        scale_x_continuous(name = "", breaks = brks) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
        labs(title = paste0(pk, " Area Burnt - ", vg),
             subtitle = "Annual proportion burnt by season",
             caption = "Source data: NAFI",
             y = "Proportion (%)")
      pk <- str_split(pk, " ")[[1]][1]
      pname <- paste0("./output_stats/", pk, "_", vg, "_prop.png")
      ggsave(filename = pname, p)
    }
    
  }
}
