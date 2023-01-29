#   ┌──────────────────────────────────┐
#   │                                  │▒
#   │ Implicit/Explicit Water Analysis │▒
#   │                                  │▒
#   └──────────────────────────────────┘▒
#    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒


# This script runs through the analysis of implicit/explicit models for water distribution, in the IBM simulator
# 
# It's steps are as follow:
# 0) load required functions and libraries
# 1) read the data files
# 2) stand-level characteristics comparative analysis
# 3) full-hectare simulation analyses
# 4) Water need/consumption analysis


#  ╔════════════════════════════════════════════════╗
#  ║ STEP 0 - load required functions and libraries ║
#  ╚════════════════════════════════════════════════╝

# If needed, install with:
install.packages('wordcloud')
install.packages('Hmisc')
#install.packages('wordcloud', repos="http://cran.us.r-project.org")
#install.packages('Hmisc', repos="http://cran.us.r-project.org")

# Source helper functions
source('Functions.R')

# Also, set an important variable: should we output to pdf?
to_pdf = TRUE

#  ╔══════════════════════════════╗
#  ║ STEP 1 - read the data files ║
#  ╚══════════════════════════════╝

# A note on the data files format
# -------------------------------
# The first columns are self-explanatory:
# COLUMNS: YEAR POP AGE BA STI_NB STI_BA ASSYM CANOPY_RATIO UNDER_NB OVER_NB UNDER_BA OVER_BA UNDER_AGE OVER_AGE 
# 
# => water consumption is logged as WATER_CONSUMED (the water really taken by the tree) and WATER_NEEDED (water level required to not incur increased death probability)
# COLUMNS: WATER_CONSUMED WATER_NEEDED 
#
# => the water consumption statistics are detailed for 5 classes of diameters: A <=> less than 50 cm ; B <=> 50cm-1m ; C <=>1-1.5m ; D <=> 1.5-2m ; E <=> 2+ meters
# COLUMNS: WATER_CONSUMED_A WATER_CONSUMED_B WATER_CONSUMED_C WATER_CONSUMED_D WATER_CONSUMED_E WATER_NEEDED_A WATER_NEEDED_B WATER_NEEDED_C WATER_NEEDED_D WATER_NEEDED_E POP_A POP_B POP_C POP_D POP_E
#
# => In addition, tolerant and intolerant trees are logged separately:
# COLUMNS WATER_CONSUMED_INTOLERANT WATER_NEEDED_INTOLERANT WATER_CONSUMED_TOLERANT WATER_NEEDED_TOLERANT POP_TOLERANT POP_INTOLERANT

# Initialize the data
water_1 = list() # explicit
water_2 = list() # implicit

for (availability in seq(0.3,6,0.3)) {
  a = as.character(availability)
  if (nchar(a)==1) {
    a = paste0(a, '.0')
  }
  
  # Old version, without water needed logging
  # water_1[[a]] = read.csv(paste0('data/water_explicit_0.2_',a,'Data/Stats.csv'), sep='\t')
  # water_2[[a]] = read.csv(paste0('data/water_implicit_0.2_',a,'Data/Stats.csv'), sep='\t')
  
  water_1[[a]] = read.csv(paste0('data/water_explicit_bis_0.2_',a,'Data/Stats.csv'), sep='\t')
  water_2[[a]] = read.csv(paste0('data/water_implicit_bis_0.2_',a,'Data/Stats.csv'), sep='\t')

  # Also store the water available (constant in time):
  water_available = as.numeric(a) /6

  water_1[[a]]$WATER_NEEDED = water_1[[a]]$WATER_NEEDED_A + water_1[[a]]$WATER_NEEDED_B + water_1[[a]]$WATER_NEEDED_C + water_1[[a]]$WATER_NEEDED_D + water_1[[a]]$WATER_NEEDED_E
  water_2[[a]]$WATER_NEEDED = water_2[[a]]$WATER_NEEDED_A + water_2[[a]]$WATER_NEEDED_B + water_2[[a]]$WATER_NEEDED_C + water_2[[a]]$WATER_NEEDED_D + water_2[[a]]$WATER_NEEDED_E
  water_1[[a]]$WATER_CONSUMED = water_1[[a]]$WATER_CONSUMED_A + water_1[[a]]$WATER_CONSUMED_B + water_1[[a]]$WATER_CONSUMED_C + water_1[[a]]$WATER_CONSUMED_D + water_1[[a]]$WATER_CONSUMED_E
  water_2[[a]]$WATER_CONSUMED = water_2[[a]]$WATER_CONSUMED_A + water_2[[a]]$WATER_CONSUMED_B + water_2[[a]]$WATER_CONSUMED_C + water_2[[a]]$WATER_CONSUMED_D + water_2[[a]]$WATER_CONSUMED_E
  
  # And compute the absolute water consumed/needed:
  water_1[[a]]$WATER_CONSUMED_ABS = water_1[[a]]$WATER_CONSUMED * water_available
  water_2[[a]]$WATER_CONSUMED_ABS = water_2[[a]]$WATER_CONSUMED * water_available
  water_1[[a]]$WATER_NEEDED_ABS = water_1[[a]]$WATER_NEEDED * water_available
  water_2[[a]]$WATER_NEEDED_ABS = water_2[[a]]$WATER_NEEDED * water_available
}

#  ╔═══════════════════════════════════════════════════════════╗
#  ║ STEP 2 - stand-level characteristics comparative analysis ║
#  ╚═══════════════════════════════════════════════════════════╝

# Show the characteristics obtained with different water availability with scatterplots.
# In these graphs, red dots indicate the available water, form 0.05 (very xeric) to 1 (very mesic/hydric)

if (to_pdf)
  pdf('./PDFs/implicit_explicit.pdf', width=12, height = 16)
par(mfrow=c(4,3))
for (v in c("POP", "AGE", "BA", "STI_BA", "ASSYM", "CANOPY_RATIO", "UNDER_NB", "OVER_NB", "UNDER_BA", "OVER_BA", "UNDER_AGE", "OVER_AGE")) {
  val = sapply(names(water_1), function(a) {
    n = min(nrow(water_1[[a]]), nrow(water_2[[a]]))
    # n = 5000
    m1 = mean(water_1[[a]][500:n,v], na.rm=T)
    m2 = mean(water_2[[a]][500:n,v], na.rm=T)
    # cat(paste0(a, ':', ))
    return(c(m1,m2))
  })
  lim = range(pretty(c(min(val[])*0.75, max(val[])*1.25)))
  textplot(val[1,], val[2,], as.numeric(names(water_1))/6, show.lines=T, xlab='Explicit Root System', ylab='Implicit Root System', main = v, xlim=lim, ylim=lim, xaxs='i', yaxs='i', las=1, dir=if(v=='ASSYM'){1}else{-1} )
  points(lim[1:2], lim[1:2], type='l', lty=2)
}
if (to_pdf)
  dev.off()


# This shows the traces overlayed for implicit/explicit water model

if (to_pdf)
  pdf('./PDFs/implicit.pdf', width=12, height = 16/4)
par(mfrow=c(1,3))
require('Hmisc')
for (v in c('POP','AGE',"BA")) {
  # Use the line below to output all the variables
  # for (v in c("POP", "AGE", "BA", "WATER_CONSUMED","WATER_NEEDED","WATER_CONSUMED_ABS","WATER_NEEDED_ABS","STI_BA", "ASSYM", "CANOPY_RATIO", "UNDER_BA", "OVER_BA", "UNDER_AGE", "OVER_AGE")) {
  val = sapply(names(water_1), function(a) {
    n = min(nrow(water_1[[a]]), nrow(water_2[[a]]))
    n_start = 500
    m1 = mean(water_1[[a]][n_start:n,v], na.rm=T)
    m2 = mean(water_2[[a]][n_start:n,v], na.rm=T)
    s1 = sd(water_1[[a]][n_start:n,v], na.rm=T) #/ sqrt(n - n_start)
    s2 = sd(water_2[[a]][n_start:n,v], na.rm=T) #/ sqrt(n - n_start)
    return(c(m1, m2, s1, s2))
  })
  ploti = 1:20
  plot(1,1, type='n', xlab='Aridity', ylab=v, pch=2, col='green', lwd=2, las=1, ylim=range(val[]), xlim=c(0,1))
  points(as.numeric(names(water_1))/6, val[1,], pch=2, col='green', lwd=2)
  points(as.numeric(names(water_2))/6, val[2,], pch=2, col='blue', lwd=2)
  # Add some error bars:
  errbar(as.numeric(names(water_2))/6, val[2,], val[2,]-val[4,], val[2,]+val[4,], errbar.col = 'blue', add=T, pch=NA)
  errbar(as.numeric(names(water_1))/6, val[1,], val[1,]-val[3,], val[1,]+val[3,], errbar.col = 'green', add=T, pch=NA)
  if (v %in% c("WATER_CONSUMED","WATER_NEEDED"))
    legend('right', fill=c('green', 'blue'), legend = c('Explicit', 'Implicit'),bty='n')
  else
    legend('bottomright', fill=c('green', 'blue'), legend = c('Explicit', 'Implicit'))
  mtext(side=3, font=2, line=1, v)
  
  if (v %in% c('POP','AGE',"BA")) {
    ploti = 7
    abline(a=0, b=mean(val[2,1:ploti]/(as.numeric(names(water_1))[1:ploti]/6)), col='blue', lwd=2)
    abline(a=mean(val[2,(ploti+1):20]), b=0, col='blue', lwd=2)
  }
}
if (to_pdf)
  dev.off()




#  ╔═══════════════════════════════════════════╗
#  ║ STEP 3 - full-hectare simulation analyses ║
#  ╚═══════════════════════════════════════════╝

# Full-hectare simulations were obtained with a water level of 0.3 (on the aridity scale from 0 to 1, this corresponds to the setting WATER_AVAILABLE=1.8)

explicit = read.csv('data/explicit_hiresData/Stats.csv', sep='\t')
implicit = read.csv('data/implicit_hiresData/Stats.csv', sep='\t')
shown_range = 1:5000

if (to_pdf)
  pdf('./PDFs/implicit_BA.pdf', width=8, height = 6)
par(mfrow=c(1,1))
explicit_ba = explicit$BA[shown_range]
implicit_ba = implicit$BA[shown_range]
plot(implicit_ba, type='l', lwd=2, col='blue', xlab='Years', ylab="BA", bty='n',las=1)
points(explicit_ba, type='l', lwd=2, col='green')
abline(v=100, lty=2, lwd=1.5)
abline(v=1000, lty=2, lwd=1.5)
if (to_pdf)
  dev.off()


if (to_pdf)
  pdf('./PDFs/implicit_STI.pdf', width=8, height = 6)
explicit_stiba = explicit$STI_BA[shown_range]
implicit_stiba = implicit$STI_BA[shown_range]
plot(implicit_stiba, type='l', lwd=2, col='blue', xlab='Years', ylab="Shade Tolerance Index", bty='n',las=1)
points(explicit_stiba, type='l', lwd=2, col='green')
abline(v=100, lty=2, lwd=1.5)
abline(v=1000, lty=2, lwd=1.5)
if (to_pdf)
  dev.off()


if (to_pdf)
  pdf('./PDFs/implicit_AGE.pdf', width=8, height = 6)
explicit_age = explicit$AGE[shown_range]
implicit_age = implicit$AGE[shown_range]
plot(implicit_age, type='l', lwd=2, col='blue', xlab='Years', ylab="Mean Age", bty='n',las=1)
points(explicit_age, type='l', lwd=2, col='green')
abline(v=100, lty=2, lwd=1.5)
abline(v=1000, lty=2, lwd=1.5)
if (to_pdf)
  dev.off()



#  ╔══════════════════════════════════════════╗
#  ║ STEP 4 - Water need/consumption analysis ║
#  ╚══════════════════════════════════════════╝

# Analysis of the water consumption

human_name=list()
human_name[['WATER_CONSUMED']] = 'Water consumed by trees (relative to the water available)'
human_name[['WATER_NEEDED']] = 'Relative water needed by trees  (relative to the water available)'
human_name[['WATER_CONSUMED_ABS']] = 'Water consumed by trees'
human_name[['WATER_NEEDED_ABS']] = 'Water needed by trees'

if (to_pdf)
  pdf('./PDFs/water_consumption.pdf', width=9, height = 6)
par(mfrow=c(2,2),pty='s')
require('Hmisc')
for (v in c("WATER_CONSUMED_ABS","WATER_NEEDED_ABS","WATER_CONSUMED","WATER_NEEDED")) {
  val = sapply(names(water_1), function(a) {
    n = min(nrow(water_1[[a]]), nrow(water_2[[a]]))
    # n = 2000
    n_start = 500
    m1 = mean(water_1[[a]][n_start:n,v], na.rm=T)
    m2 = mean(water_2[[a]][n_start:n,v], na.rm=T)
    s1 = sd(water_1[[a]][n_start:n,v], na.rm=T) #/ sqrt(n - n_start)
    s2 = sd(water_2[[a]][n_start:n,v], na.rm=T) #/ sqrt(n - n_start)
    return(c(m1, m2, s1, s2))
  })
  ploti = 1:20
  plot(1,1, type='n', xlab='Aridity',ylab='', pch=2, col='green', lwd=2, las=1, xlim=c(0,1),ylim=c(0,1),asp=1)
  points(as.numeric(names(water_1))/6, val[1,], pch=2, col='green', lwd=2)
  points(as.numeric(names(water_2))/6, val[2,], pch=2, col='blue', lwd=2)
  # Add some error bars:
  errbar(as.numeric(names(water_2))/6, val[2,], val[2,]-val[4,], val[2,]+val[4,], errbar.col = 'blue', add=T, pch=NA)
  errbar(as.numeric(names(water_1))/6, val[1,], val[1,]-val[3,], val[1,]+val[3,], errbar.col = 'green', add=T, pch=NA)
  legend('topleft', fill=c('green', 'blue'), legend = c('Explicit', 'Implicit'),bty='n')
  mtext(side=3, font=2, line=1, human_name[[v]])
  
  if (v %in% c('WATER_CONSUMED','WATER_NEEDED')) {
    abline(h=1, lty=2)
    text(0.5,1.05,'Water available')
  } else {
    abline(0,1, lty=2)
    text(0.5,0.55,'Water available',srt=45)
  }
}
if (to_pdf)
  dev.off()




