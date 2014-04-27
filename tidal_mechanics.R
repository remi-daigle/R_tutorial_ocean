# TODO: Add instructions


### set M2 model parameters - the effect of the moon (M2) ###
period = 12.41667                   # the tidal period in hours (period for M2 is 12.41667 h)
max_height = 5                      # the maximum tidal height
omega = 2*pi/period                 # the angular frequency

# TODO: explain help tab and F1


### create data frame ###
t = c(0:720)                      # create vector for time 0 to 720 h
heightM2 = rep(0,length(t))       # create an empty vector (full of zeros) for tidal height, we will calculate these in the next step

matrix = cbind(t = t, heightM2 = heightM2)
dataframe = as.data.frame(matrix)
View(dataframe)                   # or click on the object "dataframe" in the Environment tab (top right)

# TODO: explain $ and []


###################### model 1: simple tide model - the effect of the moon (M2) #################################
for(h in t){
  index=h+1
  dataframe$heightM2[index]=max_height*sin(omega*t[index])
}


# plot your results
plot(dataframe$t,dataframe$heightM2)             # basic plot
plot(dataframe$t,dataframe$heightM2,"l")         # line plot
plot(dataframe$t,dataframe$heightM2,"l",xlim=c(0,24))         # line plot with limits on the x-axis


###################### model 2: simple tide model - the effect of the sun (S2) ################################

### set S2 model parameters - the effect of the sun (S2)###
period = 12                         # the tidal period in hours (period for S2 is 12 h)
max_height = 2                      # the maximum tidal height
omega = 2*pi/period                 # the angular frequency

dataframe$heightS2=0                # add an empty (full of zeros) variable to the data frame

### calculate tidal height
for(h in t){
  index=h+1
  dataframe$heightS2[index]=max_height*sin(omega*t[index])
}


# plot your results
plot(dataframe$t,dataframe$heightS2,"l",xlim=c(0,48))

plot(dataframe$t,dataframe$heightM2,"l",xlim=c(0,48))
lines(dataframe$t,dataframe$heightS2,lty="dashed")
legend(43,5,c("M2","S2"),lty=c("solid","dashed"))

###################### model 3: multi-component tide model - M2+S2 ################################

### calculate tidal height
dataframe$heightM2S2=dataframe$heightM2+dataframe$heightS2

# plot your results
plot(dataframe$t,dataframe$heightM2S2,"l")
lines(dataframe$t,dataframe$heightM2,lty="dashed")
lines(dataframe$t,dataframe$heightS2,lty="dotted")
legend(450,7,c("M2S2","M2","S2"),lty=c("solid","dashed","dotted"),horiz=T)

# TODO: axis legends, point out spring/neap tides with text

