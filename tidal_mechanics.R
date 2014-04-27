# IMPORTANT: Anything starting with a hashmark ("#") is a "Comment"
# These comments are not used by the computer, so you can write in plain English (or French, or Dothraki!)
# Comments are intended to help humans understand what the script is supposed to do

# You will want to start every script file with the set of functions below,
# It basically wipes R console of any objects prior to working with new data.

rm(list=ls())          # REMEMBER THIS! Also, you can put comments in after each command too!

# to run the above command, you can copy/paste that line into the console below
# or you can hit "Ctrl+Enter" and RStudio will run your selection

# this might only work with RStudio (not in regular R?), but you can mark off different sections like this:
###################### New Section Title Here #################################
# and you can minimize the content in this section by clicking on the small triangle (far left)
# this is very useful when you write long scripts 

###################### model 1: simple tide model - the effect of the moon (M2) #################################

### set M2 model parameters - the effect of the moon (M2) ###
period = 12.41667                   # the tidal period in hours (period for M2 is 12.41667 h)
max_height = 5                      # the maximum tidal height
omega = 2*pi/period                 # the angular frequency (R already knows the value of pi)


# Look at the Environment tab (top right),
# Do you see the new objects (period, max_height, omega) you just created?
# Re-run the rm(list=ls()) command, see how that removed all the objects?
# Remember to re-run the parameter setting lines before continuing


### create data frame ###
t = c(0:720)                      # create vector for time 0 to 720 h
heightM2 = rep(0,length(t))       # create an empty vector (full of zeros) for tidal height, we will calculate these in the next step

# If you need to find help on rep(), you can go right to the Help tab (bottom right) and type it in
# or you can type ?rep() or ??rep() into the console (bottom left) to get help on the rep() function
# Finally, the most convenient way to get help is simply to left click on the function once and hit F1

matrix = cbind(t = t, heightM2 = heightM2)
dataframe = as.data.frame(matrix)
View(dataframe)                   # or click on the object "dataframe" in the Environment tab (top right)

# Did you notice that the "matrix" and the "dataframe" look the same when you View() them?
# They are similar, but you can interact with them differently

# to call a particluar value in the matrix, you can use the square brackets:
matrix[1,2]        # calls the 1st row of the 2nd column
matrix[1:10,2]     # calls the 1st to 10th rows of the 2nd column

# you can also to this with dataframes
dataframe[1,2]        # calls the 1st row of the 2nd column
dataframe[1:10,2]     # calls the 1st to 10th rows of the 2nd column

# but when you have a large data frame, it's hard to remember which column represents that particular variable
dataframe$heightM2[1] # calls the 1st row of the 2nd column (which is the variable "heightM2")

# PROTIP: type in "dataframe$", then hit "Tab". RStudio will bring up a list of possible variables
# you can also type in the first few letters of your variable (after the $ sign), then hit "Tab"
# this can autocomplete the variable name or narrows the list of possibilities. Huge time-saver!

### calculate tidal height
# "for loops" are amazingly powerful, in this case we are calculating the M2 tidal height for each row
# see help section for for(), you need to understand what's going on here before going ahead
for(h in t){
  row_num=h+1
  dataframe$heightM2[row_num]=max_height*sin(omega*t[row_num])
}

# PROTIP: for loops are powerful, but not computationally efficient
# the "apply" family of functions are even better, particularly for large data sets


# plot your results
plot(dataframe$t,dataframe$heightM2)                          # basic plot
# the default plot is hard to read
plot(dataframe$t,dataframe$heightM2,"l")                      # line plot
# the line plot has too much data for now
plot(dataframe$t,dataframe$heightM2,"l",xlim=c(0,48))         # line plot with limits on the x-axis


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
plot(dataframe$t,dataframe$heightM2,"l",xlim=c(0,48))
lines(dataframe$t,dataframe$heightS2,lty="dashed")     # you can add extra lines to your plots
legend(43,5,c("M2","S2"),lty=c("solid","dashed"))      # you can also add a legend

###################### model 3: multi-component tide model - M2+S2 ################################

### calculate tidal height
dataframe$heightM2S2=dataframe$heightM2+dataframe$heightS2

# plot your results
plot(dataframe$t,dataframe$heightM2S2,"l",col="purple")
text(175,0,"NEAP")                                     # you can even add text
text(350,0,"SPRING")
text(525,0,"NEAP")
text(700,0,"SPRING")

lines(dataframe$t,dataframe$heightM2,col="blue")
lines(dataframe$t,dataframe$heightS2,col="red")
legend("topright",c("M2S2","M2","S2"),lty=c("solid","solid","solid"),col=c("purple","blue","red"),horiz=T)
