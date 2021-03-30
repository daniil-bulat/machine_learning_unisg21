######################################################
# ANIMATION FOR TWO-DIMENSIONAL PERCEPTRON LEARNING  #
######################################################


# README
########

# This script illustrates the iterative process of learning membersip of 
# a data point to a binary class (e.g. which of two iris flowers, patient 
# suffering from a desease or not). Learning is illustrated with an animation
# using the animation package. The output is an html file that shows the animation.
# The code allows for various data sets. These are prepared for the analysis in
# Section 1. You can add your own data if you like.


# Header
########

## PLEASE ADJUST YOUR WORKING DIRECTORY
rm(list = ls())
#mainDir = "D:\\OneDrive\\Weitere Kurse\\Machine Learning und KI\\Scripts Base Topics"
mainDir = "C:\\Users\\ahorlemann\\Dropbox\\Unizeugs\\Machine Learning\\R Skripte"
setwd(mainDir)

library(animation)
library(scales)
library(stringr)


###############################################
# Section 1: Set parameters and Prepare data  #
###############################################


# Get the data, this script allows you to visualize and animate learning
# for different data sets. Data sets are listed in dataSets and referred 
# to by the variable dataLabel

dataSets = c("iris", "WDBC cancer")


dataLabel = "iris"
#dataLabel = "WDBC cancer"





# Parameters for data representation and learning

standardize = TRUE  # whether to standardize data with standFun() below

eta = 0.01   # The learning rate

nTrain = 30   # Number of observations used for training






# Data preparations that are specific to each data set

  # For the iris data set

    if (dataLabel == "iris" ){
      Data = iris
      # Select only 2 predictors and drop virginica
      Data = Data[names(Data)[c(1,3,5)]]
      Data = Data[1:100, ]
      
      Data$Species = droplevels(Data$Species)
      levels(Data$Species)
      
      Data$Label = ifelse(Data$Species == "setosa", -1, 1)
      predictors = names(Data)[1:2]
      targetVar = "Species"
      # legendLabs = c("Setosa", "Versicolor")
      
      negLab = as.character(unique(Data$Species[Data$Label==-1]))
      posLab = as.character(unique(Data$Species[Data$Label==1]))
      legendLabs = c(negLab, posLab)
      
  # For the wdbc data set
  
    } else if (dataLabel == "WDBC cancer"){
      load("../Data/wdbcData.RData") 
      Data$Label = ifelse(Data$diagnosis == "B", -1, 1)
      predictors = c(names(Data)[5], names(Data)[10])
      targetVar = "diagnosis"
      #legendLabs = c("B", "M")
      negLab = as.character(unique(Data$diagnosis[Data$Label==-1]))
      posLab = as.character(unique(Data$diagnosis[Data$Label==1]))
      legendLabs = c(negLab, posLab)
      
      
      keepList = c(predictors, "diagnosis", "Label")
      Data = Data[keepList]
    }

# Colors for plotting
posCol = "red"     # color for data points with label +1 
negCol = "blue"    # color for data points with label -1 in data
posColbg = "#f79999"  # Background color for area predicted +1
negColbg = "#9ed9f7"  # Background color for area predicted -1




# reshuffle dataset; some data sets, such as iris are ordered by target. 
# This would make learning extremely slow

set.seed(12)
resh = sample(1:nrow(Data))
Data = Data[resh,]

# Standardize data, if standardize == TRUE
# Again, this speeds up learning, or enables it at all
standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

if (standardize == TRUE){
  Data[[predictors[1]]] = standFun(Data[[predictors[1]]])
  Data[[predictors[2]]] = standFun(Data[[predictors[2]]])
}





##########################################################
# Section 2: Perceptron Learning Algorithm and Animation #
##########################################################

# Copying data to new object
D = Data
n = nrow(Data)

# Throw error if number of training observations are larger than number of 
# observations
if (nTrain>n) {stop(paste0(
  "Desired training observations (", nTrain, 
  ") exceed number of observations (", n,
  "). Please choose a number of training observations smaller than ", n, ".")) }

# All sorts of initializations
##############################

# The initial weights, initialized to 0 (= completely ignorant)
w = rep(0, length(predictors) + 1)

# Plotting preparations... 
#Get xlims and ylims from an "empty" plot
plot(Data[[predictors[1]]], Data[[predictors[2]]], type = "n")

xlims = par("usr")[1:2]; ylims = par("usr")[3:4]

# A grid of (x,y) combinations that is used for coloring prediction areas
x = seq(xlims[1], xlims[2], 0.01)
y = -w[1]/w[3] - w[2]/w[3]*x

xgrid =  seq(xlims[1], xlims[2], (xlims[2]-xlims[1])/220)
ygrid =  seq(ylims[1], ylims[2], (ylims[2]-ylims[1])/220)
nx = length(xgrid); ny = length(ygrid)

xgrid = rep(xgrid, times = ny)
ygrid = rep(ygrid, each = nx)
toPaint = data.frame(xgrid = xgrid, ygrid = ygrid)
# toPaint is the data frame that is used for coloring the prediction areas





# ANIMATION AND ITERATIVE LEARNING START HERE
#############################################

# Create a new directory inside current working directory that
# is named after dataLabel, but without blanks...
# We set this as new working directory, such that all
# folders from the animation go into that directory

animDirName = str_replace_all(dataLabel, " ", "_")

animDirName = paste0("anim_" , animDirName)

dir.create(animDirName, showWarnings = FALSE)

setwd(file.path(mainDir, animDirName))


 saveHTML({
   ani.options(interval = 1)
   # i = 1 # for testing
 for (i in 1:nTrain){ # Loop over all nTrain obs

   
     
   
    xi = as.numeric(D[predictors][i, ])  # The x data from obs i
    yi = D$Label[i]                      # The y data from obs i
    index = w[1] + w[2]*xi[1] + w[3]*xi[2]
      # The index (sum w_i x_i, with the first entry of w being the constant term or bias)
      # Dont'get confused about the weird indexing, R has no index 0.
    
    pred = ifelse(index>=0, 1, -1)  # The prediction
    
    wr = round(w, digits = 3) # rounded version of weights, for slides in right panel
    
    # General plotting setup
    
    par(mfrow = c(1,2))     # layout: Two subplots in horizontal order
    par(bg = c("#DADADA"))  # background color of overall plotting area (for all subplots)
    par(oma = c(1,2,2,2))   # outer margins
    
    
    
    
    
          # BEGIN PLOTTING LEFT PANEL
    
    
    
    # First the background colors for prediction areas
    # Remember that toPaint contains a grid through the entire area of the plot
    
    toPaint$index = w[1] + w[2]*xgrid + w[3]*ygrid
    
    toPaint$prediction = ifelse(toPaint$index >=0, 1, -1 )
    
    # First an empty plot as "canvas", to add the rest
    plot(Data[[predictors[1]]], Data[[predictors[2]]], type = "n",
         xlab = predictors[1], ylab = predictors[2], cex = 1)
    
    # Add packground coloring for areas that are predicted to be "positive"
    points(toPaint$xgrid[toPaint$prediction == 1], toPaint$ygrid[toPaint$prediction == 1],
           pch=c(16), col=posColbg, cex =1)
    
    # Add packground coloring for areas that are predicted to be "negative"
    points(toPaint$xgrid[toPaint$prediction == -1], toPaint$ygrid[toPaint$prediction == -1],
           pch=c(16), col=negColbg, cex =1)
    
    # The "positive" data points
    points(Data[[predictors[1]]][Data$Label == 1], Data[[predictors[2]]][Data$Label == 1], 
           pch=16, col=alpha(posCol, 1), cex = 1)
    
    # The "negative" data points
    points(Data[[predictors[1]]][Data$Label == -1], Data[[predictors[2]]][Data$Label == -1], 
           pch=16, col=alpha(negCol, 1),  cex = 1)
    
    # Make color case-dependent for the i-th data point that is being highlighted
    # as the i-th target
    col = ifelse(Data$Label[i] == 1, posCol, negCol)
    
    # Highlighting of i-th target
    points(Data[[predictors[1]]][i], Data[[predictors[2]]][i], 
           pch=16, col=col,  cex = 3)
    
    
    
    
    # horizontal line from target point to y-axis
    lines(x = c(xlims[1], Data[[predictors[1]]][i]),
         y = c(Data[[predictors[2]]][i], Data[[predictors[2]]][i]), 
         lwd = 1, lty = "dotted", col = col)
    
    # vertical line from target point to x-axis
    lines(x = c(Data[[predictors[1]]][i], Data[[predictors[1]]][i]),
         y = c(ylims[1], Data[[predictors[2]]][i]), 
         lwd = 1, lty = "dotted", col = col)
    
    # The decision border
    xx = seq(xlims[1], xlims[2], 0.01)
    yy = -w[1]/w[3] - w[2]/w[3]*xx
    lines(xx,yy, lty = 2, col = "black", lwd = 2)
    
    
    # Add text label for i-th target "Target (i = 'i' ")
    text(Data[[predictors[1]]][i], Data[[predictors[2]]][i],
         pos = 3, offset = 4, cex = 2,
         labels = paste0("Target (i=", i, ")"), col = "white", font = 2)
    
    # Adding an arrow from text label to data point
    ay = (ylims[2]-ylims[1])/14  # For length of arrow
    
    
    arrows(Data[[predictors[1]]][i], Data[[predictors[2]]][i] + ay,
           Data[[predictors[1]]][i], Data[[predictors[2]]][i] + 0.25*ay,
           col = "white", lwd = 3
    )
    
    
    
    # Add a legend
    
    legend("bottomright", legend=legendLabs, 
           pch=c(16,16), col = c(alpha(negCol, 0.3),alpha(posCol, 0.3)), 
           cex = 1, bg = "white")
   
          
    
          # END PLOTTING LEFT PANEL
    
    
    
    
    # Bookkeeping and updating of the LEARNING ALGORITHM
    
    # To check the current state of affairs (i.e. number of errors), we
    # need to make a prediction with the most recent (=last) value of the weights
    # This is done here.
    
    D$lastIndex = w[1] + w[2]*D[[predictors[1]]] + w[3]*D[[predictors[2]]]
    
    D$lastPrediction = ifelse(D$lastIndex >=0, 1, -1)
    
    D$lastError  = D$Label - D$lastPrediction
    
    # Number of misclassified cases
    misclas = round(sum(D$lastError!=0)/nrow(D)*100, digits = 2)
    
    # False positives
    x = ifelse(D$lastPrediction == 1 & D$Label == -1, 1, 0)
    falPos = round(sum(x)/length(D$Label[D$Label == -1])*100, digits = 1)
    
    # False negatives
    x = ifelse(D$lastPrediction == -1 & D$Label == 1, 1, 0)
    falNeg = round(sum(x)/length(D$Label[D$Label == 1])*100, digits = 1)
    
    
    # bookkeeping: Enter current values of index, prediction, and weights
    # into the D data frame (this is not really necessary, just for curiosity)
    
    
    D$index[i] = index
    D$prediction[i] = pred
    D$bias[i] = w[1]
    D$w_SLength[i] = w[2]
    D$w_PLength[i] = w[3]
    D$misclas[i] = misclas
    
    
    # THE FOLLOWING LINES ARE THE ENGINE OF THE WHOLE THING !!!
    # THIS IS WHERE THE LEARNING IS GOING ON!
    
    update = eta*(yi - pred)      # The updating factor
    w[-1] = w[-1] + update * xi   # updating the weights for the two predictors
    w[1] = w[1] + update          # updating the constant/bias
    
    
    # Again for bookkeeping (pretty self-explaining)
    
    error  = yi - pred  
    D$error[i] = error
    D$update[i] = update
    D$i = i
    
    
    
    # PLOT FOR RIGHT PANEL = SLIDES
    
    # This makes the "slides" in the right panel; it's somewhat messy, so you
    # probably best ignore it
    
    # Some useful web sources:
    
        # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/
        # http://stackoverflow.com/questions/18237134/line-break-in-expression
        # For uncommon math symbols, use octal symbols in symbol("\..."), 
        # e.g. symbol("\256") for rightarrow
        # Symbol list in https://www.stat.auckland.ac.nz/~paul/R/CM/AdobeSym.html
        
        # Round numbers that are shown on "slides" in the right panel
          ir = round(index, digits = 2)
          w0new = round(wr[1] + update, digits = 3)
          w1new = round(wr[2] + update*xi[1], digits = 3) 
          w2new = round(wr[3] + update*xi[2], digits = 3) 
          indexnew = round(w[1] + w[2]*xi[1] + w[3]*xi[2] , digits = 2) # updated value of index
            
          
          
          
         # Now follow the lines of text in the right panel. There are two types of 
         # texts: Texts that is constant, and text that is conditional.
          
         # We first define all the conditional lines of text and give them a new
         # Later, we "project" it on the pane.
          
          
          
          # Information about error, its consequences, new value of index
          # (all this depends on THREE possible values of 'error')
          
          
          if (error == 0){
            errtext1 = bquote( paste( "Error = ", .(error), " (correct prediction)"))
            errtext2 = bquote(paste(""))
            
            newindextext0 = bquote(paste(" New value of index: NONE"))
            newindextext1 = bquote(paste(""))
            #newindextext2 = bquote(paste(""))
            
          } else if (error > 0) {
            errtext1 = bquote( paste( "Error = target \055 prediction" == .(yi), 
                                      symbol(" \055 "), .(pred) == .(error), 
                                      " (index \"UNDERSHOOTS\")"))
            errtext2 = bquote( paste( "", symbol("\256"), 
                          " Adjust weights such that INDEX" ,symbol("\255")," for current i)" ))
            
            newindextext0 = bquote(paste(" New value of index:"))
            
            newindextext1 = bquote( paste( "  ",
              .(w0new) + .(w1new)*x[1*i]+.(w2new)*x[2*i]  == .(indexnew), symbol("\076"), .(ir),
              
            "       with   ", x[1]==.(round(xi[1], digits = 2)), 
                          ",  ", x[2]==.(round(xi[2], digits = 2))))
            
          } else {
            errtext1 = bquote( paste( "Error = target \055 prediction" == .(yi), 
                                      symbol(" \055 "), .(pred) == .(error), 
                                      " (index \"OVERSHOOTS\")"))
            errtext2 = bquote( paste( "", symbol("\256"), 
                          " Adjust weights such that INDEX" ,symbol("\257")," for current i)" ))
            
            newindextext0 = bquote(paste(" New value of index:"))
            
            newindextext1 = bquote( paste( "  ",
              .(w0new) + .(w1new)*x[1*i]+.(w2new)*x[2*i] == .(indexnew) , symbol("\074"), .(ir), 
            
            "       with   ", x[1]==.(round(xi[1], digits = 2)), 
                                          ",  ", x[2]==.(round(xi[2], digits = 2))))
            
          }
          
          
          # Information about adjustments of weights
          # (this depends on only TWO possible value sets of the error)
          
          
          if (error == 0){
            w0adjtext = bquote( paste(
              "      Adjustment of ", w[0], ": NONE" ))
            w1adjtext = bquote( paste(
              "      Adjustment of ", w[1], ": NONE" ))
            w2adjtext = bquote( paste(
              "      Adjustment of ", w[2], ": NONE" ))
            
            formula1 = bquote("")
            formula2 = bquote("")
      
            
          } else {
            
            formula1 = bquote(paste("Formula for adjustment: "))
            
            formula2 = bquote(paste("       ", w[k]^paste(old) %->% w[k]^paste(new) == w[k]^paste(old) + .(eta) %.% error %.% x[k], 
                         ";     k = 0,1,2;  ",   x[0] %==% 1))
            
            w0adjtext = bquote( paste(
              "             Adjustment of ", w[0], ":  ", 
                        .(wr[1]) %->% .(w0new )))
            
            w1adjtext = bquote( paste( 
              "             Adjustment of ", w[1], ":  ", 
                        .(wr[2]) %->% .(w1new)))
            
            w2adjtext = bquote( paste( 
              "             Adjustment of ", w[2], ":  ", 
                        .(wr[3]) %->% .(w2new)))
            
          }
          
          
          # Now put all the text together in a list "annot"
          # every element of the list will end up as a line in the plot,
          # where the lines are set by mtext() below
          # Every element of annot is embedded in bquote(paste())
          # (except empty lines).
          
          annot <- list( 
             
            bquote( paste( "Index at current state of learning (", i==.(i), "):" ) ) ,
            
            
            # Formula for index
            
            bquote( paste( "      ",.(wr[1]) + .(wr[2])*x[1*i]+.(wr[3])*x[2*i] == .(ir),
                    ",       with  ", x[1*i]==.(round(xi[1], digits = 2)), 
                          ",  ", x[2]==.(round(xi[2], digits = 2)))),
            
            bquote(""),
            
            bquote( paste( "Current prediction: ", .(pred), 
                    " (\"", .(
                      unique(as.character(
                        D[[targetVar]][D$Label == pred]))
                      ),"\"" ,
            
                      "; predict 1 if ",index>=0,", and 0 otherwise)")),
            
            
            bquote( paste( "Current target: ", .(yi), 
                           " (\"", .(as.character(D[[targetVar]])[i]),"\")") ) ,
              
            bquote(""),
            
            
            errtext1 ,
            
            errtext2,
            
            formula1,
            
            formula2, 
            
            w0adjtext,
            
            w1adjtext,
            
            w2adjtext,
            
            bquote(""),
            
            newindextext0,
            
            newindextext1,
            
            bquote(""),
            
            bquote("Errors:"),
            
            bquote(paste("   Total of misclassified cases (relativ to all cases): ", 
                         .(misclas), "\045")), # percent 045
            
            bquote(paste("   False positives (relative to true negatives): ", .(falPos), 
                         "\045")),
            
            bquote(paste("   False negaties (relative to true positives): ", .(falNeg),
                         "\045"))
            )
          
          
          nLines = length(annot)
          
          
          # left-alignment of text
          at = c(c(0.2, 1), c(0.2, 0.8), c(0.2, 0.6))
          
          # basline color for text
          col = rep("#305050", nLines)
          
          # Adjust colors of some lines
          if (error!=0){
            col[c(7:8)] = posCol
            col[11:13] = negCol
            col[16] = negCol
            
          } else {
            col[c(7:8)] = "#a00613"
            col[11:13] = "#081160"
            
          }
          
          # draw empty "canvas" for left pane; 
          # the measures of the canvas are [0 1]x[0 1]
          
          plot(0:1, 0:1, type = "n", axes = FALSE, xlab="", ylab="")
          
          # Get effective limits x- ad y-limits of plot 
          # (these are not literally 0 and 1)
          lims = par('usr')
          
          # Draw a rectangle inside the canvas with background color white.
          rect(lims[1],lims[3],lims[2], lims[4], col = "white")#alpha("white", 0))
          
          # Draw border area
          box()
          
          # Project text onto canvas
          mtext(do.call(expression, annot),
                
                # define border and spacing of lines on (0,1)-canvas
                line = - 0.4 - c(1:nLines)*2 ,  
                
                # set font size, color, alignment and positioning
                cex = 1.5, col = col , adj = 0, at = rep(0, 3))

    
    # Title and subtitle entire plot
          
    if (standardize){
        tt = ", predictors standardized"
    } else {
        tt = ", predictors not standardized"
      }
          
    title(paste0("Two-dimensional perceptron learning (" , dataLabel, " data)"),
          outer = TRUE, line = 0, cex.main = 2)
    
    mtext(bquote(paste("Parameters: ", eta, "=", .(eta), .(tt))), 
          outer=TRUE, line = -3, cex = 1.7, col = "steelblue4"  )
    
    

 ani.pause()  # indicates that new plot for animation should be made
 
}  # ending curly bracket of the big for-loop 
}, # ending curly bracket of saveHTML

# more arguments for the animation


    # name of html file
    htmlfile = paste0("Perceptron2d_",dataLabel,".html"),

    # name of folder where images are saved
    imgdir = paste0("IMG_perceptron2d_",dataLabel),

    # The theme for the animation slide show
    global.opts = "$.fn.scianimator.defaults.theme = 'dark';",

    # Start the animation automatically
    autobrowse = TRUE, 

    # Title indicated in browser
    title = "Perceptron learning",

    # Add navigator to the animation
    navigator = TRUE,

    # Do not show the code that generated the animation
    verbose = FALSE,

    # size of animation field/"div" in the html file
    # This is completely independend of how the plots are shown in RStudio
    ani.width = 1500, ani.height = 820

) # Ending bracket of saveHTML


 
 

