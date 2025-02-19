#EML-ASSIGNMENT 02

#-------------------------------------------------------------------------------
#QUESTION 1
#(2P) Download and load the phoneme data set (phoneme.csv) from the course website. Split the
#dataset into training and test set according to the speaker column. Be sure to exclude the row
#number, speaker and response columns from the features. Useful functions: strsplit()
getwd()
phoneme<-read.csv("phoneme.csv")
View(phoneme)
speaker_split = unlist(strsplit(x=phoneme$speaker, "[.]"))
#convert list into a matrix for easier accessibility  
splitspeaker_matrix = matrix(speaker_split, ncol=4, byrow = T)
#construct train boolean vectors; first column of matrix corresponds to either "train" or "test"
#and skip columns "row_number", "g" and "speaker" 
traindata = (splitspeaker_matrix[,1]=="train")
traindata_X = phoneme[traindata,2:257]
traindata_Y = phoneme$g[traindata]
#construct test 
testdata_X = phoneme[!traindata,2:257]
testdata_Y = phoneme$g[!traindata]

#----------------------------------------------------------------------------

#2. (3P) Fit an LDA model, compute and report train and test error. Useful functions: lda() from the
#MASS package

library(MASS)
lda_fit = lda(phoneme$g~., phoneme[,2:257], subset=traindata)
lda_fit

#predict on training-set
lda_pred_train = predict(lda_fit, traindata_X)$class
table(lda_pred_train, traindata_Y)
mean(lda_pred_train==traindata_Y)

#predict on test-set
lda_pred_test = predict(lda_fit, testdata_X)$class
table(lda_pred_test, testdata_Y)
mean(lda_pred_test==testdata_Y)

#----------------------------------------------------------------------------

#(3P) Select the two phonemes aa and ao. Fit an LDA model on this data set and repeat the steps
#done in (2).
attach(phoneme)
#construct boolean vectors for determining train and test sets
trainingdata = (ss_matrix[,1]=="train")
phoneme_selection = (g=="ao" | g=="aa")
traindata_sel = (phoneme_selection & trainingdata)
testdata_sel = (phoneme_selection & !trainingdata)

traindata_X_sel = phoneme[traindata_sel,2:257]
traindata_Y_sel = g[traindata_sel]
#construct test 
testdata_X_sel = phoneme[testdata_sel,2:257]
testdata_Y_sel = g[testdata_sel]

#fit lda model using only training observations with phonemes "aa" or "ao"
lda_fit_sel = lda(formula=g~., data=phoneme[,2:257], subset=traindata_sel)

#prediction on train set
lda_sel_pred_traindata = predict(lda_fit_sel, traindata_X_sel)$class
table(lda_sel_pred_traindata, traindata_Y_sel)
mean(lda_sel_pred_traindata==traindata_Y_sel)

#prediction on test set
lda_sel_pred_testdata = predict(lda_fit_sel, testdata_X_sel)$class
table(lda_sel_pred_testdata, testdata_Y_sel)
mean(lda_sel_pred_testdata==testdata_Y_sel)

#-----------------------------------------------------------------------------

#4. (5P) Repeat steps (2) and (3) using QDA and report your findings. Would you prefer LDA or QDA
#in this example? Why? Useful functions: qda() from the MASS package

#qda with all phonemes
qda_fit = qda(formula=g~., data=phoneme[,2:257], subset=traindata)

#prediction on train set
qda_pred_traindata = predict(qda_fit, traindata_X)$class
table(qda_pred_traindata, traindata_Y)
mean(qda_pred_traindata==traindata_Y)

#prediction on test set
qda_pred_testdata = predict(qda_fit, testdata_X)$class
table(qda_pred_testdata, testdata_Y)
mean(qda_pred_testdata==testdata_Y)

#------------------------------------------------------------------------------

#5. (3P) Generate four confusion matrices: for the LDA and QDA model for aa and ao on test and
#training data. Which differences can you observe between the models?

#qda with only "aa" and "ao"
qda_fit_sel = qda(formula=g~., data=phoneme[,2:257], subset=traindata_sel)

#predict on train set
qda_pred_traindata_sel = predict(qda_fit_sel, traindata_X_sel)$class
table(qda_pred_traindata_sel, traindata_Y_sel)
mean(qda_pred_traindata_sel==traindata_Y_sel)

#predict on test set
qda_pred_testdata_sel = predict(qda_fit_sel, testdata_X_sel)$class
table(qda_pred_testdata_sel, testdata_Y_sel)
mean(qda_pred_testdata_sel==testdata_Y_sel)

#-----------------------------------------------------------------------------


#6. (4P) Compare LOOCV, 5- and 10-fold cross validation on the training data set to estimate the test
#error of using linear regression to predict lpsa from all other features. Use the full training data set
#to train a linear regression model and compute the test error. Compare your estimates obtained
#from cross validation to the error obtained from the test set and argue about your findings. Which
#of the methods is (theoretically) fastest?

#get working dir path
working_dir = getwd()
#construct relative path to data file
path_to_file = paste(working_dir, "/prostate.Rdata", sep="")
#read data
load(path_to_file)

library(boot)
attach(prostate.train)

#multiple linear regression
glm_fit = glm(lpsa~., data=prostate.train)
#predict lpsa for test set
glm_pred_test = predict(glm_fit, prostate.test)
#calculate test mse
glm_mspe = mean( (prostate.test$lpsa - glm_pred_test)^2 )
glm_mspe

#LOOCV linear regression
loocv_error = cv.glm(prostate.train, glm_fit)
#get the bias adjusted test MSE estimate
loocv_error$delta

#5-fold CV
cv5_error = cv.glm(prostate.train, glm_fit, K=5)
#get the bias adjusted test MSE estimate
cv5_error$delta

#10-fold CV
cv10_error = cv.glm(prostate.train, glm_fit, K=10)
#get the bias adjusted test MSE estimate
cv10_error$delta
