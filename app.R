# https://youtu.be/a0E0aEiF_aE
# Waleed Khalid Siraj - 23803313
# Saif Ali Athyaab - 23810756
library(shiny)
library(shinyWidgets)
library(forcats)
library(gridExtra)
library(tidyverse)
library(vtreat)
library(dplyr)
library(reshape2)
library(ROCR)
library(knitr)
library(ggplot2)
library(crayon)
library(ROCit)
library(rpart)
library(rpart.plot)
library(pander)
library(e1071)
library(pROC)
gys.data <- read.csv("youtube_UTF_8.csv",stringsAsFactors=T,na.strings = "nan",encoding = 'UTF-8')
#Reading in the world bank dataset
#Taking in only 2005 to 2022 columns
wbd <- read.csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_5871588.csv",header=TRUE)
wbd <- wbd[,c(1,2,50:67)]
#Calculating Avergae GDP per capita from 2005 to 2022
wbd$Average_GDP_PCAP <- rowMeans(wbd[,3:20], na.rm=TRUE)
#Keeping only Country Name, Code, calculated column
wbd2 <- wbd[,c(1,2,21)]
#Merging with WDB data by Country as the KEY
s1 <- merge(gys.data,wbd2, by.x ="Country", by.y="Country.Name", all.x=TRUE)

#Calculating Rural population
attach(s1)
#s1$Rural_population <- Population - Urban_population

s1$date_n <- as.Date(paste(s1$created_year, s1$created_month, s1$created_date, sep = "-"), format = "%Y-%b-%d")

s1$date_n[s1$date_n=="1970-01-01"] <- NA
#Using the most recent youtube channel to calculate age of channel
date_rec <- max(s1$date_n, na.rm = TRUE)
s1$channel_age <- as.numeric(date_rec - s1$date_n)

date_ref <- as.Date("2012-02-14")

s1$oldNew <- ifelse(s1$channel_age > as.numeric(date_rec- date_ref), -1, 1)

sv <- subset(s1, select = -c(rank,Youtuber,Title,video_views_rank,date_n,lowest_monthly_earnings,lowest_yearly_earnings,highest_yearly_earnings, created_year, created_month, created_date, Country.Code,channel_age,Latitude,Longitude,Urban_population,Abbreviation))
detach(s1)
add_missing <- function(column_old) {
  column_new <- column_old
  levels(column_new) <- c(levels(column_old), "Missing")
  for (i in 1:length(column_new)) {
    if (is.na(column_new[i])) {
      column_new[i] <- "Missing"
    }
  }
  column_new
}
outcome <- 'oldNew'
pos <- '1'
sv <- sv %>% rename(Unemployment_rate := Unemployment.rate,
                    video_views := video.views,
                    GTEE := starts_with("Gross.tertiary"))
vars <- setdiff(colnames(sv), outcome)

catVars <- vars[sapply(sv[, vars], class) %in% c('factor', 'character')]
numericVars <- vars[sapply(sv[, vars], class) %in% c('numeric', 'integer')]
for (i in catVars){
  sv[,i] <- add_missing(sv[,i])
}

svNum <- sv[,numericVars]
svCat <- sv[,catVars]
treatment_plan <- design_missingness_treatment(svNum)
svNum <- prepare(treatment_plan,svNum)

svNum <- svNum %>% select(-contains("isBAD"))
d <- cbind(svNum,svCat)
d$oldNew <- sv$oldNew
d <- na.omit(d)
set.seed(729375)
d$rgroup <- runif(nrow(d))
dTrainAll <- subset(d, rgroup<=0.9)
dTest <- subset(d, rgroup > 0.9)

vars <- setdiff(colnames(dTrainAll), c(outcome,'rgroup'))

catVars <- vars[sapply(dTrainAll[,vars], class) %in% c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars], class) %in% c('numeric','integer')]
# split dTrainAll into a training set and a validation (or calibration) set
useForCal <- rbinom(n=dim(dTrainAll)[[1]], size=1, prob=0.1) > 0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll,!useForCal)
mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol == pos,na.rm = TRUE) / length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]),useNA = 'ifany')
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos, ] + 1.0e-3*pPos) / (colSums(vTab) + 1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

mkPredN <- function(outCol, varCol, appCol) {
  cuts <- unique(as.numeric(
    quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

for (v in catVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTest[,v])
}

for(v in numericVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTest[,v])
}
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}


# Create an empty data frame to store AUC values
auc_df <- data.frame(Variable = character(0), Train_AUC = numeric(0), Calibration_AUC = numeric(0))

for (v in c(catVars, numericVars)) {
  pi <- paste('pred', v, sep='')
  aucTrain <- calcAUC(dTrain[, pi], dTrain[, outcome])
  if (aucTrain >= 0.6) {
    aucCal <- calcAUC(dCal[, pi], dCal[, outcome])
    
    # Add the AUC values to the data frame
    auc_df <- rbind(auc_df, data.frame(Variable = pi, Train_AUC = aucTrain, Calibration_AUC = aucCal))
  }
}
auc_df <- auc_df[order(-auc_df$Calibration_AUC),]
dt1 <- rpart(formula = oldNew>0 ~ Population + Average_GDP_PCAP + category + channel_type, data = dTrain)
dt2 <- rpart(formula = oldNew>0 ~ channel_type + subscribers + Population + Average_GDP_PCAP + country_rank, data = dTrain)

#SVM DATA PREP
set.seed(729374)

svm_d <- d
svm_d$oldNew <- as.factor(svm_d$oldNew)
svm_d$rgroup <- runif(nrow(svm_d))
svm_TrainAll <- subset(svm_d, rgroup<=0.9)
svm_Test <- subset(svm_d, rgroup > 0.9)

svm_vars <- setdiff(colnames(svm_TrainAll), c(outcome,'rgroup'))

svm_catVars <- svm_vars[sapply(svm_TrainAll[,svm_vars], class) %in% c('factor','character')]
svm_numericVars <- svm_vars[sapply(svm_TrainAll[,svm_vars], class) %in% c('numeric','integer')]
# split dTrainAll into a training set and a validation (or calibration) set
svm_useForCal <- rbinom(n=dim(svm_TrainAll)[[1]], size=1, prob=0.1) > 0
svm_Cal <- subset(svm_TrainAll, svm_useForCal)
svm_Train <- subset(svm_TrainAll,!svm_useForCal)

# Train the SVM model on the training data
svm_model1 <- e1071::svm(oldNew ~ Population + Average_GDP_PCAP + category + Country, data = svm_Train, kernel="linear", cost=2)

# Make predictions on the validation data
svm_pred <- predict(svm_model1, newdata = svm_Cal)
# Calculate the AUC on the validation data
svm_auc <- roc(svm_Cal$oldNew, as.numeric(svm_pred))$auc
# Print the AUC on the validation data
cat("SVM AUC on validation data:", svm_auc, "\n")
# Make predictions on the test data
svm_pred_test <- predict(svm_model1, newdata = svm_Test)
# Calculate the AUC on the test data
svm_auc_test <- roc(svm_Test$oldNew, as.numeric(svm_pred_test))$auc
# Print the AUC on the test data
cat("SVM AUC on test data:", svm_auc_test, "\n")
# Create a confusion matrix for the test data
svm_cm <- table(svm_pred_test, svm_Test$oldNew)


plot_svm_roc2 <- function(svm_model, train_data, test_data) {
  
  # Train the Null Model on the training data
  null_model <- glm(oldNew ~ 1, data = train_data, family = "binomial")
  
  # Make predictions on the training data for the SVM model and Null Model
  svm_pred_train <- predict(svm_model, newdata = train_data)
  null_pred_train <- predict(null_model, newdata = train_data, type = "response")
  
  # Calculate the ROC curves for the SVM model and Null Model on the training data
  svm_roc_train <- roc(train_data$oldNew, as.numeric(svm_pred_train))
  null_roc_train <- roc(train_data$oldNew, as.numeric(null_pred_train))
  
  # Make predictions on the test data for the SVM model and Null Model
  svm_pred_test <- predict(svm_model, newdata = test_data)
  null_pred_test <- predict(null_model, newdata = test_data, type = "response")
  
  # Calculate the ROC curves for the SVM model and Null Model on the test data
  svm_roc_test <- roc(test_data$oldNew, as.numeric(svm_pred_test))
  null_roc_test <- roc(test_data$oldNew, as.numeric(null_pred_test))
  
  # Plot the ROC curves for the SVM model, Null Model, and random classifier
  ggplot() +
    geom_line(aes(x = svm_roc_train$specificities, y = svm_roc_train$sensitivities, color = "Training"), linetype = "solid") +
    geom_line(aes(x = svm_roc_test$specificities, y = svm_roc_test$sensitivities, color = "Testing"), linetype = "solid") +
    geom_line(aes(x = null_roc_train$specificities, y = null_roc_train$sensitivities, color = "Null Model"), linetype = "dashed") +
    labs(x = "False Positive Rate", y = "True Positive Rate", title = "ROC Curves for Training, Testing and Null Model", legend="Model Legend", color="Model Legend") +
    scale_x_reverse() +
    scale_color_manual(values = c("Training" = "blue", "Testing" = "red", "Null Model" = "black")) +
    theme_bw()
}
### SVM Model 2 - Feature Subset from logLikelihood
svm_model2 <- svm(oldNew ~ subscribers + subscribers_for_last_30_days + Population + Average_GDP_PCAP, kernel="linear" ,data = svm_Train, cost=2)
svm_pred <- predict(svm_model2, newdata = svm_Cal)
# Calculate the AUC on the validation data
svm_auc <- roc(svm_Cal$oldNew, as.numeric(svm_pred))$auc
svm_pred_test <- predict(svm_model2, newdata = svm_Test)
# Calculate the AUC on the test data
svm_auc_test <- roc(svm_Test$oldNew, as.numeric(svm_pred_test))$auc
svm_cm <- table(svm_pred_test, svm_Test$oldNew)


vars.to.use <- c("subscribers","video_views","uploads","country_rank", "channel_type_rank", "highest_monthly_earnings","Average_GDP_PCAP")
scaled_df <- scale(d[ ,vars.to.use])
attr(scaled_df, "scaled:center")

dist <- dist(scaled_df, method="manhattan")
pfit <- hclust(dist, method="ward.D2") # perform hierarchical clustering
groups <- cutree(pfit, k=3)



# UI

ui <- fluidPage(
  titlePanel("Global Youtube Statistics 2023 Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Select model type:", choices = c("Single Variable", "Multi Variable", "Clustering results")),
      conditionalPanel(
        condition = "input.model_type == 'Single Variable'",
        selectInput("selected_attributes", "Select attribute:", choices = auc_df$Variable),
        selectInput("plot_type", "Select plot type:", choices = c("AUC", "Density", "ROC"))
      ),
      conditionalPanel(
        condition = "input.model_type == 'Multi Variable'",
        selectInput("feature_subset", "Feature subset:",
                    choices = c("Feature Subset 1(Population, Average_GDP_PCAP, Category, Channel Type)", "Feature Subset 2(Channel Type, Subscribers,  Population,  Average_GDP_PCAP, Country Rank)")
        ),
        selectInput("multi_model_type", "Model type:",
                    choices = c("Decision Tree", "SVM")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'Clustering results'",
        # You can add more inputs for clustering results if needed
      )
    ),
    mainPanel(
      plotOutput("plot", height = "500px", width = "800px")
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    if (input$model_type == "Single Variable") {
      if (input$plot_type == "Density") {
        selected_attribute <- input$selected_attributes
        if (!is.null(selected_attribute)) {
          # Display the density plot for the selected attribute
          ggplot(dCal) + geom_density(aes(x = .data[[selected_attribute]], color = as.factor(oldNew)))
        }
      } else if (input$plot_type == "ROC") {
        selected_attribute <- input$selected_attributes
        if (!is.null(selected_attribute)) {
          # Display the ROC curve for the selected attribute
          plot_roc(dCal[[selected_attribute]], dCal[, outcome])
        }
      }
      else if (input$plot_type == "AUC") {
        selected_attribute <- input$selected_attributes
        if (!is.null(selected_attribute)) {
          # Display the auc for the selected attribute
          # Assuming 'selected_attribute' contains the selected variable
          
          # Load the ggplot2 pack
          # Use the position_dodge function to adjust the position of the bars
          ggplot(auc_df[auc_df$Variable == selected_attribute, ], aes(x = Variable)) +
            geom_bar(aes(y = Train_AUC, alpha=0.2, fill = "Train AUC"), stat = "identity", position = position_dodge(width = 0.8)) +
            geom_bar(aes(y = Calibration_AUC,alpha=0.2, fill = "Calibration AUC"), stat = "identity", position = position_dodge(width = 0.8)) +
            scale_fill_manual(values = c("Train AUC" = "blue", "Calibration AUC" = "red")) +
            labs(y = "AUC Values") +
            theme_minimal()
          
        }
      }
    }
    else if (input$model_type == "Multi Variable") {
      feature_subset <- input$feature_subset
      if (feature_subset == "Feature Subset 1(Population, Average_GDP_PCAP, Category, Channel Type)") {
        if (input$multi_model_type == "Decision Tree") {
          rpart.plot(dt1)
        } else if (input$multi_model_type == "SVM") {
          # Add code for SVM plot 
          plot(svm_model1 , data= svm_Cal, formula = Population ~ Average_GDP_PCAP, svSymbol = 4, dataSymbol = 5, symbolPalette = rainbow(4))
          
        }
      } else if (feature_subset == "Feature Subset 2(Channel Type, Subscribers,  Population,  Average_GDP_PCAP, Country Rank)") {
        if (input$multi_model_type == "Decision Tree") {
          rpart.plot(dt2)
        } else if (input$multi_model_type == "SVM") {
          # Add code for SVM plot
          plot(svm_model2 , data= svm_Cal, formula = subscribers ~ Population, svSymbol = 4, dataSymbol = 5, symbolPalette = rainbow(4))
        }
      }
      
    }
    else if (input$model_type == "Clustering results") {
      # Add code to display the cluster dendrogram
      hcd <- as.dendrogram(pfit)
      plot(hcd, ylim=c(30,100), cex = 0.2, main="Dendogram Subset", xlab="Data Points", ylab="Height")
      rect.hclust(pfit, k=3)
      text(x = 60, y = 32, labels = "Cluster 1", col = "red", cex = 0.8)
      text(x = 160, y = 32, labels = "Cluster 3", col = "red", cex = 0.5)
      text(x = 600, y = 32, labels = "Cluster 2", col = "red", cex = 0.9)
    }
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
