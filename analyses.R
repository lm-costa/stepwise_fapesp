tab <- readxl::read_excel("VIF.xlsx")

XCO2 <- tab$XCO2


tab <- tab[,-1] |>
  dplyr::mutate_all(scale)

tab <- cbind(XCO2, tab)

model <- lm(XCO2~.,data=tab)

a <- car::vif(model)
a

####
mod_ta <- tab |> dplyr::select(XCO2, Qg, `SIF 757`, RH, Ws)



set.seed(138)

inTraining <- caret::createDataPartition(mod_ta$XCO2, p= .7, list=FALSE)
training <- mod_ta[inTraining,]
testing <- mod_ta[-inTraining,]

fitControl <- caret::trainControl(method = "repeatedcv", 
                                  number=3,
                                  repeats = 3)


# step selection 

set.seed(283)

stepfit1 <- caret::train(XCO2~., data=training,
                         method= "leapForward",
                         tuneGrid=data.frame(nvmax=1:4),
                         trControl=fitControl
)

ggplot2::ggplot(stepfit1)+
  ggplot2::theme_classic()


coef(stepfit1$finalModel,2)


stepPred <- predict(stepfit1, newdata = testing)

xco2_obs=testing$XCO2
xco2_est=stepPred

MAPE = sum(abs((xco2_obs-xco2_est)/xco2_obs))*100
MAPE
