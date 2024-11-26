library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
library(rsconnect)

# UI 정의
ui <- dashboardPage(
  dashboardHeader(title = "구매 여부 예측 서비스"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("직접 입력 예측", tabName = "prediction", icon = icon("chart-line")),
      menuItem("파일 데이터 예측", tabName = "upload", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Roboto', sans-serif;
          background-color: #f4f7fa;
        }
        .box { 
          background-color: #ffffff; 
          border-radius: 15px; 
          padding: 30px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }
        .box-header { 
          background-color: #4CAF50; 
          color: white; 
          text-align: center;
          font-size: 20px;
          border-radius: 10px;
        }
        h4, h3, p {
          font-weight: 500;
          color: #333;
        }
        .btn {
          background-color: #4080EC;
          color: white;
          border: none;
          padding: 12px 20px;
          font-size: 16px;
          border-radius: 5px;
          width: 100%;
        }
        .btn:hover {
          background-color: #45a049;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "prediction",
              fluidRow(
                column(12,
                       box(
                         status = "primary", solidHeader = TRUE,
                         selectInput("month", "월(Month):", 
                                     choices = c("Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                         selectInput("visitorType", "방문자 유형(Visitor Type):", 
                                     choices = c("New_Visitor", "Returning_Visitor")),
                         numericInput("productRelated", "제품 관련 방문 수(Product Related):", value = 50, min = 1),
                         numericInput("BounceRates", "이탈률(Bounce Rate):", value = 0.2, min = 0, step = 0.01),
                         numericInput("ExitRates", "종료율(Exit Rate):", value = 0.3, min = 0, step = 0.01),
                         numericInput("PageValues", "PageValues:", value = 20, min = 0),
                         numericInput("productRelatedDuration", "제품 관련 방문 지속 시간(Product Related Duration):", value = 10, min = 1),
                         numericInput("adminDuration", "행정 관련 방문 지속 시간(Administrative Duration):", value = 5, min = 1),
                         actionButton("predict", "예측하기")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         status = "info", solidHeader = TRUE,
                         h5("구매 여부 예측:"),
                         verbatimTextOutput("prediction"),
                         h5("추천 서비스:"),
                         verbatimTextOutput("recommendation")
                       )
                )
              )
      ),
      tabItem(tabName = "upload",
              fluidRow(
                column(12,
                       box(
                         status = "primary", solidHeader = TRUE,
                         fileInput("file", "CSV 파일 업로드", accept = ".csv")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         tags$strong("파일 미리보기"),
                         status = "info", solidHeader = TRUE,
                         DTOutput("file_preview"),
                         actionButton("selected_row", "선택한 행으로 예측하기")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         status = "primary", solidHeader = TRUE,
                         numericInput("row_number", "예측할 행 번호", value = 1, min = 1),
                         actionButton("predict_row", "행 번호로 예측하기")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         status = "info", solidHeader = TRUE,
                         h5("구매 여부 예측:"),
                         verbatimTextOutput("file_prediction"),
                         h5("추천 서비스:"),
                         verbatimTextOutput("file_recommendation")
                       )
                )
              )
      )
    )
  )
)

# 서버 로직 정의
server <- function(input, output) {
  # 예측 함수
  rf3 <- readRDS("data/random_forest_model.rds")
  
  predict_purchase <- reactive({
    req(input$predict)
    input_data <- data.frame(
      Month = as.numeric(factor(input$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))),
      VisitorType = ifelse(input$visitorType == "New_Visitor", 0, 1),
      ProductRelated = input$productRelated,
      BounceRates = input$BounceRates,
      ExitRates = input$ExitRates,
      ProductRelated_Duration = input$productRelatedDuration,
      Administrative_Duration = input$adminDuration,
      PageValues = input$PageValues
    )
    prediction <- predict(rf3, newdata = input_data, type = "response")
    return(list(prediction = prediction, input_data = input_data))
  })
  
  output$prediction <- renderText({
    prediction <- predict_purchase()$prediction
    if (prediction == 1) {
      return("구매 가능성 높음")
    } else {
      return("구매 가능성 낮음")
    }
  })
  
  output$recommendation <- renderText({
    result <- predict_purchase()
    input_data <- result$input_data
    prediction <- result$prediction
    
    recommendations <- c()
    if (input$visitorType == "New_Visitor" && prediction == 0) {
      recommendations <- c(recommendations, "신규 방문자를 위한 첫 구매 20% 할인 제공.")
    }
    if (input$visitorType == "Returning_Visitor" && prediction == 0) {
      recommendations <- c(recommendations, "재방문 고객을 위한 이전 관심 상품 맞춤 추천 제공.")
    }
    if (input$productRelatedDuration > 300) {
      recommendations <- c(recommendations, "제품 관련 페이지에 오래 머물렀으므로, 해당 제품에 대한 추가 할인 제공.")
    }
    if (input$PageValues > 100) {
      recommendations <- c(recommendations, "구매 완료를 유도하는 팝업 메시지 제공.")
    }
    if (length(recommendations) == 0) {
      recommendations <- c("추천 서비스: 일반 구매 유도 캠페인.")
    }
    return(paste(recommendations, collapse = "\n"))
  })
  
  # 업로드된 파일 데이터를 저장
  dataset <- reactive({
    req(input$file)
    data <- read.csv(input$file$datapath)
    return(data)
  })
  
  # 파일 미리보기 출력
  output$file_preview <- renderDT({
    req(dataset())
    dataset()
  }, selection = "single", options = list(scrollX = TRUE))
  
  # 선택된 행 예측
  observeEvent(input$file_preview_rows_selected, {
    selected_row <- input$file_preview_rows_selected
    req(selected_row)
    
    selected_data <- dataset()[selected_row, , drop = FALSE]
    prediction <- predict(rf3, newdata = selected_data, type = "response")
    
    output$file_prediction <- renderText({
      if (prediction == 1) {
        return("구매 가능성 높음")
      } else {
        return("구매 가능성 낮음")
      }
    })
    
    output$file_recommendation <- renderText({
      recommendations <- c()
      if (selected_data$VisitorType == "New_Visitor" && prediction == 0) {
        recommendations <- c(recommendations, "신규 방문자를 위한 첫 구매 20% 할인 제공.")
      }
      if (selected_data$VisitorType == "Returning_Visitor" && prediction == 0) {
        recommendations <- c(recommendations, "재방문 고객을 위한 관심 상품 맞춤 추천 제공.")
      }
      if (selected_data$ProductRelated_Duration > 300) {
        recommendations <- c(recommendations, "제품 관련 페이지에 오래 머물렀으므로, 해당 제품에 대한 추가 할인 제공.")
      }
      if (selected_data$PageValues > 100) {
        recommendations <- c(recommendations, "구매 완료를 유도하는 팝업 메시지 제공.")
      }
      if (length(recommendations) == 0) {
        recommendations <- c("추천 서비스: 일반 구매 유도 캠페인.")
      }
      return(paste(recommendations, collapse = "\n"))
    })
  })
  
  # 행 번호로 예측
  observeEvent(input$predict_row, {
    req(dataset())
    row_number <- as.numeric(input$row_number)
    req(row_number > 0 && row_number <= nrow(dataset()))
    
    selected_data <- dataset()[row_number, , drop = FALSE]
    prediction <- predict(rf3, newdata = selected_data, type = "response")
    
    output$file_prediction <- renderText({
      if (prediction == 1) {
        return("구매 가능성 높음")
      } else {
        return("구매 가능성 낮음")
      }
    })
    
    output$file_recommendation <- renderText({
      recommendations <- c()
      if (selected_data$VisitorType == "New_Visitor" && prediction == 0) {
        recommendations <- c(recommendations, "신규 방문자를 위한 첫 구매 20% 할인 제공.")
      }
      if (selected_data$VisitorType == "Returning_Visitor" && prediction == 0) {
        recommendations <- c(recommendations, "재방문 고객을 위한 관심 상품 맞춤 추천 제공.")
      }
      if (selected_data$ProductRelated_Duration > 300) {
        recommendations <- c(recommendations, "제품 관련 페이지에 오래 머물렀으므로, 해당 제품에 대한 추가 할인 제공.")
      }
      if (selected_data$PageValues > 100) {
        recommendations <- c(recommendations, "구매 완료를 유도하는 팝업 메시지 제공.")
      }
      if (length(recommendations) == 0) {
        recommendations <- c("추천 서비스: 일반 구매 유도 캠페인.")
      }
      return(paste(recommendations, collapse = "\n"))
    })
  })
}

# 앱 실행
shinyApp(ui = ui, server = server)
