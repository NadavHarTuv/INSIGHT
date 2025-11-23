rm(list = ls())
options(install.packages.check.source = "no")
for (package in c("shiny", "shinyjs", "rhandsontable", "ggplot2", "Cairo", "utils", "stringr", "multiApply")) {
  if (!package %in% rownames(installed.packages())) {
    install.packages(package, repos = "https://cloud.r-project.org",
                     type = "binary")
  }
}

library(shiny)
options(shiny.maxRequestSize= 100 * 1024 ** 2)
library(rhandsontable)
library(ggplot2)
library(Cairo)
library(utils)
library(multiApply)
library(stringr)
options(shiny.usecairo=TRUE)

# Color
blue.color <- "#0072B2"
red.color <- "#CC1100"


ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Insight"),
  fluidRow(
    # Side panel
    column(width = 3,
           # Analysis method
           wellPanel(
             selectInput("analysisMethod", label = "Analysis Method",
                         choices = list("(Select an analysis)",
                                        "Independence",
                                        "3-Dimensional",
                                        "N-Dimensional",
                                        "Survival",
                                        "Loyalty",
                                        "Ranking",
                                        "Spacing Models",
                                        "Scale Conversion",
                                        "Regression"))
           ),
           # Independence panel
           conditionalPanel(condition = "input.analysisMethod == 'Independence'",
                            # Independence model
                            wellPanel(
                              selectInput("independenceModelName", label = "Model Name",
                                          choices = list("Independence Model",
                                                         "ANOAS Model",
                                                         "Predictors' Proportion",
                                                         "Log-Odds Z-Test",
                                                         "Collapse Chi-Sq Test")),
                              # Independence: ANOAS Model
                              conditionalPanel(condition = "input.independenceModelName == 'ANOAS Model'",
                                               radioButtons("independenceAnoasOrientation",
                                                            label = "Select orientation",
                                                            choices = list("Row", "Column")),
                                               textInput("independenceAnoasGroup", label = "ANOAS groups")
                              ),
                              # Independence: Log-Odds Z-Test
                              conditionalPanel(condition = "input.independenceModelName == 'Log-Odds Z-Test'",
                                               radioButtons("independenceLogOddOrientation",
                                                            label = "Select orientation",
                                                            choices = list("Row", "Column")),
                                               textInput("independenceLogOddWhichTwo", label = "Which two rows?")
                              ),
                              # Independence: Collapse Chi-Sq Test
                              conditionalPanel(condition = "input.independenceModelName == 'Collapse Chi-Sq Test'",
                                               textInput("independenceCollapseChiSqRowGroup", label = "Row groups"),
                                               textInput("independenceCollapseChiSqColumnGroup", label = "Column groups")
                              ),
                              # Go
                              actionButton("independenceGo", label = "Go!")
                            ),
                            # Independence data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("independenceDataDimension", label = "Dimensions"),
                              actionButton("independenceDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("independenceDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Collapse Data'),
                              textInput("independenceDataCollapseRow", label = "Row groups"),
                              textInput("independenceDataCollapseColumn", label = "Column groups"),
                              shinyjs::disabled(actionButton("independenceDataCollapse", label = "Collapse")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("independenceDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("independenceDataClear", label = "Clear"))
                            )
           ),
           # 3-Dimensional panel
           conditionalPanel(condition = "input.analysisMethod == '3-Dimensional'",
                            # 3-Dimensional model
                            wellPanel(
                              selectInput("threeDimensionalModelName", label = "Model Name",
                                          choices = list("Log-Linear Model")),
                              radioButtons("threeDimensionalModelChoice", label = NULL,
                                           choices = list("All coefficients", "Select coefficients", "Target variable")),
                              # 3-Dimensional: Select coefficients
                              conditionalPanel(condition = "input.threeDimensionalModelChoice == 'Select coefficients'",
                                               fluidRow(
                                                 column(width = 5,
                                                        shinyjs::disabled(checkboxInput("threeDimensionalSelectCoefIntercept",
                                                                                        label = "Intercept", value = TRUE))
                                                 ),
                                                 column(width = 3,
                                                        checkboxInput("threeDimensionalSelectCoefX", label = "X"),
                                                        checkboxInput("threeDimensionalSelectCoefY", label = "Y"),
                                                        checkboxInput("threeDimensionalSelectCoefZ", label = "Z")
                                                 ),
                                                 column(width = 4,
                                                        checkboxInput("threeDimensionalSelectCoefXY", label = "X Y"),
                                                        checkboxInput("threeDimensionalSelectCoefXZ", label = "X Z"),
                                                        checkboxInput("threeDimensionalSelectCoefYZ", label = "Y Z")
                                                 )
                                               )
                              ),
                              # 3-Dimensional: Target variable
                              conditionalPanel(condition = "input.threeDimensionalModelChoice == 'Target variable'",
                                               selectInput("threeDimensionalWhichTarget",
                                                           label = "Which target variable?", choices = "None")
                              ),
                              # Go
                              actionButton("threeDimensionalGo", label = "Go!")
                            ),
                            # 3-Dimensional data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("threeDimensionalDataDimension", label = "Dimensions"),
                              actionButton("threeDimensionalDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("threeDimensionalDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              selectInput("threeDimensionalWhichTwoWay", label = "Which two-way data?",
                                          choices = c("X Y", "X Z", "Y X", "Y Z", "Z X", "Z Y")),
                              shinyjs::disabled(downloadButton("threeDimensionalDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("threeDimensionalDataClear", label = "Clear"))
                            )
           ),
           # N-Dimensional panel
           conditionalPanel(condition = "input.analysisMethod == 'N-Dimensional'",
                            # N-Dimensional model
                            wellPanel(
                              selectInput("nDimensionalModelName", label = "Model Name",
                                          choices = list("Log-Linear Model")),
                              radioButtons("nDimensionalModelChoice", label = "Model assumption",
                                           choices = list(
                                             # "Independence", 
                                                          # "Two-way interaction",
                                                          "Variable selection")),
                              # Go
                              actionButton("nDimensionalGo", label = "Go!")
                            ),
                            # N-Dimensional data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("nDimensionalDataDimension", label = "Number of rows and variables"),
                              actionButton("nDimensionalDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("nDimensionalDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("nDimensionalDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("nDimensionalDataClear", label = "Clear"))
                            )
           ),
           # Survival panel
           conditionalPanel(condition = "input.analysisMethod == 'Survival'",
                            # Survival model
                            wellPanel(
                              selectInput("survivalModelName", label = "Model Name",
                                          choices = list("Homogeneous, ACC/DC",
                                                         "Out-of-Sample Splining",
                                                         "Explanatory Variable")),
                              # Survival: Homogeneous, ACC/DC Model
                              conditionalPanel(condition = "input.survivalModelName == 'Homogeneous, ACC/DC'",
                                               textInput("survivalSplining", label = "(Optional) Splining"),
                                               checkboxInput("survivalShowGraph", label = "Show plot?"),
                                               actionButton("survivalGoHomoAccdc", label = "Go!")
                              ),
                              # Survival: Out-of-Sample Splining
                              conditionalPanel(condition = "input.survivalModelName == 'Out-of-Sample Splining'",
                                               textInput("survivalTrainingPortion", label = "Training portion (in %)"),
                                               checkboxInput("survivalReproducible", label = "Reproducible?"),
                                               actionButton("survivalGoTraining", label = "Go Training!"),
                                               hr(style="border-color: lightgray;"),
                                               textInput("survivalSpliningTesting", label = "(Optional) Testing splining"),
                                               checkboxInput("survivalShowGraphTesting", label = "Show plot?"),
                                               actionButton("survivalGoTesting", label = "Go Testing!")
                              ),
                              # Survival: Explanatory Variables
                              conditionalPanel(condition = "input.survivalModelName == 'Explanatory Variable'",
                                               actionButton("survivalGoExplain", label = "Go!")
                              )
                            ),
                            # Survival data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("survivalDataDimension", label = "Number of stages"),
                              actionButton("survivalDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("survivalDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Remove Stage(s)'),
                              textInput("survivalDataRemoveRow", label = "Which stage(s)?"),
                              shinyjs::disabled(actionButton("survivalDataRemove", label = "Remove")),
                              hr(style="border-color: lightgray;"),
                              helpText('Collapse Stages'),
                              textInput("survivalDataCollapseRow", label = "Consecutive stage groups"),
                              shinyjs::disabled(actionButton("survivalDataCollapse", label = "Collapse")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("survivalDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("survivalDataClear", label = "Clear"))
                            )
           ),
           # Loyalty panel
           conditionalPanel(condition = "input.analysisMethod == 'Loyalty'",
                            # Loyalty model
                            wellPanel(
                              selectInput("loyaltyModelName", label = "Model Name",
                                          choices = list("M Model",
                                                         "Q Model",
                                                         "Explore Model",
                                                         "Brand Sensitivity",
                                                         "Brand Omission",
                                                         "Explanatory Variable")),
                              # Go
                              actionButton("loyaltyGo", label = "Go!")
                            ),
                            # Loyalty data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("loyaltyDataDimension", label = "Number of brands"),
                              actionButton("loyaltyDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("loyaltyDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Brand Omission'),
                              textInput("loyaltyBrandOmission", label = "Brands"),
                              shinyjs::disabled(actionButton("loyaltyOmitBrand", label = "Omit")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("loyaltyDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("loyaltyDataClear", label = "Clear"))
                            )
           ),
           # Ranking panel
           conditionalPanel(condition = "input.analysisMethod == 'Ranking'",
                            # Ranking model
                            wellPanel(
                              selectInput("rankingModelName", label = "Model Name",
                                          choices = list("Exploratory",
                                                         "Confirmatory",
                                                         "Brand Sensitivity",
                                                         "Brand Omission",
                                                         "Explanatory Variable")),
                              # Independence: ANOAS Model
                              conditionalPanel(condition = "input.rankingModelName == 'Exploratory' || input.rankingModelName == 'Confirmatory'",
                                               textInput("rankingPolarityIndex", label = "(Optional) Upper tail of customized polarity index")
                              ),
                              # Go
                              actionButton("rankingGo", label = "Go!")
                            ),
                            # Independence data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("rankingDataDimension", label = "Dimensions"),
                              actionButton("rankingDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("rankingDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Brand Omission'),
                              textInput("rankingBrandOmission", label = "Brands"),
                              shinyjs::disabled(actionButton("rankingOmitBrand", label = "Omit")),
                              hr(style="border-color: lightgray;"),
                              helpText('Collapse Data'),
                              textInput("rankingDataCollapseColumn", label = "Consecutive satisfaction level groups"),
                              shinyjs::disabled(actionButton("rankingDataCollapse", label = "Collapse")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("rankingDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("rankingDataClear", label = "Clear"))
                            )
           ),
           # Spacing panel
           conditionalPanel(condition = "input.analysisMethod == 'Spacing Models'",
                            # Spacing model
                            wellPanel(
                              selectInput("spacingModelName", label = "Model Name",
                                          choices = list("Exponential Spacing",
                                                         "Canonical Correlation")),
                              radioButtons("spacingModelType",
                                           label = "Model Type",
                                           choices = list("Model Selection", "RC Model", "R Model",
                                                          "C Model", "U Model")),
                              # Go
                              actionButton("spacingGo", label = "Go!")
                            ),
                            # Spacing data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("spacingDataDimension", label = "Dimensions"),
                              actionButton("spacingDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("spacingDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Collapse Data'),
                              textInput("spacingDataCollapseRow", label = "Row groups"),
                              textInput("spacingDataCollapseColumn", label = "Column groups"),
                              shinyjs::disabled(actionButton("spacingDataCollapse", label = "Collapse")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("spacingDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("spacingDataClear", label = "Clear"))
                            )
           ),
           # Scale conversion panel
           conditionalPanel(condition = "input.analysisMethod == 'Scale Conversion'",
                            # Scale conversion model
                            wellPanel(
                              selectInput("scaleModelName", label = "Model Name",
                                          choices = list("Scale Conversion")),
                              # Go
                              actionButton("scaleGo", label = "Go!")
                            ),
                            # Scale conversion data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("scaleDataDimension", label = "Data length"),
                              actionButton("scaleDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("scaleDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              shinyjs::disabled(downloadButton("scaleDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("scaleDataClear", label = "Clear"))
                            )
           ),
           # Regression panel
           conditionalPanel(condition = "input.analysisMethod == 'Regression'",
                            # Regression model
                            wellPanel(
                              selectInput("regressionModelName", label = "Model Name",
                                          choices = list("Logistic Regression")),
                              # Go
                              actionButton("regressionGo", label = "Go!")
                            ),
                            # Regression data
                            wellPanel(
                              helpText('Create Data'),
                              textInput("regressionDataDimension", label = "Dimensions"),
                              actionButton("regressionDataCreate", label = "Create"),
                              hr(style="border-color: lightgray;"),
                              helpText('Import Data'),
                              fileInput("regressionDataFile", label = NULL, accept = c(".csv")),
                              hr(style="border-color: lightgray;"),
                              helpText('Export Data'),
                              checkboxInput("regressionDataDownloadHeader", label = "Header?", value=TRUE),
                              checkboxInput("regressionDataDownloadRowName", label = "Row names?"),
                              shinyjs::disabled(downloadButton("regressionDataDownload", label = "Download")),
                              hr(style="border-color: lightgray;"),
                              helpText('Clear Data'),
                              shinyjs::disabled(actionButton("regressionDataClear", label = "Clear"))
                            )
           ),
           # Version
           helpText("Version 3.27")
    ),
    # Main panel
    column(width = 9,
           # Independence output
           conditionalPanel(condition = "input.analysisMethod == 'Independence'",
                            tabsetPanel(id = "independenceTab",
                                        tabPanel("Data", br(), rHandsontableOutput("independenceData"))
                            )
           ),
           # 3-Dimensional output
           conditionalPanel(condition = "input.analysisMethod == '3-Dimensional'",
                            tabsetPanel(id = "threeDimensionalTab",
                                        tabPanel("Data", br(), rHandsontableOutput("threeDimensionalData"))
                            )
           ),
           # N-Dimensional output
           conditionalPanel(condition = "input.analysisMethod == 'N-Dimensional'",
                            tabsetPanel(id = "nDimensionalTab",
                                        tabPanel("Data", br(), rHandsontableOutput("nDimensionalData"))
                            )
           ),
           # Survival output
           conditionalPanel(condition = "input.analysisMethod == 'Survival'",
                            tabsetPanel(id = "survivalTab",
                                        tabPanel("Data", br(), rHandsontableOutput("survivalData"))
                            )
           ),
           # Loyalty output
           conditionalPanel(condition = "input.analysisMethod == 'Loyalty'",
                            tabsetPanel(id = "loyaltyTab",
                                        tabPanel("Data", br(), rHandsontableOutput("loyaltyData"))
                            )
           ),
           # Ranking output
           conditionalPanel(condition = "input.analysisMethod == 'Ranking'",
                            tabsetPanel(id = "rankingTab",
                                        tabPanel("Data", br(), rHandsontableOutput("rankingData"))
                            )
           ),
           # Spacing output
           conditionalPanel(condition = "input.analysisMethod == 'Spacing Models'",
                            tabsetPanel(id = "spacingTab",
                                        tabPanel("Data", br(), rHandsontableOutput("spacingData"))
                            )
           ),
           # Scale conversion output
           conditionalPanel(condition = "input.analysisMethod == 'Scale Conversion'",
                            tabsetPanel(id = "scaleTab",
                                        tabPanel("Data", br(), rHandsontableOutput("scaleData"))
                            )
           ),
           # Regression output
           conditionalPanel(condition = "input.analysisMethod == 'Regression'",
                            tabsetPanel(id = "regressionTab",
                                        tabPanel("Data", br(), rHandsontableOutput("regressionData"))
                            )
           )
    )
  )
)

# Util logic


matrix.data <- function(m, row.name = NULL, col.name = NULL) {
  m[] <- as.character(m)
  df <- data.frame(m, stringsAsFactors = FALSE)
  if (is.null(row.name)) {
    row.name <- paste("Row", 1:nrow(m))
  }
  if (is.null(col.name)) {
    col.name <- paste("Column", 1:ncol(m))
  }
  dimnames(df) <- list(row.name, col.name)
  return(df)
}

empty.data <- function(num.row, num.col, row.name = NULL, col.name = NULL) {
  return(matrix.data(matrix("", num.row, num.col), row.name, col.name))
}

empty.data2 <- function(num.row, num.col, row.name = NULL, col.name = NULL) {
  df <- data.frame(matrix("", num.row, num.col), stringsAsFactors = FALSE)
  if (is.null(row.name)) {
    row.name <- paste("Row", 1:num.row)
  }
  if (is.null(col.name)) {
    col.name <- paste("Column", 1:num.col)
  }
  dimnames(df) <- list(row.name, col.name)
  return(df)
}

util.character.is.empty <- function(x, strip.white=TRUE) {
  if (strip.white) {
    x <- trimws(x)
  }
  return(!is.na(x == "") & (x == ""))
}

util.data.frame.to.character <- function(df, strip.white=TRUE) {
  if (is.data.frame(df)) {
    temp <- lapply(df, as.character)
    if (strip.white) {
      temp <- lapply(temp, trimws)
    }
    df2 <- as.data.frame(temp, stringsAsFactors=FALSE,
                         row.names=row.names(df))
    colnames(df2) <- colnames(df)
    return(df2)
  } else {
    temp <- as.character(df)
    if (strip.white) {
      temp <- trimws(temp)
    }
    df2 <- as.data.frame(temp)
    colnames(df2) <- "Column"
    return(df2)
  }
}

util.has.missing.data <- function(x, strip.white=TRUE) {
  x <- as.character(x)
  if (strip.white) {
    x <- trimws(x)
  }
  return(any(x == ""))
}

util.is.numeric <- function(x, strip.white=TRUE) {
  x <- as.character(x)
  if (strip.white) {
    x <- trimws(x)
  }
  return(util.character.is.empty(x) | suppressWarnings(!is.na(as.numeric(x))))
}

util.is.all.numeric <- function(x) {
  return(all(util.is.numeric(x)))
}

util.space <- function(n) {
  return(paste(rep(" ", n), collapse=""))
}

util.matrix.to.string <- function(x, spacing=2, left.justified=TRUE) {
  dim.x <- dim(x)
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  dim(x) <- dim.x
  for (j in seq_len(ncol(x))) {
    longest.entry <- max(nchar(x[,j]))
    if (left.justified) {
      for (i in seq_len(nrow(x))) {
        x[i,j] <- paste0(x[i,j], util.space(longest.entry - nchar(x[i,j])))
      }
    } else {
      for (i in seq_len(nrow(x))) {
        x[i,j] <- paste0(util.space(longest.entry - nchar(x[i,j])), x[i,j])
      }
    }
  }
  for (j in seq_len(ncol(x)-1)) {
    for (i in seq_len(nrow(x))) {
      x[i,j] <- paste0(x[i,j], util.space(spacing))
    }
  }
  return(paste(apply(x, 1, paste, collapse=""), collapse="\n"))
}

table.data <- function(t, row.name = NULL, col.name = NULL) {
  if (is.null(row.name)) {
    row.name <- trimws(rownames(t))
  }
  if (is.null(col.name)) {
    col.name <- trimws(colnames(t))
  }
  return(matrix.data(unclass(t), row.name, col.name))
}

string.is.positive.integer <- function(s) {
  x <- suppressWarnings(as.numeric(s))
  return(!is.na(x) && x > 0 && x == round(x))
}

string.is.dimension <- function(s, d) {
  s <- strsplit(s, ",", fixed = TRUE)[[1]]
  return(length(s) == d && all(sapply(s, string.is.positive.integer)))
}

parse.dimension.string <- function(s) {
  s <- strsplit(s, ",", fixed = TRUE)[[1]]
  return(sapply(s, as.numeric))
}

parse.group.string <- function(s) {
  output <- list()
  for (g in strsplit(s, ";", fixed = TRUE)[[1]]) {
    group <- NULL
    for (v in strsplit(g, ",", fixed = TRUE)[[1]]) {
      value <- strsplit(v, "-", fixed = TRUE)[[1]]
      if (length(value) == 0) {
        next
      } else if (length(value) == 1) {
        if (string.is.positive.integer(value)) {
          group <- c(group, as.numeric(value))
        } else if (trimws(value) == "") {
          next
        } else {
          return(NA)
        }
      } else if (length(value) == 2 && all(sapply(value, string.is.positive.integer))) {
        v1 <- as.numeric(value[1])
        v2 <- as.numeric(value[2])
        if (v1 > v2) {
          return(NA)
        }
        group <- c(group, v1:v2)
      } else {
        return(NA)
      }
    }
    if (length(group) > 0) {
      output <- c(output, list(sort(unique(group))))
    }
  }
  if (anyDuplicated(unlist(output)) > 0) {
    return(NA)
  }
  return(output)
}

group.label <- function(n, group) {
  label <- numeric(n)
  for (g in seq_along(group)) {
    label[group[[g]]] <- g
  }
  num.missing <- sum(label == 0)
  label[label == 0] <- g + 1:num.missing
  output <- numeric(n)
  group.order <- label[!duplicated(label)]
  for (i in seq_along(group.order)) {
    output[label == group.order[i]] <- i
  }
  return(output)
}

is.consecutive.group <- function(group) {
  is.consecutive <- logical(length(group))
  for (i in seq_along(group)) {
    g <- group[[i]]
    if (length(g) == diff(range(g)) + 1) {
      is.consecutive[i] <- TRUE
    }
  }
  return(is.consecutive)
}

collapse.data <- function(data, row.group = NULL, col.group = NULL, as.data.metrix = FALSE) {
  if (is.data.frame(data)) {
    m <- data.matrix(data)
  } else {
    m <- data
  }
  is.collapsed <- FALSE
  if (length(row.group) > 0) {
    row.label <- group.label(nrow(m), row.group)
    if (any(row.label != seq_len(nrow(m)))) {
      num.label <- max(row.label)
      m.new <- matrix(0, nrow = num.label, ncol = ncol(m))
      for (i in seq_len(num.label)) {
        m.new[i,] <- colSums(m[row.label == i,,drop = FALSE])
      }
      m <- m.new
      is.collapsed <- TRUE
    }
  }
  if (length(col.group) > 0) {
    col.label <- group.label(ncol(m), col.group)
    if (any(col.label != seq_len(ncol(m)))) {
      num.label <- max(col.label)
      m.new <- matrix(0, nrow = nrow(m), ncol = num.label)
      for (j in seq_len(num.label)) {
        m.new[,j] <- rowSums(m[,col.label == j,drop = FALSE])
      }
      m <- m.new
      is.collapsed <- TRUE
    }
  }
  if (is.collapsed) {
    if (is.data.frame(data) | as.data.metrix) {
      data <- matrix.data(m)
    } else {
      data <- (m)
    }
  }
  return(data)
}

#r 4.0.0 can not convert charcter to numeric when one use data.me
data.numeric.matrix <- function(x){
  m <- sapply(x, as.numeric)
  row.names(m) <- row.names(x)
  colnames(m) <- colnames(x)
  return(data.matrix(m))
}

matrix.string <- function(m, spacing = 2, left.justified = TRUE) {
  m[] <- as.character(m)
  m[is.na(m)] <- "NA"
  for (j in seq_len(ncol(m))) {
    longest.entry <- max(nchar(m[,j]))
    if (left.justified) {
      for (i in seq_len(nrow(m))) {
        m[i,j] <- paste0(m[i,j], space(longest.entry - nchar(m[i,j])))
      }
    } else {
      for (i in seq_len(nrow(m))) {
        m[i,j] <- paste0(space(longest.entry - nchar(m[i,j])), m[i,j])
      }
    }
  }
  for (j in seq_len(ncol(m) - 1)) {
    for (i in seq_len(nrow(m))) {
      m[i,j] <- paste0(m[i,j], space(spacing))
    }
  }
  return(paste(apply(m, 1, paste, collapse=""), collapse="\n"))
}

space <- function(n) {
  return(paste(rep(" ", n), collapse=""))
}

# Independence logic
independence.expected.count <- function(m) {
  row.total <- rowSums(m)
  col.total <- colSums(m)
  total <- sum(row.total)
  return(outer(row.total, col.total / total))
}

independence.goodman.kruskal.tau <- function(m, by.col.given.row = TRUE) {
  if (!by.col.given.row) {
    m = t(m)
  }
  row.total = rowSums(m)
  col.total = colSums(m)
  total = sum(col.total)
  Q.col = 1 - sum((col.total / total) ^ 2)
  Q.col.given.row = sum((1 - rowSums(sweep(m, 1, row.total, "/") ^ 2)) *
                          (row.total / total))
  return(1 - Q.col.given.row / Q.col) 
}

independence.pearson.chi.sq <- function(observed, expected) {
  return(sum((observed - expected) ^ 2 / expected))
}

# Compute the L^2 likelihood ratio Chi-squared statistic
independence.l.sq.chi.sq <- function(observed, expected) {
  return(2 * sum(observed * log(observed / expected)))
}

independence.pearson.residuals <- function(observed, expected) {
  return((observed - expected) / sqrt(expected))
}

independence.num.signif.residual <- function(residual, threshold = 1.64) {
  return(sum(abs(residual) > threshold))
}

independence.independence.computation <- function(data) {
  original <- data
  data[data == 0] <- 0.5
  
  observed <- data
  expected <- independence.expected.count(data)
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- (nrow(data) - 1) * (ncol(data) - 1)
  p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE)
  model.is.fit <- p.value > 0.05
  std.residual <- independence.pearson.residuals(observed, expected)
  num.signif.residual <- independence.num.signif.residual(std.residual)
  
  return(list(original = original,
              observed = observed,
              expected = expected,
              tau.given.row = tau.given.row,
              tau.given.col = tau.given.col,
              chi.sq.stat = chi.sq.stat,
              l.sq.stat = l.sq.stat,
              d.o.f = d.o.f,
              p.value = p.value,
              model.is.fit = model.is.fit,
              std.residual = std.residual,
              num.signif.residual = num.signif.residual))
}

independence.independence.report <- function(computed) {
  s <- ""
  s <- paste0(s, "Independence Model Result\n")
  s <- paste0(s, "=========================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(computed$original, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Expected Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(computed$expected, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("TAU(Y|X)= ", signif(computed$tau.given.row, 4),
                        "  TAU(X|Y)= ", signif(computed$tau.given.col, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("X-Square= ", round(computed$chi.sq.stat, 2),
                        "  L-Square= ", round(computed$l.sq.stat, 2),
                        "  D.O.F.= ", computed$d.o.f,
                        "  p-value= ", signif(computed$p.value, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Model Diagnostics\n")
  s <- paste0(s, "-----------------\n")
  if (computed$model.is.fit) {
    s <- paste0(s, "Model fits the observed data.\n")
  } else {
    s <- paste0(s, "Model does not fit the observed data.\n")
  }
  s <- paste0(s, "\n")
  s <- paste0(s, "Standardized Residuals\n")
  s <- paste0(s, "----------------------\n")
  s <- paste0(s, matrix.string(round(computed$std.residual, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("Number of significant residuals = ", computed$num.signif.residual))
  return(s)
}

independence.anoas.computation <- function(data, group, is.row) {
  original <- data
  data[data == 0] <- 0.5
  
  full.computed <- independence.independence.computation(data)
  collapsed <- independence.anoas.collapse(data, group, is.row)
  group.computed <- list()
  for (m in collapsed$non.single.data) {
    group.computed <- c(group.computed, list(independence.independence.computation(m)))
  }
  collapsed.computed <- independence.independence.computation(collapsed$collapsed.data)
  collapsed.original <- independence.anoas.collapse(original, group, is.row)$collapsed.data
  
  change.in.l.sq <- full.computed$l.sq.stat - collapsed.computed$l.sq.stat
  change.in.d.o.f <- full.computed$d.o.f - collapsed.computed$d.o.f
  information.p.value <- pchisq(change.in.l.sq, df=change.in.d.o.f, lower.tail=FALSE)
  information.is.lost <- information.p.value <= 0.05
  
  return(list(original = original,
              data = data,
              is.row = is.row,
              full.computed=full.computed,
              non.single.group = collapsed$non.single.group,
              group.computed = group.computed,
              collapsed.computed = collapsed.computed,
              collapsed.original = collapsed.original,
              change.in.l.sq = change.in.l.sq,
              change.in.d.o.f = change.in.d.o.f,
              information.p.value = information.p.value,
              information.is.lost = information.is.lost))
}

independence.anoas.collapse <- function(data, group, is.row) {
  label <- group.label(ifelse(is.row, nrow(data), ncol(data)), group)
  group <- list()
  for (i in seq_len(max(label))) {
    group[[i]] <- which(label == i)
  }
  non.single.data <- list()
  non.single.group <- list()
  for (g in group) {
    if (length(g) > 1) {
      if (is.row) {
        non.single.data <- c(non.single.data, list(data[g,,drop = FALSE]))
      } else {
        non.single.data <- c(non.single.data, list(data[,g,drop = FALSE]))
      }
      non.single.group <- c(non.single.group, list(g))
    }
  }
  if (is.row) {
    collapsed.data <- collapse.data(data, row.group = group)
  } else {
    collapsed.data <- collapse.data(data, col.group = group)
  }
  return(list(non.single.data = non.single.data,
              non.single.group = non.single.group,
              collapsed.data = collapsed.data))
}

independence.anoas.report <- function(computed) {
  s <- ""
  s <- paste0(s, "ANOAS Model Result\n")
  s <- paste0(s, "==================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(computed$original, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("TAU(Y|X)= ", signif(computed$full.computed$tau.given.row, 4),
                        "  TAU(X|Y)= ", signif(computed$full.computed$tau.given.col, 4)))
  s <- paste0(s, "\n\n")
  if (computed$is.row) {
    s <- paste0(s, "Association of Rows\n")
    s <- paste0(s, "-------------------\n")
  } else {
    s <- paste0(s, "Association of Columns\n")
    s <- paste0(s, "----------------------\n")
  }
  s <- paste0(s, "\n")
  # Decomposition table
  num.group <- length(computed$non.single.group)
  temp <- matrix("", 2 + num.group + 2, 6)
  temp[1,1] <- "Sub"
  temp[2,1] <- "==="
  temp[1,2] <- "L Stats"
  temp[2,2] <- "======="
  temp[1,3] <- "X Stats"
  temp[2,3] <- "======="
  temp[1,4] <- "DOF"
  temp[2,4] <- "==="
  temp[1,5] <- "p-value"
  temp[2,5] <- "======="
  temp[1,6] <- "Interpretation"
  temp[2,6] <- "=============="
  if (num.group > 0) {
    for (g in seq_len(num.group)) {
      temp[2 + g,1] <- paste0("S", g)
      temp[2 + g,2] <- round(computed$group.computed[[g]]$l.sq.stat, 2)
      temp[2 + g,3] <- round(computed$group.computed[[g]]$chi.sq.stat, 2)
      temp[2 + g,4] <- computed$group.computed[[g]]$d.o.f
      temp[2 + g,5] <- signif(computed$group.computed[[g]]$p.value, 4)
      temp[2 + g,6] <- paste0("within {", paste(computed$non.single.group[[g]],
                                                collapse = ","), "}")
    }
  }
  temp[2 + num.group + 1,1] <- "Mk"
  temp[2 + num.group + 1,2] <- round(computed$collapsed.computed$l.sq.stat, 2)
  temp[2 + num.group + 1,3] <- round(computed$collapsed.computed$chi.sq.stat, 2)
  temp[2 + num.group + 1,4] <- computed$collapsed.computed$d.o.f
  temp[2 + num.group + 1,5] <- signif(computed$collapsed.computed$p.value, 4)
  temp[2 + num.group + 1,6] <- "between subsets"
  temp[2 + num.group + 2,1] <- "N"
  temp[2 + num.group + 2,2] <- round(computed$full.computed$l.sq.stat, 2)
  temp[2 + num.group + 2,3] <- round(computed$full.computed$chi.sq.stat, 2)
  temp[2 + num.group + 2,4] <- computed$full.computed$d.o.f
  temp[2 + num.group + 2,5] <- signif(computed$full.computed$p.value, 4)
  temp[2 + num.group + 2,6] <- "overall"
  s <- paste0(s, matrix.string(temp, spacing = 3))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Information Loss\n")
  s <- paste0(s, "----------------\n")
  s <- paste0(s, paste0("L^2(N) - L^2(Mk) = ", round(computed$change.in.l.sq, 2)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Information loss D.O.F. = ", computed$change.in.d.o.f))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Information p-value = ", signif(computed$information.p.value, 4)))
  s <- paste0(s, "\n")
  if (computed$information.is.lost) {
    s <- paste0(s, "Collapsing leads to information loss.\n")
  } else {
    s <- paste0(s, "Collapsing does not lead to information loss.\n")
  }
  s <- paste0(s, "\n")
  s <- paste0(s, "Mk Collapsed Matrix\n")
  s <- paste0(s, "-------------------\n")
  s <- paste0(s, matrix.string(round(computed$collapsed.original, 2),
                               left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Expected Collapsed Matrix\n")
  s <- paste0(s, "-------------------------\n")
  s <- paste0(s, matrix.string(round(computed$collapsed.computed$expected, 2),
                               left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Standardized Residuals\n")
  s <- paste0(s, "----------------------\n")
  s <- paste0(s, matrix.string(round(computed$collapsed.computed$std.residual, 2),
                               left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("Number of significant residuals = ",
                        computed$collapsed.computed$num.signif.residual))
  return(s)
}

independence.predictors.proportion.report <- function(data) {
  s <- ""
  s <- paste0(s, "Predictors' Proportion Result\n")
  s <- paste0(s, "=============================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(data, 2), left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Observed Proportion (Per Row)\n")
  s <- paste0(s, "-----------------------------\n")
  row.prop <- prop.table(data, margin = 1)
  row.prop[is.nan(row.prop)] <- 0
  s <- paste0(s, matrix.string(round(row.prop, 4), left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Observed Proportion (Per Column)\n")
  s <- paste0(s, "--------------------------------\n")
  col.prop <- prop.table(data, margin = 2)
  col.prop[is.nan(col.prop)] <- 0
  s <- paste0(s, matrix.string(round(col.prop, 4), left.justified = FALSE))
  return(s)
}

independence.ztest.computation <- function(data, which.two, by.row) {
  original <- data
  data[data == 0] <- 0.5
  # Find highest and lowest residuals in the two relevant rows (or columns)
  indep.computed <- independence.independence.computation(data)
  high.idx <- numeric(2)
  high.n <- numeric(2)
  low.idx <- numeric(2)
  low.n <- numeric(2)
  if (by.row) {
    for (i in seq_len(2)) {
      high.idx[i] <- which.max(indep.computed$std.residual[which.two[i],])
      high.n[i] <- data[which.two[i],high.idx[i]]
      low.idx[i] <- which.min(indep.computed$std.residual[which.two[i],])
      low.n[i] <- data[which.two[i],low.idx[i]]
    }
  } else {
    for (i in seq_len(2)) {
      high.idx[i] <- which.max(indep.computed$std.residual[,which.two[i]])
      high.n[i] <- data[high.idx[i],which.two[i]]
      low.idx[i] <- which.min(indep.computed$std.residual[,which.two[i]])
      low.n[i] <- data[low.idx[i],which.two[i]]
    }
  }
  # Compute odds and z-value
  odd <- high.n / low.n
  z.value <- abs(diff(log(odd))) / sqrt(sum(1 / high.n) + sum(1 / low.n))
  is.significant <- z.value > 1.645
  
  return(list(original = original,
              data = data,
              which.two = which.two,
              by.row = by.row,
              indep.computed = indep.computed,
              high.idx = high.idx,
              high.n = high.n,
              low.idx = low.idx,
              low.n = low.n,
              odd = odd,
              z.value = z.value,
              is.significant = is.significant))
}

independence.ztest.report <- function(computed) {
  s <- ""
  s <- paste0(s, "Log-Odds Z-Test Result\n")
  s <- paste0(s, "======================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(computed$original, 2), left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Expected Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(computed$indep.computed$expected, 2), left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Standardized Residuals\n")
  s <- paste0(s, "----------------------\n")
  s <- paste0(s, matrix.string(round(computed$indep.computed$std.residual, 2), left.justified = FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Odds\n")
  s <- paste0(s, "----\n")
  odd.text <- matrix("", 8, 3)
  if (computed$by.row) {
    for (i in seq_len(2)) {
      odd.text[(i - 1) * 4 + 1,1] <- paste0("Row ", computed$which.two[i], ":")
      odd.text[(i - 1) * 4 + 1,2] <- paste0("Largest residual= ",
                                            round(computed$indep.computed$std.residual[computed$which.two[i],computed$high.idx[i]], 2))
      odd.text[(i - 1) * 4 + 2,2] <- paste0("Column= ", computed$high.idx[i])
      odd.text[(i - 1) * 4 + 3,2] <- paste0("Observed data= ",
                                            computed$original[computed$which.two[i],computed$high.idx[i]])
      odd.text[(i - 1) * 4 + 1,3] <- paste0("Smallest residual= ",
                                            round(computed$indep.computed$std.residual[computed$which.two[i],computed$low.idx[i]], 2))
      odd.text[(i - 1) * 4 + 2,3] <- paste0("Column= ", computed$low.idx[i])
      odd.text[(i - 1) * 4 + 3,3] <- paste0("Observed data= ",
                                            computed$original[computed$which.two[i],computed$low.idx[i]])
      odd.text[(i - 1) * 4 + 4,2] <- paste0("Odd= ", round(computed$odd[i], 4))
    }
  } else {
    for (i in seq_len(2)) {
      odd.text[(i - 1) * 4 + 1,1] <- paste0("Column ", computed$which.two[i], ":")
      odd.text[(i - 1) * 4 + 1,2] <- paste0("Largest residual= ",
                                            round(computed$indep.computed$std.residual[computed$high.idx[i],computed$which.two[i]], 2))
      odd.text[(i - 1) * 4 + 2,2] <- paste0("Row= ", computed$high.idx[i])
      odd.text[(i - 1) * 4 + 3,2] <- paste0("Observed data= ",
                                            computed$original[computed$high.idx[i],computed$which.two[i]])
      odd.text[(i - 1) * 4 + 1,3] <- paste0("Smallest residual= ",
                                            round(computed$indep.computed$std.residual[computed$low.idx[i],computed$which.two[i]], 2))
      odd.text[(i - 1) * 4 + 2,3] <- paste0("Row= ", computed$low.idx[i])
      odd.text[(i - 1) * 4 + 3,3] <- paste0("Observed data= ",
                                            computed$original[computed$low.idx[i],computed$which.two[i]])
      odd.text[(i - 1) * 4 + 4,2] <- paste0("Odd= ", round(computed$odd[i], 4))
    }
  }
  for (i in seq_len(2)) {
    s <- paste0(s, matrix.string(odd.text[(i - 1) * 4 + 1:4,], spacing = 3))
    s <- paste0(s, "\n\n")
  }
  s <- paste0(s, "Log-Odds Z-Test\n")
  s <- paste0(s, "---------------\n")
  s <- paste0(s, "z-value= ", round(computed$z.value, 4), "\n")
  s <- paste0(s, "\n")
  if (computed$is.significant) {
    s <- paste0(s, "The two odds differ significantly.")
  } else {
    s <- paste0(s, "The two odds do not differ significantly.")
  }
  return(s)
}

independence.chisq.test.computation <- function(data, row.group, col.group) {
  full.computed <- independence.independence.computation(data)
  collapsed <- collapse.data(data, row.group, col.group)
  collapsed.computed <- independence.independence.computation(collapsed)
  return(list(full.computed = full.computed,
              collapsed.computed = collapsed.computed))
}

independence.chisq.test.report <- function(computed) {
  s <- ""
  s <- paste0(s, "Collapse Chi-Sq Test Result\n")
  s <- paste0(s, "===========================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix (Original)\n")
  s <- paste0(s, "-------------------------------\n")
  s <- paste0(s, matrix.string(round(computed$full.computed$original, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Expected Data Matrix (Original)\n")
  s <- paste0(s, "-------------------------------\n")
  s <- paste0(s, matrix.string(round(computed$full.computed$expected, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("TAU(Y|X)= ", signif(computed$full.computed$tau.given.row, 4),
                        "  TAU(X|Y)= ", signif(computed$full.computed$tau.given.col, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("X-Square= ", round(computed$full.computed$chi.sq.stat, 2),
                        "  L-Square= ", round(computed$full.computed$l.sq.stat, 2),
                        "  D.O.F.= ", computed$full.computed$d.o.f,
                        "  p-value= ", signif(computed$full.computed$p.value, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Observed Data Matrix (Collapsed)\n")
  s <- paste0(s, "--------------------------------\n")
  s <- paste0(s, matrix.string(round(computed$collapsed.computed$original, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Expected Data Matrix (Collapsed)\n")
  s <- paste0(s, "--------------------------------\n")
  s <- paste0(s, matrix.string(round(computed$collapsed.computed$expected, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("TAU(Y|X)= ", signif(computed$collapsed.computed$tau.given.row, 4),
                        "  TAU(X|Y)= ", signif(computed$collapsed.computed$tau.given.col, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("X-Square= ", round(computed$collapsed.computed$chi.sq.stat, 2),
                        "  L-Square= ", round(computed$collapsed.computed$l.sq.stat, 2),
                        "  D.O.F.= ", computed$collapsed.computed$d.o.f,
                        "  p-value= ", signif(computed$collapsed.computed$p.value, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Standardized Residuals (Collapsed)\n")
  s <- paste0(s, "----------------------------------\n")
  s <- paste0(s, matrix.string(round(computed$collapsed.computed$std.residual, 2), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("Number of significant residuals = ",
                        computed$collapsed.computed$num.signif.residual))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Chi-Squared Test for Loss of Information\n")
  s <- paste0(s, "----------------------------------------\n")
  diff.l.sq.stat <- computed$full.computed$l.sq.stat - computed$collapsed.computed$l.sq.stat
  diff.d.o.f <- computed$full.computed$d.o.f - computed$collapsed.computed$d.o.f
  p.value <- pchisq(diff.l.sq.stat, df=diff.d.o.f, lower.tail=FALSE)
  s <- paste0(s, "Diff in L-Square= ", round(diff.l.sq.stat, 2), "\n")
  s <- paste0(s, "Diff in D.O.F.= ", diff.d.o.f, "\n")
  s <- paste0(s, "p-value= ", signif(p.value, 4), "\n")
  s <- paste0(s, "\n")
  if (p.value > 0.05) {
    s <- paste0(s, "The collapsing does not entail a loss of information.\n")
  } else {
    s <- paste0(s, "The collapsing entails a loss of information.\n")
  }
  return(s)
}

# 3-Dimensional logic
array.data <- function(a) {
  df <- data.frame(as.character(a), stringsAsFactors = FALSE)
  dim.a <- dim(a)
  if (is.null(dim.a)) {
    dim.a <- length(a)
  }
  d <- length(dim.a)
  row.name <- NULL
  idx <- c(0, rep(1, d - 1))
  while (TRUE) {
    for (i in seq_len(d)) {
      if (idx[i] < dim.a[i]) {
        break
      }
    }
    if (idx[i] == dim.a[i]) {
      break
    }
    idx[i] <- idx[i] + 1
    for (j in seq_len(i - 1)) {
      idx[j] <- 1
    }
    row.name <- c(row.name, paste(idx, collapse = ","))
  }
  dimnames(df) <- list(row.name, c("Value"))
  return(df)
}

three.dimensional.loglin.computation <- function(data, which.x,
                                                 p.value.threshold = 0.05) {
  dimnames(data) <- NULL
  
  observed <- data
  if (length(which.x) == 0) {
    expected <- rep(mean(observed), prod(dim(observed)))
    dim(expected) <- dim(observed)
    # Compute the L^2 likelihood ratio Chi-squared statistic given observed
    # and expected counts
    l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
    d.o.f <- prod(dim(observed)) - 1
    lambda.coef <- list()
    lambda.coef$'(Intercept)' <- log(mean(observed)) - log(sum(observed))
  } else {
    fitted.model <- loglin(data, which.x, fit = TRUE, param = TRUE,
                           print = FALSE)
    # browser()
    expected <- fitted.model$fit
    l.sq.stat <- fitted.model$lrt
    d.o.f <- fitted.model$df
    lambda.coef <- fitted.model$param
    lambda.coef$'(Intercept)' <- lambda.coef$'(Intercept)' - log(sum(data))
  }
  p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE)
  log.p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE, log.p = TRUE)
  model.is.fit <- p.value > p.value.threshold    
  std.residual <- independence.pearson.residuals(observed, expected)
  num.signif.residual <- independence.num.signif.residual(std.residual)
  # browser()
  return(list(observed = observed,
              expected = expected,
              l.sq.stat = l.sq.stat,
              d.o.f = d.o.f,
              p.value = p.value,
              log.p.value = log.p.value,
              model.is.fit = model.is.fit,
              std.residual = std.residual,
              num.signif.residual = num.signif.residual,
              lambda.coef = lambda.coef))
}

three.dimensional.all.coef.computation <- function(data) {
  original <- data
  data[data == 0] <- 0.5
  model <- list(three.dimensional.loglin.computation(data, list(c(1,2), c(1,3), c(2,3))),
                three.dimensional.loglin.computation(data, list(c(1,2), c(1,3))),
                three.dimensional.loglin.computation(data, list(c(1,2), c(2,3))),
                three.dimensional.loglin.computation(data, list(c(1,3), c(2,3))),
                three.dimensional.loglin.computation(data, list(3, c(1,2))),
                three.dimensional.loglin.computation(data, list(2, c(1,3))),
                three.dimensional.loglin.computation(data, list(1, c(2,3))),
                three.dimensional.loglin.computation(data, list(1, 2, 3)))
  coef.text <- list("Intercept X Y Z (X,Y) (X,Z) (Y,Z)",
                    "Intercept X Y Z (X,Y) (X,Z)",
                    "Intercept X Y Z (X,Y) (Y,Z)",
                    "Intercept X Y Z (X,Z) (Y,Z)",
                    "Intercept X Y Z (X,Y)",
                    "Intercept X Y Z (X,Z)",
                    "Intercept X Y Z (Y,Z)",
                    "Intercept X Y Z")
  return(list(model = model,
              coef.text = coef.text))
}

model.selection <- function(models, p.value.threshold=0.05) {
  highest.tier <- max(sapply(models, function(m) m$tier))
  for (m in models) {
    if (m$tier == highest.tier) {
      p.value <- pchisq(m$l.sq.stat, df=m$d.o.f, lower.tail=FALSE)
      if (p.value <= p.value.threshold) {
        return(NULL)
      } else {
        highest.l.sq.stat <- m$l.sq.stat
        highest.d.o.f <- m$d.o.f
        break
      }
    }
  }
  best.tier <- Inf
  for (m in models) {
    p1 <- pchisq(m$l.sq.stat, df=m$d.o.f, lower.tail=FALSE)
    p2 <- pchisq(m$l.sq.stat - highest.l.sq.stat, df=m$d.o.f - highest.d.o.f, lower.tail=FALSE)
    if (p1 > p.value.threshold && p2 > p.value.threshold) {
      if (m$tier < best.tier || m$tier == best.tier && p1 > best.p.value) {
        best.tier <- m$tier
        best.p.value <- p1
        best.name <- m$model.name
      }
    }
  }
  return(best.name)
}

three.dimensional.all.coef.report <- function(computed) {
  s <- ""
  s <- paste0(s, "3-Dimensional Model Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")    
  s <- paste0(s, "List of Possible Models\n")
  s <- paste0(s, "-----------------------\n")
  dof.space <- 6
  l.sq.space <- 10
  indent <- space(4)
  tiers <- c(3, 2, 2, 2, 1, 1, 1, 0)
  models <- list()
  for (i in seq_len(8)) {
    s <- paste0(s, "Lambda coefficients: ", computed$coef.text[[i]], "\n")
    s <- paste0(s, indent, "D.O.F. = ", paste0(computed$model[[i]]$d.o.f,
                                               space(max(0, dof.space - nchar(computed$model[[i]]$d.o.f)))),
                "L-Squared = ", paste0(round(computed$model[[i]]$l.sq.stat, 2),
                                       space(max(0, l.sq.space - nchar(round(computed$model[[i]]$l.sq.stat, 2))))),
                "p-value = ", signif(computed$model[[i]]$p.value, 4), "\n")
    if (computed$model[[i]]$model.is.fit) {
      s <- paste0(s, indent, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, indent, "Model does not fit the observed data.\n")
    }
    if (i < 8) {
      s <- paste0(s, "\n")
    }
    models[[length(models) + 1]] <- list(model.name=computed$coef.text[[i]],
                                         l.sq.stat=computed$model[[i]]$l.sq.stat, d.o.f=computed$model[[i]]$d.o.f,
                                         tier=tiers[i])
  }
  return(s)
}

three.dimensional.select.coef.computation <- function(data, which.x) {
  original <- data
  data[data == 0] <- 0.5
  
  model <- three.dimensional.loglin.computation(data, which.x)
  return(list(original = original,
              data = data,
              which.x = which.x,
              model = model))
}

array.string <- function(a) {
  df <- array.data(a)
  row.name <- rownames(df)
  s <- ""
  for (i in seq_len(nrow(df))) {
    s <- paste0(s, gsub (",", " ", row.name[i]), " : ", df[i,1],"\n")
  }
  return(s)
}

three.dimensional.coef.string <- function(coef) {
  s <- c()
  coef.name <- c("X", "Y", "Z")
  for (g in coef) {
    s.g <- paste(coef.name[g], collapse = ",")
    if (length(g) > 1) {
      s.g <- paste0("(", s.g, ")")
    }
    s <- c(s, s.g)
  }
  return(paste(s, collapse = " "))
}

three.dimensional.select.coef.report <- function(computed) {
  s <- ""
  s <- paste0(s, "3-Dimensional Model Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Results for the selected model:\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, array.string(computed$original))
  s <- paste0(s, "\n")
  s <- paste0(s, "Expected Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, array.string(round(computed$model$expected, 2)))
  s <- paste0(s, "\n")
  s <- paste0(s, "L-Square = ", round(computed$model$l.sq.stat, 2),
              "  D.O.F. = ", computed$model$d.o.f,
              "  p-value = ", signif(computed$model$p.value, 4), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Information on Selected Model\n")
  s <- paste0(s, "-----------------------------\n")
  s <- paste0(s, "Lambda coefficients of the selected model: Intercept ",
              three.dimensional.coef.string(computed$which.x), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Coefficient of \"Intercept\":\n")
  s <- paste0(s, array.string(round(computed$model$lambda.coef$'(Intercept)', 4)))
  for (g in computed$which.x) {
    s <- paste0(s, "\n")
    s <- paste0(s, "Coefficient of \"", three.dimensional.coef.string(list(g)), "\":\n")
    s <- paste0(s, array.string(round(computed$model$lambda.coef[[paste(g, collapse = ".")]], 4)))
  }
  s <- paste0(s, "\n")
  s <- paste0(s, "Model Diagnostics\n")
  s <- paste0(s, "-----------------\n")
  if (computed$model$model.is.fit) {
    s <- paste0(s, "Model fits the observed data.\n")
  } else {
    s <- paste0(s, "Model does not fit the observed data.\n")
  }
  s <- paste0(s, "\n")
  s <- paste0(s, "Standardized Residuals\n")
  s <- paste0(s, "----------------------\n")
  s <- paste0(s, array.string(round(computed$model$std.residual, 2)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Number of significant residuals = ",
                        computed$model$num.signif.residual))
  return(s)
}

three.dimensional.target.computation <- function(data, target) {
  original <- data
  data[data == 0] <- 0.5
  
  which.x <- list(1, 2, 3, c(1,2), c(1,3), c(2,3))
  model <- three.dimensional.loglin.computation(data, which.x)
  # Compute Mu
  target.mu <- -diff(model$lambda.coef[[as.character(target)]])
  # browser()
  # Compute Mu of pairs
  explanatory.variables <- setdiff(1:3, target)
  explanatory.mu <- list()
  for (v in seq_len(2)) {
    # browser()
    this.explantory.variable <- explanatory.variables[v]
    pair <- sort(c(this.explantory.variable, target))
    pair.lambda.coef <- model$lambda.coef[[paste(pair, collapse = ".")]]
    if (this.explantory.variable < target) {
      pair.mu <- pair.lambda.coef[,1] - pair.lambda.coef[,2]
    } else {
      pair.mu <- pair.lambda.coef[1,] - pair.lambda.coef[2,]
    }
    explanatory.mu[[v]] <- pair.mu
  }
  # Odds matrix
  
  odd.matrix <- exp(target.mu +
                      outer(explanatory.mu[[1]], explanatory.mu[[2]], "+"))
  propensity.matrix <- round(odd.matrix / (odd.matrix + 1), 4)
  epsilon <- as.numeric(paste0("1e-", 4))
  propensity.matrix[propensity.matrix < epsilon] <- epsilon
  propensity.matrix[propensity.matrix > 1 - epsilon] <- 1 - epsilon
  freq.matrix <- apply(original, explanatory.variables, sum)
  
  return(list(original = original,
              data = data,
              which.x = which.x,
              model = model,
              target = target,
              target.mu = target.mu,
              explanatory.variables = explanatory.variables,
              explanatory.mu = explanatory.mu,
              odd.matrix = odd.matrix,
              propensity.matrix = propensity.matrix,
              freq.matrix = freq.matrix))
}


three.dimensional.named.matrix <- function(m, num.decimal.place, row.name,
                                           col.name) {
  num.row <- nrow(m)
  num.col <- ncol(m)
  named.m <- matrix("", num.row + 1, num.col + 1)
  for (i in seq_len(num.row)) {
    named.m[i+1,1] <- paste0(row.name, "=", i)
  }
  for (j in seq_len(num.col)) {
    named.m[1,j+1] <- paste0(col.name, "=", j)
    for (i in seq_len(num.row)) {
      named.m[i+1,j+1] <- round(m[i,j], num.decimal.place)
    }
  }
  return(named.m)
}

three.dimensional.target.report <- function(computed) {
  s <- ""
    s <- paste0(s, "3-Dimensional Model Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Results for the selected model:\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, array.string(computed$original))
  s <- paste0(s, "\n")
  s <- paste0(s, "Expected Data Matrix\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, array.string(round(computed$model$expected, 2)))
  s <- paste0(s, "\n")
  s <- paste0(s, "L-Square = ", round(computed$model$l.sq.stat, 2),
              "  D.O.F. = ", computed$model$d.o.f,
              "  p-value = ", signif(computed$model$p.value, 4), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Information on Selected Model\n")
  s <- paste0(s, "-----------------------------\n")
  s <- paste0(s, "Lambda coefficients of the selected model: Intercept ",
              three.dimensional.coef.string(computed$which.x), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Coefficient of \"Intercept\":\n")
  s <- paste0(s, array.string(round(computed$model$lambda.coef$'(Intercept)', 4)))
  for (g in computed$which.x) {
    s <- paste0(s, "\n")
    s <- paste0(s, "Coefficient of \"", three.dimensional.coef.string(list(g)), "\":\n")
    s <- paste0(s, array.string(round(computed$model$lambda.coef[[paste(g, collapse=".")]], 4)))
  }
  s <- paste0(s, "\n")
  # Mu
  all.variable.name <- c("X", "Y", "Z")
  s <- paste0(s, "Target variable: ",  all.variable.name[computed$target], "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Mu's for Target Variable:\n")
  s <- paste0(s, "-------------------------\n")
  s <- paste0(s, "Mu(Intercept) = ", round(computed$target.mu, 4), "\n")
  s <- paste0(s, "\n")
  # Mu of pairs
  for (v in seq_len(2)) {
    this.explantory.variable <- computed$explanatory.variables[v]
    pair <- sort(c(this.explantory.variable, computed$target))
    for (i in seq_along(computed$explanatory.mu[[v]])) {
      s <- paste0(s, "Mu(", paste(all.variable.name[pair], collapse=","), ")(",
                  all.variable.name[computed$explanatory.variables[v]], "=", i,
                  ") = ", round(computed$explanatory.mu[[v]][i], 4), "\n")
    }
    s <- paste0(s, "\n")
  }
  # Odds matrix
  row.var.name <- all.variable.name[computed$explanatory.variables[1]]
  col.var.name <- all.variable.name[computed$explanatory.variables[2]]
  s <- paste0(s, "Odds Matrix (Row = ", row.var.name, ", column = ",
              col.var.name, ")\n")
  s <- paste0(s, "---------------------------------\n")
  s <- paste0(s, matrix.string(three.dimensional.named.matrix(
    computed$odd.matrix, 4, row.var.name, col.var.name)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Propensity Matrix (Row = ", row.var.name, ", column = ",
              col.var.name, ")\n")
  s <- paste0(s, "---------------------------------------\n")
  s <- paste0(s, matrix.string(three.dimensional.named.matrix(
    computed$propensity.matrix, 4, row.var.name, col.var.name)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Frequency Matrix for Explanatory Variables (Row = ",
              row.var.name, ", column = ", col.var.name, ")\n")
  s <- paste0(s, "----------------------------------------------------------------\n")
  s <- paste0(s, matrix.string(three.dimensional.named.matrix(
    computed$freq.matrix, 2, row.var.name, col.var.name)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Model Diagnostics\n")
  s <- paste0(s, "-----------------\n")
  if (computed$model$model.is.fit) {
    s <- paste0(s, "Model fits the observed data.\n")
  } else {
    s <- paste0(s, "Model does not fit the observed data.\n")
  }
  s <- paste0(s, "\n")
  s <- paste0(s, "Standardized Residuals\n")
  s <- paste0(s, "----------------------\n")
  s <- paste0(s, array.string(round(computed$model$std.residual, 2)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Number of significant residuals = ",
                        computed$model$num.signif.residual))
  return(s)
}

three.dimensional.model.value.computation <- function(data,
                                                      explanatory.variables, propensity.matrix, threshold, tp.reward, tn.reward,
                                                      fp.reward, fn.reward) {
  
  target <- setdiff(1:3, explanatory.variables)
  if (target == 1) {
    obs.one <- data[1,,]
    obs.two <- data[2,,]
  } else if (target == 2) {
    obs.one <- data[,1,]
    obs.two <- data[,2,]
  } else {
    obs.one <- data[,,1]
    obs.two <- data[,,2]
  }
  predict.is.one <- propensity.matrix >= threshold
  num.tp <- sum(obs.one[predict.is.one])
  num.fp <- sum(obs.two[predict.is.one])
  num.fn <- sum(obs.one[!predict.is.one])
  num.tn <- sum(obs.two[!predict.is.one])
  sensitivity <- num.tp / (num.tp + num.fn)
  specificity <- num.tn / (num.fp + num.tn)
  model.value <- tp.reward * num.tp + tn.reward * num.tn +
    fp.reward * num.fp + fn.reward * num.fn
  return(list(num.tp = num.tp,
              num.fp = num.fp,
              num.fn = num.fn,
              num.tn = num.tn,
              sensitivity = sensitivity,
              specificity = specificity,
              model.value = model.value))
}




threedimensional.sort.explanatory.variables <- function(all.data, target.col,
                                                        explanatory.col) {
  num.explanatory <- length(explanatory.col)
  if (num.explanatory == 1) {
    return(1)
  }
  selected.col <- numeric(num.explanatory)
  log.p.values <- numeric(num.explanatory)
  for (i in seq_len(num.explanatory)) {
    observed.i <- table(as.data.frame(all.data[,c(target.col,
                                                  explanatory.col[i])]))
    # browser()
    l.sq.stat <- independence.l.sq.chi.sq(observed.i,
                                          independence.expected.count (observed.i))
    d.o.f <- prod(dim(observed.i) - 1)
    log.p.values[i] <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE,
                              log.p=TRUE)
  }
  
  # DEBUG: Print initial selection
  print(paste("DEBUG R SORT: Initial log.p.values for vars", paste(explanatory.col, collapse=", "), ":", paste(log.p.values, collapse=", ")))
  init.idx <- which.min(log.p.values)
  print(paste("DEBUG R SORT: Selected initial variable", explanatory.col[init.idx], "(index", init.idx, ")"))
  
  selected.col[which.min(log.p.values)] <- 1
  encoded.var <- all.data[,explanatory.col[which(selected.col==1)]]
  
  coeff <- list(1, 2, 3, c(1,2), c(1,3), c(2,3))
  for (t in 2:num.explanatory) {
    log.p.values <- rep(-Inf, num.explanatory)
    for (i in seq_len(num.explanatory)) {
      if (selected.col[i] > 0) {
        next
      }
      
      observed.i <- table(as.data.frame(cbind(encoded.var,
                                              all.data[,target.col], all.data[,explanatory.col[i]])))

      # browser()
      log.lin.result <- three.dimensional.loglin.computation(observed.i,
                                                             coeff)
      log.p.values[i] <- log.lin.result$log.p.value
      print(paste("DEBUG R SORT iter", t, ": Var", explanatory.col[i], "shape=", paste(dim(observed.i), collapse="x"), "log_p=", sprintf("%.4f", log.p.values[i]), "p-value=", sprintf("%.4f", log.lin.result$p.value)))
    }
    next.idx <- which.max(log.p.values)
    selected.col[next.idx] <- t
    print(paste("DEBUG R SORT iter", t, ": Selected variable", explanatory.col[next.idx], "(index", next.idx, ") with log_p=", sprintf("%.4f", log.p.values[next.idx])))
    
    if (t < num.explanatory) {
      selected.var <- all.data[,explanatory.col[which(selected.col==t)]]
      num.new.category <- max(selected.var)
      encoded.var <- encoded.var * (num.new.category + 1) + selected.var
    }
  }
  
  # DEBUG: Print final ordering
  print(paste("DEBUG R SORT: selected.col array:", paste(selected.col, collapse=", ")))
  print(paste("DEBUG R SORT: order(selected.col):", paste(order(selected.col), collapse=", ")))
  final.order <- explanatory.col[order(selected.col)]
  print(paste("DEBUG R SORT: Final ordering:", paste(final.order, collapse=", ")))
  
  return(explanatory.col[order(selected.col)])
}

# N-dimensional logic
n.dimensional.indep.computation <- function(data, predictor.variable,
                                            target.variable) {
  
  predictor.variable <- sort(predictor.variable)
  all.variable <- sort(c(predictor.variable, target.variable))
  target.idx <- which(all.variable == target.variable)
  data <- data[,all.variable]
  
  p <- ncol(data)
  contingency.array <- table(as.data.frame(data))
  dimnames(contingency.array) <- NULL
  
  covariate.to.fit <- list()
  for (i in seq_len(p)) {
    if (i != target.idx) {
      covariate.to.fit <- c(covariate.to.fit, list(c(target.idx, i)))
    }
  }
  model <- loglin(contingency.array, covariate.to.fit, fit = TRUE, param = TRUE,
                  print = FALSE)
  l.sq.stat <- model$lrt
  d.o.f <- model$df
  p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE)
  lambda.coef <- model$param
  lambda.coef$'(Intercept)' <- lambda.coef$'(Intercept)' -
    log(sum(contingency.array))
  
  # Compute the mu's
  mu <- list()
  for (i in seq_len(p)) {
    if (i == target.idx) {
      mu[[i]] <- -diff(lambda.coef[[as.character(target.idx)]])
    } else {
      pair <- sort(c(i, target.idx))
      lambda.coef.i <- lambda.coef[[paste(pair, collapse = ".")]]
      if (is.null(lambda.coef.i)) {
        mu[[i]] <- NA
        next
      }
      if (i < target.idx) {
        mu[[i]] <- lambda.coef.i[,1] - lambda.coef.i[,2]
      } else {
        mu[[i]] <- lambda.coef.i[1,] - lambda.coef.i[2,]
      }
    }
  }
  return(list(data = data,
              all.variable = all.variable,
              predictor.variable = predictor.variable,
              target.variable = target.variable,
              target.idx = target.idx,
              p = p,
              l.sq.stat = l.sq.stat,
              d.o.f = d.o.f,
              p.value = p.value,
              lambda.coef = lambda.coef,
              mu = mu))
}

group.to.list <- function(group) {
  group <- sort(group)
  output <- list()
  i <- 1
  while (i <= length(group)) {
    j <- i
    while (j < length(group) && group[j + 1] == group[j] + 1) {
      j <- j + 1
    }
    if (j == i) {
      output <- c(output, group[i])
    } else {
      output <- c(output, list(c(group[i], group[j])))
    }
    i <- j + 1
  }
  return(output)
}

outer.multi <- function(x, ...) {
  output <- x[[1]]
  if (length(x) > 1)
    for (i in 2:length(x)) {
      output <- outer(output, x[[i]], ...)
    }
  return(output)
}

n.dimensional.indep.report <- function(computed) {
  s <- ""
  s <- paste0(s, "N-Dimensional Model Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")
  predictor.list <- group.to.list(computed$predictor.variable)
  predictor.text <- ""
  for (i in seq_along(predictor.list)) {
    if (i != 1) {
      predictor.text <- paste0(predictor.text, ", ")
    }
    if (length(predictor.list[[i]]) == 1) {
      predictor.text <- paste0(predictor.text, "X", predictor.list[[i]])
    } else {
      predictor.text <- paste0(predictor.text, "X", predictor.list[[i]][1],
                               " to X", predictor.list[[i]][2])
    }
  }
  s <- paste0(s, "Predictor variables: ", predictor.text, "\n")
  s <- paste0(s, "Target variable: X", computed$target.variable, "\n")
  s <- paste0(s, "Model assumption: Independence\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "L-Square = ", round(computed$l.sq.stat, 2),
              "  D.O.F. = ", computed$d.o.f,
              "  p-value = ", signif(computed$p.value, 4), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Lambda Coefficients\n")
  s <- paste0(s, "-------------------\n")
  s <- paste0(s, "Coefficient of \"Intercept\":\n")
  s <- paste0(s, array.string(round(computed$lambda.coef$'(Intercept)', 4)))
  # First-order terms
  for (i in seq_len(computed$p)) {
    s <- paste0(s, "\n")
    s <- paste0(s, "Coefficient of \"X", computed$all.variable[i], "\":\n")
    temp.l <- computed$lambda.coef[[as.character(i)]]
    if (is.null(temp.l)) {
      s <- paste0(s, "NA\n")
    } else {
      s <- paste0(s, array.string(round(temp.l, 4)))
    }
  }
  # Second-order terms
  for (i in seq_len(computed$p)) {
    if (i == computed$target.idx) {
      next
    }
    pair <- sort(c(i, computed$target.idx))
    s <- paste0(s, "\n")
    s <- paste0(s, "Coefficient of \"(X", computed$all.variable[pair[1]], ",X",
                computed$all.variable[pair[2]], ")\":\n")
    temp.l <- computed$lambda.coef[[paste(pair, collapse=".")]]
    if (is.null(temp.l)) {
      s <- paste0(s, "NA\n")
    } else {
      s <- paste0(s, array.string(round(temp.l, 4)))
    }
  }
  # Mu's
  s <- paste0(s, "\n")
  s <- paste0(s, "Mu's for Target Variable X", computed$target.variable, ":\n")
  s <- paste0(s, paste(rep("-", 27 + nchar(computed$target.variable)), collapse=""), "\n")
  s <- paste0(s, "Mu(Intercept) = ", round(computed$mu[[computed$target.idx]], 4), "\n")
  variable.value <- list()
  for (i in seq_len(computed$p)) {
    if (i == computed$target.idx) {
      next
    }
    s <- paste0(s, "\n")
    if (length(computed$mu[[i]]) == 1 && is.na(computed$mu[[i]])) {
      s <- paste0(s, "Mu(X", computed$target.variable, ",X",
                  computed$all.variable[i], ") is NA\n")
    } else {
      variable.value[[i]] <- sort(unique(computed$data[,i]))
      for (v in seq_along(variable.value[[i]])) {
        s <- paste0(s, "Mu(X", computed$target.variable, ",X",
                    computed$all.variable[i], "=", variable.value[[i]][v], ") = ",
                    round(computed$mu[[i]][v], 4), "\n")
      }
    }
  }
  # Propensity
  
  # Variable value
  
  
  num.propensity <- prod(sapply(computed$mu, length))
  variable.value[[computed$target.idx]] <- NULL
  if (computed$p == 2) {
    explanatory.value <- paste(variable.value[[1]])
  } else {
    explanatory.value <- c(outer.multi(variable.value, paste))
  }
  # Propensity value
  explanatory.mu <- list()
  for (i in seq_len(computed$p)) {
    if (i == computed$target.idx) {
      next
    }
    explanatory.mu <- c(explanatory.mu, list(computed$mu[[i]]))
  }
  odd.array <- exp(computed$mu[[computed$target.idx]] +
                     outer.multi(explanatory.mu, "+"))
  propensity.value <- c(odd.array / (odd.array + 1))
  # Construct the table
  propensity.table <- matrix("", 1 + num.propensity, 1 + computed$p)
  propensity.table[1,1] <- "Case"
  propensity.table[1,2:computed$p] <-
    paste0("X",computed$all.variable[-computed$target.idx])
  propensity.table[1,computed$p+1] <- "Propensity"
  for (i in seq_along(propensity.value)) {
    propensity.table[i+1,1] <- paste0(i, ":")
    propensity.table[i+1,2:computed$p] <- strsplit(explanatory.value[[i]], " ")[[1]]
    propensity.table[i+1,computed$p+1] <- round(propensity.value[i], 4)
  }
  if (num.propensity > 1) {
    min.i <- which.min(propensity.value) + 1
    propensity.table[min.i,computed$p+1] <-
      paste0(propensity.table[min.i,computed$p+1], "  <- Minimum")
    max.i <- which.max(propensity.value) + 1
    propensity.table[max.i,computed$p+1] <-
      paste0(propensity.table[max.i,computed$p+1], "  <- Maximum")
  }
  
  s <- paste0(s, "\n")
  s <- paste0(s, "Propensity Values\n")
  s <- paste0(s, "-----------------\n")
  
  if (num.propensity > 500) {
    s <- paste0(s, "There are ", num.propensity, " propensity values.  ","\n",
                "only highest 100 and lowest 100 are displayed")
    top.100.indexes <- order(as.numeric(propensity.table[2:nrow(propensity.table),computed$p+1]), decreasing = TRUE)[1:100]
    bottom.100.indexes <- order(as.numeric(propensity.table[2:nrow(propensity.table),computed$p+1]), decreasing = FALSE)[1:100]
    top.100.indexes<- c(max.i-1, top.100.indexes)
    bottom.100.indexes<- c(min.i-1, bottom.100.indexes)
    
    top.table <- rbind(propensity.table[1,],propensity.table[top.100.indexes+1,])
    bottom.table <- rbind(propensity.table[1,],propensity.table[bottom.100.indexes+1,])
    
    
    s <- paste0(s, "\n")
    s <- paste0(s, "Highest 100 values\n")
    s <- paste0(s, "-----------------\n")
    
    s <- paste0(s, matrix.string(top.table))
    
    s <- paste0(s, "\n")
    s <- paste0(s, "Lowest 100 values\n")
    s <- paste0(s, "-----------------\n")
    
    s <- paste0(s, matrix.string(bottom.table))
    
  } else {
    s <- paste0(s, matrix.string(propensity.table))
  }
  
  
  return(s)
}

n.dimensional.twoway.computation <- function(data, predictor.variable,
                                             target.variable) {
  predictor.variable <- sort(predictor.variable)
  all.variable <- sort(c(predictor.variable, target.variable))
  target.idx <- which(all.variable == target.variable)
  data <- data[,all.variable]
  
  p <- ncol(data)
  contingency.array <- table(as.data.frame(data))
  dimnames(contingency.array) <- NULL
  covariate.to.fit <- list()
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (i < j) {
        covariate.to.fit <- c(covariate.to.fit, list(c(i, j)))
      }
    }
  }

  model <- loglin(contingency.array, covariate.to.fit, fit = TRUE, param = TRUE,
                  print = FALSE)
  l.sq.stat <- model$lrt
  d.o.f <- model$df
  p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE)
  lambda.coef <- model$param
  lambda.coef$'(Intercept)' <- lambda.coef$'(Intercept)' -
    log(sum(contingency.array))
  
  # Compute the mu's
  mu <- list()
  for (i in seq_len(p)) {
    if (i == target.idx) {
      mu[[i]] <- -diff(lambda.coef[[as.character(target.idx)]])
    } else {
      pair <- sort(c(i, target.idx))
      lambda.coef.i <- lambda.coef[[paste(pair, collapse = ".")]]
      if (is.null(lambda.coef.i)) {
        mu[[i]] <- NA
        next
      }
      if (i < target.idx) {
        mu[[i]] <- lambda.coef.i[,1] - lambda.coef.i[,2]
      } else {
        mu[[i]] <- lambda.coef.i[1,] - lambda.coef.i[2,]
      }
    }
  }
  return(list(data = data,
              all.variable = all.variable,
              predictor.variable = predictor.variable,
              target.variable = target.variable,
              target.idx = target.idx,
              p = p,
              l.sq.stat = l.sq.stat,
              d.o.f = d.o.f,
              p.value = p.value,
              lambda.coef = lambda.coef,
              mu = mu))
}

n.dimensional.complexity.computation <- function(data, predictor.variable,
                                                 target.variable) {
  
  predictor.variable <- sort(predictor.variable)
  all.variable <- sort(c(predictor.variable, target.variable))
  target.idx <- which(all.variable == target.variable)
  data <- data[,all.variable]
  
  p <- ncol(data)
  contingency.array <- table(as.data.frame(data))
  dimnames(contingency.array) <- NULL
  covariate.to.fit <- list()
  combintaions = combn(seq(p), p-1)
  for (i in seq_len(ncol(combintaions))) {
    covariate.to.fit[[i]] <- combintaions[,i]
  }
  model <- loglin(contingency.array, covariate.to.fit, fit = TRUE, param = TRUE,
                  print = FALSE)
  l.sq.stat <- model$lrt
  d.o.f <- model$df
  p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE)
  lambda.coef <- model$param
  lambda.coef$'(Intercept)' <- lambda.coef$'(Intercept)' -
    log(sum(contingency.array))
  
  # Compute the mu's
  mu <- list()
  for (i in seq_len(p)) {
    if (i == target.idx) {
      mu[[i]] <- -diff(lambda.coef[[as.character(target.idx)]])
    } else {
      pair <- sort(c(i, target.idx))
      lambda.coef.i <- lambda.coef[[paste(pair, collapse = ".")]]
      if (is.null(lambda.coef.i)) {
        mu[[i]] <- NA
        next
      }
      if (i < target.idx) {
        mu[[i]] <- lambda.coef.i[,1] - lambda.coef.i[,2]
      } else {
        mu[[i]] <- lambda.coef.i[1,] - lambda.coef.i[2,]
      }
    }
  }
  
  return(list(data = data,
              all.variable = all.variable,
              predictor.variable = predictor.variable,
              target.variable = target.variable,
              target.idx = target.idx,
              p = p,
              l.sq.stat = l.sq.stat,
              d.o.f = d.o.f,
              p.value = p.value,
              lambda.coef = lambda.coef,
              mu = mu))
}


n.dimensional.select.model.computation <- function(data, predictor.variable, target.variable, which.x, p.value.threshold=0.05) {
  predictor.variable <- sort(predictor.variable)
  all.variable <- c(predictor.variable, target.variable)
  target.idx <- which(all.variable == target.variable)
  data <- data[,all.variable]
  
  p <- ncol(data)
  contingency.array <- table(as.data.frame(data))
  dimnames(contingency.array) <- NULL
  model <- loglin(contingency.array, margin = which.x, fit = TRUE, param = TRUE, print = FALSE)
  l.sq.stat <- model$lrt
  d.o.f <- model$df
  p.value <- pchisq(l.sq.stat, df = d.o.f, lower.tail = FALSE)
  model.is.fit <- p.value > p.value.threshold 
  lambda.coef <- model$param
  lambda.coef$'(Intercept)' <- lambda.coef$'(Intercept)' -
    log(sum(contingency.array))
  
  lambda.coef.names <- names(lambda.coef)
  
  
  # Compute the mu's
  
  mu <- list()
  
  for (i in lambda.coef.names) {
    if (i == target.idx) {
      mu[[i]] <- -diff(lambda.coef[[as.character(target.idx)]])
    } else if(stringr::str_detect(string = i, pattern = "\\.")){
      if(any(unlist(regmatches(i, gregexpr(pattern ="\\d+",  i))) == target.idx)){
        dim.target <- which(unlist(regmatches(i, gregexpr(pattern ="\\d+",  i))) == target.idx)
        mu[[i]] <- -Apply(data = lambda.coef[[i]],target = list(dim.target) ,fun =  diff)[[1]]
      }
    }
  }
  # browser()
  # compute the propensities

  explanatory.mu <- mu[names(mu)[names(mu) != as.character(target.idx)]]
  
  
  sequences <- list()
  for (i in 1:(p-1)){
    sequences[[i]] <- seq_len(length(unique(data[,i])))
  }
  cases = as.matrix(expand.grid(sequences))
  colnames(cases)=paste0("X",predictor.variable)
  
  # browser()
  sum.mu.array <- c()
  for(i in 1:nrow(cases)){
    case = drop(cases[i,])
    odd = mu[[as.character(target.idx)]]
    for (tmp.mu in names(explanatory.mu)){
      tmp.mu.as.vector <- as.numeric(stringr::str_extract_all(tmp.mu, "\\d+")[[1]])
      tmp.mu.as.vector <- tmp.mu.as.vector[1:length(tmp.mu.as.vector)-1]
      odd = odd + explanatory.mu[[tmp.mu]][matrix(case[tmp.mu.as.vector],nrow = 1)]
    }
    sum.mu.array <- cbind(sum.mu.array, odd)
  }
  odd.array <- c(exp(sum.mu.array))
  propensity.array <- (c(odd.array/(odd.array+1)))
  odd.array <- round(odd.array, 4)
  propensity.array <- round(propensity.array, 4)
  odd.array <- cbind(cases, odd.array)
  propensity.array <- cbind(cases, propensity.array)
  
  
  
  return(list(data = data,
              all.variable = all.variable,
              predictor.variable = predictor.variable,
              target.variable = target.variable,
              target.idx = target.idx,
              p = p,
              l.sq.stat = l.sq.stat,
              d.o.f = d.o.f,
              p.value = p.value,
              model.is.fit = model.is.fit,
              lambda.coef = lambda.coef,
              mu = mu,
              odd = odd.array,
              propensity = propensity.array))
}


coefficients_names <- function(data.col.names, coefficients){
  pairs <- which(stringr::str_detect(string = coefficients, pattern = "\\."))
  
  text <- paste(c(
    "Intercept",
    data.col.names[as.integer(coefficients[-c(1,pairs)])],
    unlist(lapply(X = regmatches(coefficients[pairs], gregexpr(pattern ="\\d+",  coefficients[pairs])), function(x){
      paste("(", paste0(data.col.names[as.integer(x)], collapse = ", "), ")", sep = "")
    }))), collapse = " ")
  
  return(text)
}

lose.of.information.test <- function(new_model, complexity_model){
  return(new_model$l.sq.stat - complexity_model$l.sq.stat < qchisq(0.95, df = new_model$d.o.f - complexity_model$d.o.f))
}

n.dimensional.target.variable <- function(data, predictor.variable,
                                          target.variable,client.constraints, level = 2) {
  all.variable <- c(predictor.variable, target.variable)
  target.idx <- which(all.variable == target.variable)
  original.data <- data
  client.constraints = unlist(client.constraints)
  sorted.by.predictivens = threedimensional.sort.explanatory.variables(original.data,target.variable,predictor.variable)
  # top.predictors = sorted.by.predictivens[c(1,2)]
  if (length(client.constraints) > 0){
  model.predictor.variables = client.constraints
  }
  else{
    model.predictor.variables = sorted.by.predictivens[c(1,2)]
  }
  remainig.variables = setdiff(sorted.by.predictivens, model.predictor.variables)
  
  model1 = n.dimensional.twoway.computation(data = original.data, predictor.variable = model.predictor.variables, target.variable = target.variable)
  p0 = model1$p.value
  

  if (length(remainig.variables)>0){
    for (i in 1:length(remainig.variables)){
      temp.model.predictor.variables = c(model.predictor.variables, remainig.variables[i])
      model = n.dimensional.twoway.computation(data = original.data,predictor.variable = temp.model.predictor.variables, target.variable = target.variable )
      if (model$p.value > 0.05 & model$p.value <= p0){
        model1 = model
        model.predictor.variables = temp.model.predictor.variables
        p0 = model1$p.value
      } 
      else{
        break
      }
    }
      
  }
  
  p = length(model.predictor.variables)+1
  # browser()
  model_list = list()
  levels = c()
  for (k in (p-1):2){
    covariates.to.fit = list()
    combinations = combn(seq(p),k)
    for (i in seq_len(ncol(combinations))){
      covariates.to.fit[[i]] <- combinations[,i]
    }
    # browser()
    model <- n.dimensional.select.model.computation(data = original.data, predictor.variable = model.predictor.variables, target.variable = target.variable, which.x = covariates.to.fit)
    if (model$p.value > 0.05 & all(!is.nan(model$propensity))){
      model_list <- append(model_list, list(model))
      levels = c(levels, k)
    }
  }
  indep.covariates.to.fit = list()
  for (i in 1:(p-1)){
    indep.covariates.to.fit[[i]] <- c(i,p)
  }
  
  
  indep_model <- n.dimensional.select.model.computation(data = original.data, predictor.variable = model.predictor.variables, target.variable = target.variable, which.x = indep.covariates.to.fit)
  
  if (indep_model$p.value > 0.05){
  model_list <- append(model_list, list(indep_model))
  }
  data <- data[,model1$all.variable]
  coef.text = list()
  if (length(model_list) >= 1){
  for (i in 1:length(model_list)){
    coef.text <- append(coef.text, list(coefficients_names(data.col.names = colnames(data), names(model_list[[i]][["lambda.coef"]]))))
  }
    return(computed = list(original.data = original.data,model = model_list,coef.text = coef.text,sorted.by.predictivens = sorted.by.predictivens, levels = levels))
    
  }
  else if (length(model_list) == 0){
    return(NULL)
  }
}

n.dimensional.target.variable.report <- function(computed){

  s <- ""
  s <- paste0(s, "n-Dimensional - Model Selection Result\n")
  s <- paste0(s, "==========================\n")
  if (is.null(computed)){
    s <- paste0(s, "No model fits the data under these constraints")
    return(s)
  }
  s <- paste0(s, "Explanatory variables sorted by predictiveness: ")
  for (i in 1:length(computed$sorted.by.predictivens)){
    s <- paste0(s, as.character(computed$sorted.by.predictivens[i]))
    s <- paste0(s, " ")
  }
  omitted.variables = setdiff(computed$sorted.by.predictivens,computed$model[[1]]$predictor.variable )
  s <- paste0(s, "\n")  
  if (length(omitted.variables)==0){
    s <- paste0(s, "No variables omitted")
  }
  else{
  s <- paste0(s,"Omitted variables: ")
  for (i in 1:length(omitted.variables)){
    s<-paste0(s," ", omitted.variables[i], " ")}
  }
  s<-paste0(s,"\n")
  s <- paste0(s, "Models that fit the data by level of complexity:\n")
  s <- paste0(s, "-----------------------\n")
  dof.space <- 6
  l.sq.space <- 10
  indent <- space(4)
  tiers <- c(3, 2, 2, 2, 1, 1, 1, 0)
  models <- list()
  n <- length(computed$coef.text)
  print_model <- function(s, i){
    propensity <- n.dimensional.propensity.computation(computed$model[[i]])
    max.propensity <- propensity[which.max(propensity[,ncol(propensity)]),]
    min.propensity <- propensity[which.min(propensity[,ncol(propensity)]),]
    
    s <- paste0(s, "Lambda coefficients: ", computed$coef.text[[i]], "\n")
    s <- paste0(s, indent, "D.O.F. = ", paste0(computed$model[[i]]$d.o.f,
                                               space(max(0, dof.space - nchar(computed$model[[i]]$d.o.f)))),
                "L-Squared = ", paste0(round(computed$model[[i]]$l.sq.stat, 2),
                                       space(max(0, l.sq.space - nchar(round(computed$model[[i]]$l.sq.stat, 2))))),
                "p-value = ", signif(computed$model[[i]]$p.value, 4), "\n")
    
    
    if (computed$model[[i]]$p.value > 0.05) {
      s <- paste0(s, indent, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, indent, "Model does not fit the observed data.\n")
      
    }
    s<-paste0(s, "\n")
    s<-paste0(s, "Profile of higest propensity:")

    for (j in 1:(ncol(propensity)-1)){
      s<-paste0(s, "(", colnames(propensity)[j],"=",max.propensity[j],") ")
    }
    s <- paste0(s, "Propensity: ", max.propensity[length(max.propensity)])
    s<-paste0(s, "\n")
    s<-paste0(s, "Profile of lowest propensity:")
    for (j in 1:(ncol(propensity)-1)){
      s<-paste0(s, "(", colnames(propensity)[j],"=",min.propensity[j],") ")
    }
    s <- paste0(s, "Propensity: ", min.propensity[length(min.propensity)])
    
  }
  for (i in 1:length(computed$levels)){
    k = computed$levels[i]
    s <- paste0(s, "(", i, ") " ,k,"-way interaction model:\n")
    s <- print_model(s,i)
    s <- paste0(s, "\n \n \n ")
  }


  if (length(computed$model) > length(computed$levels)){
  s <- paste0(s,"(",length(computed$levels)+1, ") No interaction model:\n")
  s <- print_model(s, length(computed$levels)+1)
  s<- paste0(s, "\n \n \n ")
  }

  for (i in length(computed$model):1){
    l.sq.diff = computed$model[[i]]$l.sq.stat - computed$model[[1]]$l.sq.stat 
    d.o.f.diff = computed$model[[i]]$d.o.f-computed$model[[1]]$d.o.f
    if (l.sq.diff <= qchisq(0.95,d.o.f.diff)){
      s <- paste0(s,"L squared difference between model (",i,") and model (1) is ", round(l.sq.diff,2)," with ", round(d.o.f.diff,2)," degrees of freedom \n")
      s <- paste0(s, "Model (",i,") is the the most parsimonious model that does not entail a loss of information \n")
    break
      }
    
    else{
      s <- paste0(s,"L squared difference between model (",i,") and model (1) is ", round(l.sq.diff,2)," with ", round(d.o.f.diff,2)," degrees of freedom \n")
      s <- paste0(s, "Model (",i,") entails a loss of information \n")
    }
  }
      

  # if(n == 1){
  #   s <- paste0(s, "\nNo model of a lesser complexity fits the data.")
  # } else{
  #   for (i in seq(n,2)) {
  #     s <- paste0(s, "\n")
  #     if(i==2){
  #       s <- paste0(s, "Most parsimony model: \n")
  #     } else if (i==n){
  #       s <- paste0(s, "\nModel of higher complexity in descending order: \n")
  #     }
  #     s <- paste0(s, "Model level: ", i-1, "\n")
  #     s <- print_model(s, i)
  #   }
  # }
  # 
  
  return(s)
}

coefficients_names <- function(data.col.names, coefficients){
  pairs <- which(stringr::str_detect(string = coefficients, pattern = "\\."))
  
  text <- paste(c(
    "Intercept",
    data.col.names[as.integer(coefficients[-c(1,pairs)])],
    unlist(lapply(X = regmatches(coefficients[pairs], gregexpr(pattern ="\\d+",  coefficients[pairs])), function(x){
      paste("(", paste0(data.col.names[as.integer(x)], collapse = ", "), ")", sep = "")
    }))), collapse = " ")
  
  return(text)
}

var_names <- function(computed, coefficient){
  data.col.names <- paste("X", computed$all.variable, sep = "")
  if(coefficient == "(Intercept)"){
    return("Intercept")
  } else if(stringr::str_detect(string = coefficient, pattern = "\\.")){
    unlist(lapply(X = regmatches(coefficient, gregexpr(pattern ="\\d+",  coefficient)), function(x){
      paste("(", paste0(data.col.names[as.integer(x)], collapse = ", "), ")", sep = "")
    }))
  } else {
    data.col.names[as.integer(coefficient)]
  }
}

n.dimensional.propensity.computation <- function(model){
 
  
  predictor.variable = model$predictor.variable
  p <- ncol(model$data)
  mu <- list()
  
  for (i in  names(model$lambda.coef)) {
    if (i == model$target.idx) {
      mu[[i]] <- -diff(model$lambda.coef[[as.character(model$target.idx)]])
    } else if(stringr::str_detect(string = i, pattern = "\\.")){
      if(any(unlist(regmatches(i, gregexpr(pattern ="\\d+",  i))) == model$target.idx)){
        dim.target <- which(unlist(regmatches(i, gregexpr(pattern ="\\d+",  i))) == model$target.idx)
        mu[[i]] <- -Apply(data = model$lambda.coef[[i]],target = list(dim.target) ,fun =  diff)[[1]]
      }
    }
  }
  
  # compute the propensities
  explanatory.mu <- mu[names(mu)[names(mu) != as.character(model$target.idx)]]
  
  
  sequences <- list()
  for (i in 1:(p-1)){
    sequences[[i]] <- seq_len(length(unique(model$data[,i])))
  }
  cases = as.matrix(expand.grid(sequences))
  colnames(cases)=paste0("X",predictor.variable)
  
  
  sum.mu.array <- c()
  for(i in 1:nrow(cases)){
    case = drop(cases[i,])
    odd = mu[[as.character(model$target.idx)]]
    for (tmp.mu in names(explanatory.mu)){
      tmp.mu.as.vector <- as.numeric(stringr::str_extract_all(tmp.mu, "\\d+")[[1]])
      tmp.mu.as.vector <- tmp.mu.as.vector[1:length(tmp.mu.as.vector)-1]
      odd = odd + explanatory.mu[[tmp.mu]][matrix(case[tmp.mu.as.vector],nrow = 1)]
    }
    sum.mu.array <- cbind(sum.mu.array, odd)
  }
  odd.array <- c(exp(sum.mu.array))
  propensity.array <- (c(odd.array/(odd.array+1)))
  odd.array <- round(odd.array, 4)
  propensity.array <- round(propensity.array, 4)
  odd.array <- cbind(cases, odd.array)
  propensity.array <- cbind(cases, propensity.array)
  return(propensity.array)
}

n.dimensional.model.value.computation <- function(data,
                                                  propensity.matrix, threshold, tp.reward, tn.reward,
                                                  fp.reward, fn.reward) {
  # browser()
  p <- ncol(data)
  obs.one <- data[data[,p]==1,1:(p-1)]
  obs.two<- data[data[,p]==2,1:(p-1)]
  predict.is.one <- propensity.matrix[propensity.matrix[,p] >= threshold,1:(p-1)]
  if(sum(propensity.matrix[,p]>=threshold)<=1){
    num.tp = 0 
    num.fp = 0
    num.fn = nrow(obs.one)
    num.tn = nrow(obs.two)
  }
  
  else{
  
  obs.one.pasted.rows <- apply(obs.one, 1, paste, collapse = ", ")
  obs.two.pasted.rows <- apply(obs.two, 1, paste, collapse = ", ")
  predict.is.one.pasted.rows <- apply(predict.is.one, 1, paste, collapse = ", ")
  
  
  num.tp <- nrow(obs.one[obs.one.pasted.rows %in% intersect(obs.one.pasted.rows,predict.is.one.pasted.rows), , drop=FALSE])
  num.fp <- nrow(obs.two[obs.two.pasted.rows %in% intersect(obs.two.pasted.rows, predict.is.one.pasted.rows),, drop=FALSE])
  num.fn <- nrow(obs.one[!(obs.one.pasted.rows %in% intersect(obs.one.pasted.rows,predict.is.one.pasted.rows)),, drop = FALSE])
  num.tn <- nrow(obs.two[!(obs.two.pasted.rows %in% intersect(obs.two.pasted.rows, predict.is.one.pasted.rows)),, drop=FALSE])
  }
  sensitivity <- num.tp / (num.tp + num.fn)
  specificity <- num.tn / (num.fp + num.tn)
  model.value <- tp.reward * num.tp + tn.reward * num.tn +
    fp.reward * num.fp + fn.reward * num.fn
  return(list(num.tp = num.tp,
              num.fp = num.fp,
              num.fn = num.fn,
              num.tn = num.tn,
              sensitivity = sensitivity,
              specificity = specificity,
              model.value = model.value))
}

n.dimensional.model.select.report <- function(computed) {

  
  
  predictor.variable = computed$predictor.variable
  s <- ""
  s <- paste0(s, "N-Dimensional Model Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")
  predictor.list <- group.to.list(predictor.variable)
  predictor.text <- ""
  for (i in seq_along(predictor.list)) {
    if (i != 1) {
      predictor.text <- paste0(predictor.text, ", ")
    }
    if (length(predictor.list[[i]]) == 1) {
      predictor.text <- paste0(predictor.text, "X", predictor.list[[i]])
    } else {
      predictor.text <- paste0(predictor.text, "X", predictor.list[[i]][1],
                               " to X", predictor.list[[i]][2])
    }
  }
  s <- paste0(s, "Predictor variables: ", predictor.text, "\n")
  s <- paste0(s, "Target variable: X", computed$target.variable, "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "L-Square = ", round(computed$l.sq.stat, 2),
              "  D.O.F. = ", computed$d.o.f,
              "  p-value = ", signif(computed$p.value, 4), "\n")
  # s <- paste0(s, "\n")
  # s <- paste0(s, "Lambda Coefficients\n")
  # s <- paste0(s, "-------------------\n")

  # for (coefficient in names(computed$lambda.coef)) {
  #   computed_var <- var_names(computed = computed, coefficient = coefficient)
  #   
  #   s <- paste0(s, "\n")
  #   s <- paste0(s, "Coefficient of \"", computed_var ,"\":\n")
  #   temp.l <- computed$lambda.coef[[coefficient]]
  #   if (is.null(temp.l)) {
  #     s <- paste0(s, "NA\n")
  #   } else {
  #     s <- paste0(s, array.string(round(temp.l, 4)))
  #   }
  # }
  
  
  # Mu's
  s <- paste0(s, "\n")
  s <- paste0(s, "Mu's for Target Variable X", computed$target.variable, ":\n")
  s <- paste0(s, paste(rep("-", 27 + nchar(computed$target.variable)), collapse=""), "\n")
  s <- paste0(s, "Mu(Intercept) = ", round(computed$mu[[computed$target.idx]], 4), "\n")
  
  explanatory.mu <- computed$mu[names(computed$mu)[names(computed$mu) != computed$target.idx]]
  for (tmp.mu in names(explanatory.mu)){
    sequences <- list()
    tmp.mu.as.vector <- as.numeric(stringr::str_extract_all(tmp.mu, "\\d+")[[1]])
    tmp.mu.as.vector <- tmp.mu.as.vector[1:length(tmp.mu.as.vector)-1]
    for (i in 1:length(tmp.mu.as.vector)){
      sequences[[i]] <- seq_len(length(unique(computed$data[,tmp.mu.as.vector[i]])))
    }
    cases = as.matrix(expand.grid(sequences))
    for (i in 1:nrow(cases)){
      s1 = paste0("Mu (X",computed$target.variable)
      for (j in 1:ncol(cases)){
        s1 = paste0(s1,", X", predictor.variable[tmp.mu.as.vector[j]],"=", cases[i,j])
      }
      s1 = paste0(s1, ")= ", round(explanatory.mu[[tmp.mu]][matrix(cases[i,], nrow = 1)],4))
      
      s <- paste0(s, s1, "\n")
    }
    s <- paste0(s, "\n")
    
  }
  
  
  # Odds
  s <- paste0(s, "\n")
  s <- paste0(s, "Odds Values\n")
  s <- paste0(s, "-----------------\n")
  num.odds <- nrow(computed$odd)
  odd.table <- computed$odd
  if (num.odds > 500) {
    s <- paste0(s, "There are ", num.odds, " odds values.  ", "\n",
                "Only highest 100","\n" ,"and lowest 100 are displayed")
   
    
    
    
    top.100.indexes <- order(odd.table[,ncol(computed$odd)], decreasing = TRUE)[1:100]
    bottom.100.indexes <- order(odd.table[,ncol(computed$odd)], decreasing = FALSE)[1:100]
    
    top.table <- odd.table[top.100.indexes,]
    max.i <- which.max(top.table[,ncol(computed$odd)])
    top.table <- cbind("",top.table,"")
    first.row <- matrix(c("Case",paste0("X", computed$all.variable[-computed$target.idx]),"",""), nrow = 1)
    top.table <- rbind(first.row, top.table)
    top.table[1,computed$p+1] <- "Odds"
    top.table <- cbind(top.table, "")
    top.table[max.i+1,ncol(computed$odd)+2] <- "  <- Maximum"
    
    bottom.table <- odd.table[bottom.100.indexes,]
    min.i <- which.min(bottom.table[,ncol(computed$odd)])
    bottom.table <- cbind("",bottom.table,"")
    first.row <- matrix(c("Case",paste0("X", computed$all.variable[-computed$target.idx]),"",""), nrow = 1)
    bottom.table <- rbind(first.row, bottom.table)
    bottom.table[1,computed$p+1] <- "Odds"
    bottom.table <- cbind(bottom.table, "")
    bottom.table[min.i+1,ncol(computed$odd)+2] <- "  <- Minimum"
    
    
    
    s <- paste0(s, "\n")
    s <- paste0(s,"Highest 100 odds")
    s <- paste0(s, "\n")
    
    s <- paste0(s, matrix.string(top.table))
    s <- paste0(s, "\n")
    
    s <- paste0(s, "\n")
    s <- paste0(s,"Lowest 100 odds")
    s <- paste0(s, "\n")
    
    s <- paste0(s, matrix.string(bottom.table))
    s <- paste0(s, "\n")
    
  } else {
    if (num.odds > 1) {
      max.i <- which.max(odd.table[,ncol(computed$odd)])
      min.i <- which.min(odd.table[,ncol(computed$odd)])
      odd.table <- cbind("",odd.table,"")
      first.row <- matrix(c("Case",paste0("X", computed$all.variable[-computed$target.idx]),"",""), nrow = 1)
      odd.table <- rbind(first.row, odd.table)
      odd.table[1,computed$p+1] <- "Odds"
      odd.table <- cbind(odd.table, "")
      odd.table[max.i+1,ncol(computed$odd)+2] <- "  <- Maximum"
      odd.table[min.i+1,ncol(computed$odd)+2] <- "  <- Minimum"
      s <- paste0(s, matrix.string(odd.table))
      s <- paste0(s, "\n")
      
    }
    
  }
  
  # Propensity
  
  s <- paste0(s, "\n")
  s <- paste0(s, "Propensity Values\n")
  s <- paste0(s, "-----------------\n")
  num.propensity <- nrow(computed$propensity)
  propensity.table <- cbind("",computed$propensity,"")
  first_row = matrix(c("Case",paste0("X", computed$all.variable[-computed$target.idx]),"",""), nrow = 1)
  propensity.table<- rbind(first_row, propensity.table)
  propensity.table[1,computed$p+1] <- "Propensity"
  propensity.table<- cbind(propensity.table,"")
  min.i <- which.min(computed$propensity[,ncol(computed$propensity)])
  propensity.table[min.i+1,ncol(computed$propensity)+2] <-"  <- Minimum"
  max.i <- which.max(computed$propensity[,ncol(computed$propensity)])
  propensity.table[max.i+1,ncol(computed$propensity)+2] <- "  <- Maximum"
  
  
  if (num.propensity > 500) {
    s <- paste0(s, "There are ", num.propensity, " propensity values.  ",
                "only highest 100 and lowest 100", "\n", "are displayed")
    
    top.100.indexes <- order(propensity.table[2:nrow(propensity.table),ncol(computed$odd)+1], decreasing = TRUE)[1:100]
    bottom.100.indexes <- order(propensity.table[2:nrow(propensity.table),ncol(computed$odd)+1], decreasing = FALSE)[1:100]
    
    top.table <- rbind(propensity.table[1,], propensity.table[top.100.indexes+1,])
    bottom.table <- rbind(propensity.table[1,], propensity.table[bottom.100.indexes+1,])
    
    
    s <- paste0(s, "\n")
    s <- paste0(s,"Highest 100 propensities")
    s <- paste0(s, "\n")
    
    s <- paste0(s, matrix.string(top.table))
    s <- paste0(s, "\n")
    
    s <- paste0(s, "\n")
    s <- paste0(s,"Lowest 100 propensties")
    s <- paste0(s, "\n")
    
    s <- paste0(s, matrix.string(bottom.table))
    s <- paste0(s, "\n")
  } else {
    if (num.propensity > 1) {
      
      
      s <- paste0(s, matrix.string(propensity.table))
    }
    
  }
  return(s[1])
}

n.dimensional.twoway.report <- function(computed) {
  s <- ""
  s <- paste0(s, "N-Dimensional Model Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")
  predictor.list <- group.to.list(computed$predictor.variable)
  predictor.text <- ""
  for (i in seq_along(predictor.list)) {
    if (i != 1) {
      predictor.text <- paste0(predictor.text, ", ")
    }
    if (length(predictor.list[[i]]) == 1) {
      predictor.text <- paste0(predictor.text, "X", predictor.list[[i]])
    } else {
      predictor.text <- paste0(predictor.text, "X", predictor.list[[i]][1],
                               " to X", predictor.list[[i]][2])
    }
  }
  s <- paste0(s, "Predictor variables: ", predictor.text, "\n")
  s <- paste0(s, "Target variable: X", computed$target.variable, "\n")
  s <- paste0(s, "Model assumption: Two-way interaction\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "L-Square = ", round(computed$l.sq.stat, 2),
              "  D.O.F. = ", computed$d.o.f,
              "  p-value = ", signif(computed$p.value, 4), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Lambda Coefficients\n")
  s <- paste0(s, "-------------------\n")
  s <- paste0(s, "Coefficient of \"Intercept\":\n")
  s <- paste0(s, array.string(round(computed$lambda.coef$'(Intercept)', 4)))
  # First-order terms
  for (i in seq_len(computed$p)) {
    s <- paste0(s, "\n")
    s <- paste0(s, "Coefficient of \"X", computed$all.variable[i], "\":\n")
    temp.l <- computed$lambda.coef[[as.character(i)]]
    if (is.null(temp.l)) {
      s <- paste0(s, "NA\n")
    } else {
      s <- paste0(s, array.string(round(temp.l, 4)))
    }
  }
  # Second-order terms
  for (i in seq_len(computed$p)) {
    for (j in seq_len(computed$p)) {
      if (i < j) {
        s <- paste0(s, "\n")
        s <- paste0(s, "Coefficient of \"(X", computed$all.variable[i], ",X",
                    computed$all.variable[j], ")\":\n")
        temp.l <- computed$lambda.coef[[paste(c(i, j), collapse=".")]]
        if (is.null(temp.l)) {
          s <- paste0(s, "NA\n")
        } else {
          s <- paste0(s, array.string(round(temp.l, 4)))
        }
      }
    }
  }
  # Mu's
  s <- paste0(s, "\n")
  s <- paste0(s, "Mu's for Target Variable X", computed$target.variable, ":\n")
  s <- paste0(s, paste(rep("-", 27 + nchar(computed$target.variable)), collapse=""), "\n")
  s <- paste0(s, "Mu(Intercept) = ", round(computed$mu[[computed$target.idx]], 4), "\n")
  variable.value <- list()
  for (i in seq_len(computed$p)) {
    if (i == computed$target.idx) {
      next
    }
    s <- paste0(s, "\n")
    if (length(computed$mu[[i]]) == 1 && is.na(computed$mu[[i]])) {
      s <- paste0(s, "Mu(X", computed$target.variable, ",X",
                  computed$all.variable[i], ") is NA\n")
    } else {
      variable.value[[i]] <- sort(unique(computed$data[,i]))
      for (v in seq_along(variable.value[[i]])) {
        s <- paste0(s, "Mu(X", computed$target.variable, ",X",
                    computed$all.variable[i], "=", variable.value[[i]][v], ") = ",
                    round(computed$mu[[i]][v], 4), "\n")
      }
    }
  }
  # Propensity
  s <- paste0(s, "\n")
  s <- paste0(s, "Propensity Values\n")
  s <- paste0(s, "-----------------\n")
  num.propensity <- prod(sapply(computed$mu, length))
  
  variable.value[[computed$target.idx]] <- NULL
  if (computed$p == 2) {
    explanatory.value <- paste(variable.value[[1]]) # In string
  } else {
    explanatory.value <- c(outer.multi(variable.value, paste)) # In string
  }
  # Propensity value
  
  
  explanatory.mu <- list()
  for (i in seq_len(computed$p)) {
    if (i == computed$target.idx) {
      next
    }
    explanatory.mu <- c(explanatory.mu, list(computed$mu[[i]]))
  }
  odd.array <- exp(computed$mu[[computed$target.idx]] +
                     outer.multi(explanatory.mu, "+"))
  propensity.value <- c(odd.array / (odd.array + 1))
  # Construct the table
  propensity.table <- matrix("", 1 + num.propensity, 1 + computed$p)
  propensity.table[1,1] <- "Case"
  propensity.table[1,2:computed$p] <-
    paste0("X", computed$all.variable[-computed$target.idx])
  propensity.table[1,computed$p+1] <- "Propensity"
  for (i in seq_along(propensity.value)) {
    propensity.table[i+1,1] <- paste0(i, ":")
    propensity.table[i+1,2:computed$p] <- strsplit(explanatory.value[[i]], " ")[[1]]
    propensity.table[i+1,computed$p+1] <- round(propensity.value[i], 4)
  }
  
  if (num.propensity > 1) {
    min.i <- which.min(propensity.value) + 1
    propensity.table[min.i,computed$p+1] <-
      paste0(propensity.table[min.i,computed$p+1], "  <- Minimum")
    max.i <- which.max(propensity.value) + 1
    propensity.table[max.i,computed$p+1] <-
      paste0(propensity.table[max.i,computed$p+1], "  <- Maximum")
  }
  
  if (num.propensity > 500) {
    s <- paste0(s, "There are ", num.propensity, " propensity values.  ","\n",
                "Only highest 100 and lowest 100 are displayed")
    
    top.100.indexes = order(propensity.value, decreasing = TRUE)[1:100]
    bottom.100.indexes = order(propensity.value, decreasing = FALSE)[1:100]
    top.table = rbind(propensity.table[1,], propensity.table[top.100.indexes+1,])
    bottom.table = rbind(propensity.table[1,], propensity.table[bottom.100.indexes+1,])
    
    s <- paste0(s, "\n")
    s <- paste0(s, "Highest propensities\n")
    s <- paste0(s, matrix.string(top.table))
    s <- paste0(s, "\n")
    s <- paste0(s, "\n")
    s <- paste0(s, "Lowest propensities\n")
    s <- paste0(s, matrix.string(bottom.table))
  } else {
    # Variable value
    
    s <- paste0(s, matrix.string(propensity.table))
  }
  return(s)
}

# Survival logic
survival.subject.to.data <- function(subject, num.stage = NULL,
                                     return.vector = FALSE) {
  data <- table(subject)
  stage <- as.numeric(names(data))
  # Number of stages
  if (is.null(num.stage)) {
    num.stage <- max(stage)
    if (num.stage == -1) {
      num.stage <- 0
    }
  }
  v <- numeric(num.stage + 1)
  if (stage[1] == -1) {
    v[num.stage + 1] <- data[1]
    if (length(stage) > 1) {
      v[stage[-1]] <- data[-1]
    }
  } else {
    v[stage] <- data
  }
  if (return.vector) {
    return(v)
  }
  if (num.stage > 0) {
    row.name <- c(paste("Stage", 1:num.stage), "Survive")
  } else {
    row.name <- "Survive"
  }
  return(matrix.data(matrix(v, length(v), 1), row.name = row.name,
                     col.name = "Count"))
}

survival.parse.splining.string <- function(s, num.stage) {
  group <- parse.group.string(s)
  if (length(group) == 1 && is.na(group)) {
    return(NA)
  }
  if (length(group) == 0) {
    return(list(c(1, num.stage)))
  }
  if(!all(is.consecutive.group(group))) {
    return(NA)
  }
  if (max(unlist(group)) > num.stage || sum(sapply(group, length)) < num.stage) {
    return(NA)
  }
  membership <- group.label(num.stage, group)
  num.group <- tail(membership, 1)
  splining.interval <- list()
  for (i in seq_len(num.group)) {
    group.idx <- which(membership == i)
    splining.interval <- c(splining.interval, list(c(group.idx[1],
                                                     tail(group.idx, 1))))
  }
  return(splining.interval)
}

survival.homogeneous.computation <- function(death.per.period, still.survive,
                                             envelope.factor = 1.96, p.value.threshold = 0.05) {
  total.num.people <- sum(death.per.period) + still.survive
  num.period <- length(death.per.period)
  num.survivor <- rev(cumsum(rev(c(death.per.period, still.survive))))
  print(num.survivor)
  print(paste0("death per period: ", death.per.period))
  cond.frac.of.death.per.period <- death.per.period /
    num.survivor[-(num.period+1)]
  
  # Compute phi.
  phi <- sum(death.per.period) / sum(num.survivor[-(num.period+1)])
  phi.easd <- sqrt(phi^2 * (1 - phi) / (1 - (1 - phi)^(num.period)) /
                     total.num.people)
  
  # Compute the expected numbers of deaths per period.
  cond.prob.to.die <- rep(phi, num.period)
  expected.death.per.period <- cumprod(c(1, 1 -
                                           cond.prob.to.die[-num.period])) * cond.prob.to.die * total.num.people
  expected.still.survive <- prod(1 - cond.prob.to.die) * total.num.people
  
  # Compute observed and expected (unconditional) survival probabilities.
  uncond.survival.freq <- num.survivor / total.num.people
  temp.p <- death.per.period / total.num.people
  uncond.survival.freq.envelope <- c(0, envelope.factor *
                                       sqrt(temp.p * (1 - temp.p) / num.survivor[-1]))
  
  expected.uncond.survival.prob <- cumprod(c(1, 1 - cond.prob.to.die))
  temp.n <- total.num.people - cumsum(c(0, expected.death.per.period))
  temp.p <- expected.death.per.period / temp.n[-(num.period+1)]
  expected.uncond.survival.prob.envelope <- c(0, envelope.factor *
                                                sqrt(temp.p * (1 - temp.p) / temp.n[-1]))
  
  observed <- c(death.per.period, still.survive)
  expected <- c(expected.death.per.period, expected.still.survive)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- num.period - 1
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  
  return(list(
    observed=observed,
    expected=expected,
    cond.frac.of.death.per.period=cond.frac.of.death.per.period,
    phi=phi,
    phi.easd=phi.easd,
    expected.death.per.period=expected.death.per.period,
    expected.still.survive=expected.still.survive,
    chi.sq.stat=chi.sq.stat,
    l.sq.stat=l.sq.stat,
    d.o.f=d.o.f,
    p.value=p.value,
    model.is.fit=model.is.fit,
    std.residuals=std.residuals,
    num.sig.residuals=num.sig.residuals,
    uncond.survival.freq=uncond.survival.freq,
    uncond.survival.freq.envelope=uncond.survival.freq.envelope,
    expected.uncond.survival.prob=expected.uncond.survival.prob,
    expected.uncond.survival.prob.envelope=expected.uncond.survival.prob.envelope))
}

survival.homogeneous.report <- function(result, first.stage.idx,
                                        test.portion=NULL) {
  num.stage <- length(result$observed)
  s <- ""
  s <- paste0(s, "Homogeneous Model Result\n")
  s <- paste0(s, "========================\n")
  s <- paste0(s, "\n")
  if (!is.null(test.portion)) {
    s <- paste0(s, paste0("Test portion= ", round(test.portion, 2), "%"))
    s <- paste0(s, "\n\n")
  }
  temp <- matrix("", 2 + num.stage + 1, 5)
  temp[1,1] <- "Stage"
  temp[2,1] <- "====="
  temp[1,2] <- "Observed"
  temp[2,2] <- "========"
  temp[1,3] <- "n(k)/N(k)"
  temp[2,3] <- "========="
  temp[1,4] <- "Expected"
  temp[2,4] <- "========"
  temp[1,5] <- "Residual"
  temp[2,5] <- "========"
  for (t in 1:num.stage) {
    if (t < num.stage) {
      temp[2+t,1] <- first.stage.idx - 1 + t
      temp[2+t,3] <- round(result$cond.frac.of.death.per.period[t], 4)
      temp[2+t,4] <- round(result$expected.death.per.period[t], 2)
    } else {
      temp[2+num.stage] <- "Survive"
      temp[2+num.stage,4] <- round(result$expected.still.survive, 2)
    }
    temp[2+t,2] <- result$observed[t]
    temp[2+t,5] <- round(result$std.residuals[t], 2)
  }
  temp[2+num.stage+1,1] <- "Total"
  temp[2+num.stage+1,2] <- sum(result$observed)
  temp[2+num.stage+1,4] <- round(sum(result$expected.death.per.period) +
                                   result$expected.still.survive, 2)
  s <- paste0(s, matrix.string(temp, spacing=3))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Parameter Estimate\n")
  s <- paste0(s, "------------------\n")
  s <- paste0(s, paste0("Phi= ", signif(result$phi, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Phi EASD= ", signif(result$phi.easd, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("X-Square= ", signif(result$chi.sq.stat, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("L-Square= ", signif(result$l.sq.stat, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("D.O.F.= ", signif(result$d.o.f, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("p-value= ", signif(result$p.value, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Model Diagnostics\n")
  s <- paste0(s, "-----------------\n")
  if (result$model.is.fit) {
    s <- paste0(s, "Model fits the observed data.\n")
  } else {
    s <- paste0(s, "Model does not fit the observed data.\n")
  }
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Number of significant residuals = ",
                        result$num.sig.residuals, "\n"))
  return(s)
}

survival.acc.dc.computation <- function(death.per.period,
                                        still.survive, envelope.factor=1.96, p.value.threshold=0.05) {
  
  total.num.people <- sum(death.per.period) + still.survive
  num.period <- length(death.per.period)
  num.survivor <- rev(cumsum(rev(c(death.per.period, still.survive))))
  cond.frac.of.death.per.period <- death.per.period / num.survivor[-(num.period+1)]
  
  neg.log.like <- function(x, death.per.period, num.survivor) {
    num.period <- length(death.per.period)
    delta <- x[1]
    phi <- x[2]
    probs <- delta * exp((1:num.period) * phi)
    temp.s <- num.survivor[2:(num.period+1)]
    neg.log.like.x <- -sum(temp.s * log(1 - probs)) -
      sum(death.per.period * log(probs))
    total.death <- num.survivor[1] - tail(num.survivor,1)
    g1 <- -sum(temp.s * exp((1:num.period) * phi) / (1 - probs)) +
      total.death / delta
    g2 <- sum((1:num.period) * (death.per.period -
                                  probs / (1 - probs) * temp.s))
    attr(neg.log.like.x, "gradient") <- -c(g1, g2)
    h11 <- -sum(temp.s * exp(2*phi*(1:num.period)) / (1 - probs)^2) -
      total.death / delta^2
    h12 <- -sum(temp.s * exp((1:num.period)*phi) * (1:num.period) /
                  (1 - probs)^2)
    h22 <- -sum((1:num.period)^2 * temp.s * probs / (1 - probs)^2)
    attr(neg.log.like.x, "hessian") <- -matrix(c(h11, h12, h12, h22), 2, 2)
    return(neg.log.like.x)
  }
  # Initialize phi and delta.
  phi <- sum(log(death.per.period[2:num.period] / death.per.period[1:(num.period-1)]
                 * num.survivor[1:(num.period-1)] / num.survivor[2:num.period])) / (num.period - 1)
  delta <- prod(death.per.period / num.survivor[1:num.period])^(1/num.period) *
    exp(-phi * (1 + num.period) / 2)
  nlm.result <- nlm(f=neg.log.like, p=c(delta,phi), death.per.period,
                    num.survivor, hessian=TRUE, check.analyticals=FALSE)
  delta <- nlm.result$estimate[1]
  phi <- nlm.result$estimate[2]
  
  # Compute the EASD.
  hessian <- -nlm.result$hessian
  det.hessian <- det(hessian) 
  if (det.hessian == 0) {
    return(list(computation.has.singularity=TRUE))
  }
  delta.easd <- sqrt(abs(hessian[2,2] / det.hessian) / total.num.people)
  phi.easd <- sqrt(abs(hessian[1,1] / det.hessian) / total.num.people)
  
  # Compute the expected numbers of deaths per period.
  cond.prob.to.die <- delta * exp((1:num.period) * phi)
  if (any(cond.prob.to.die <= 0) || any(cond.prob.to.die >= 1)) {
    return(list(computation.has.singularity=TRUE))
  }
  
  expected.death.per.period <- cumprod(c(1, 1 - cond.prob.to.die[-num.period])) *
    cond.prob.to.die * total.num.people
  expected.still.survive <- prod(1 - cond.prob.to.die) * total.num.people
  
  # Compute observed and expected (unconditional) survival probabilities.
  uncond.survival.freq <- num.survivor / total.num.people
  temp.p <- death.per.period / total.num.people
  uncond.survival.freq.envelope <- c(0, envelope.factor *
                                       sqrt(temp.p * (1 - temp.p) / num.survivor[-1]))
  
  expected.uncond.survival.prob <- cumprod(c(1, 1 - cond.prob.to.die))
  temp.n <- total.num.people - cumsum(c(0, expected.death.per.period))
  temp.p <- expected.death.per.period / temp.n[-(num.period+1)]
  expected.uncond.survival.prob.envelope <- c(0, envelope.factor *
                                                sqrt(temp.p * (1 - temp.p) / temp.n[-1]))
  
  observed <- c(death.per.period, still.survive)
  expected <- c(expected.death.per.period, expected.still.survive)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- num.period - 2
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  
  return(list(
    computation.has.singularity=FALSE,
    observed=observed,
    expected=expected,
    cond.frac.of.death.per.period=cond.frac.of.death.per.period,
    delta=delta,
    phi=phi,
    delta.easd=delta.easd,
    phi.easd=phi.easd,
    expected.death.per.period=expected.death.per.period,
    expected.still.survive=expected.still.survive,
    chi.sq.stat=chi.sq.stat,
    l.sq.stat=l.sq.stat,
    d.o.f=d.o.f,
    p.value=p.value,
    model.is.fit=model.is.fit,
    std.residuals=std.residuals,
    num.sig.residuals=num.sig.residuals,
    uncond.survival.freq=uncond.survival.freq,
    uncond.survival.freq.envelope=uncond.survival.freq.envelope,
    expected.uncond.survival.prob=expected.uncond.survival.prob,
    expected.uncond.survival.prob.envelope=expected.uncond.survival.prob.envelope))
}

survival.acc.dc.report <- function(result, first.stage.idx) {
  s <- ""
  s <- paste0(s, "ACC/DC Model Result\n")
  s <- paste0(s, "===================\n")
  s <- paste0(s, "\n")
  if (result$computation.has.singularity) {
    s <- paste0(s, "The ACC/DC computation reached a singularity and stopped.\n")
    return(s)
  }
  num.stage <- length(result$observed)
  temp <- matrix("", 2 + num.stage + 1, 5)
  temp[1,1] <- "Stage"
  temp[2,1] <- "====="
  temp[1,2] <- "Observed"
  temp[2,2] <- "========"
  temp[1,3] <- "n(k)/N(k)"
  temp[2,3] <- "========="
  temp[1,4] <- "Expected"
  temp[2,4] <- "========"
  temp[1,5] <- "Residual"
  temp[2,5] <- "========"
  for (t in 1:num.stage) {
    if (t < num.stage) {
      temp[2+t,1] <- first.stage.idx - 1 + t
      temp[2+t,3] <- round(result$cond.frac.of.death.per.period[t], 4)
      temp[2+t,4] <- round(result$expected.death.per.period[t], 2)
    } else {
      temp[2+num.stage] <- "Survive"
      temp[2+num.stage,4] <- round(result$expected.still.survive, 2)
    }
    temp[2+t,2] <- result$observed[t]
    temp[2+t,5] <- round(result$std.residuals[t], 2)
  }
  temp[2+num.stage+1,1] <- "Total"
  temp[2+num.stage+1,2] <- sum(result$observed)
  temp[2+num.stage+1,4] <- round(sum(result$expected.death.per.period) +
                                   result$expected.still.survive, 4)
  s <- paste0(s, matrix.string(temp, spacing=3))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Parameter Estimate\n")
  s <- paste0(s, "------------------\n")
  s <- paste0(s, paste0("Delta= ", signif(result$delta, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Delta EASD= ", signif(result$delta.easd, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Phi= ", signif(result$phi, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Phi EASD= ", signif(result$phi.easd, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, paste0("X-Square= ", signif(result$chi.sq.stat, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("L-Square= ", signif(result$l.sq.stat, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("D.O.F.= ", signif(result$d.o.f, 4)))
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("p-value= ", signif(result$p.value, 4)))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Model Diagnostics\n")
  s <- paste0(s, "-----------------\n")
  if (result$model.is.fit) {
    s <- paste0(s, "Model fits the observed data.\n")
  } else {
    s <- paste0(s, "Model does not fit the observed data.\n")
  }
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Number of significant residuals = ",
                        result$num.sig.residuals, "\n"))
  return(s)
}

survival.margin.from.prob <- function(x, fx, upper.envelope, y) {
  x.new <- approx(fx, x, y)$y
  upper.envelope.at.x.new <- approx(x, upper.envelope, x.new)$y
  return(upper.envelope.at.x.new - y)
}

survival.oos.sampling <- function(death.per.period, still.survive,
                                  training.fraction, reproducible = TRUE) {
  max.stage <- length(death.per.period)
  x <- c(rep(1:max.stage, death.per.period), rep(-1, still.survive))
  n <- sum(death.per.period) + still.survive
  num.training <- max(round(n * training.fraction), 1)
  if (reproducible) {
    set.seed(1)
  }
  # Do sampling
  training.data <- sample(x, num.training)
  training.vector <- survival.subject.to.data(training.data,
                                              num.stage = max.stage, return.vector=TRUE)
  test.vector <- c(death.per.period, still.survive) - training.vector
  return(list(training.vector = training.vector,
              test.vector = test.vector))
}

survival.oos.training.computation <- function(death.per.period, still.survive) {
  total.num.people <- sum(death.per.period) + still.survive
  num.period <- length(death.per.period)
  num.survivor <- rev(cumsum(rev(c(death.per.period, still.survive))))
  cond.frac.of.death.per.period <- death.per.period /
    num.survivor[-(num.period+1)]
  cond.frac.of.death.per.period[is.nan(cond.frac.of.death.per.period)] <- NA
  observed <- c(death.per.period, still.survive)
  
  return(list(observed = observed,
              cond.frac.of.death.per.period = cond.frac.of.death.per.period))
}

survival.oos.training.report <- function(result, training.portion) {
  num.stage <- length(result$observed)
  s <- ""
  s <- paste0(s, "Out-of-Sample Training Result\n")
  s <- paste0(s, "=============================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, paste0("Training portion= ", round(training.portion, 2), "%"))
  s <- paste0(s, "\n\n")
  temp <- matrix("", 2 + num.stage + 1, 3)
  temp[1,1] <- "Stage"
  temp[2,1] <- "====="
  temp[1,2] <- "Observed"
  temp[2,2] <- "========"
  temp[1,3] <- "n(k)/N(k)"
  temp[2,3] <- "========="
  for (t in 1:num.stage) {
    if (t < num.stage) {
      temp[2+t,1] <- t
      temp[2+t,3] <- round(result$cond.frac.of.death.per.period[t], 4)
    } else {
      temp[2+num.stage] <- "Survive"
    }
    temp[2+t,2] <- result$observed[t]
  }
  temp[2+num.stage+1,1] <- "Total"
  temp[2+num.stage+1,2] <- sum(result$observed)
  s <- paste0(s, matrix.string(temp, spacing = 3))
  return(s)
}

survival.parse.stage.string <- function(s) {
  if (grepl("[^0-9 ,;-]", s)) {
    return(NA)
  }
  s.splitted <- strsplit(s, ";", fixed=TRUE)[[1]]
  if(length(s.splitted) > 1) {
    return(NA)
  }
  if (grepl(",\\s*$", s.splitted)) {
    return(NA)
  }
  if (grepl("^\\s*$", s.splitted)) {
    return(NA)
  }
  comma.splitted <- strsplit(s.splitted, ",", fixed=TRUE)[[1]]
  comma.member <- NULL
  for (term in comma.splitted) {
    if (grepl("^\\s*[1-9]\\d*\\s*$", term)) {
      v <- as.numeric(term)
      comma.member <- c(comma.member, v)
    } else if (grepl("^\\s*[1-9]\\d*\\s*-\\s*[1-9]\\d*\\s*$", term)) {
      interval.values <- strsplit(term, "-", fixed=TRUE)[[1]]
      v1 <- as.numeric(head(interval.values, 1))
      v2 <- as.numeric(tail(interval.values, 1))
      if (v1 > v2) {
        return(NA)
      }
      comma.member <- c(comma.member, v1:v2)
    } else if (!is.na(as.numeric(term)) && as.numeric(term) == -1) {
      comma.member <- c(comma.member, -1)
    } else {
      return(NA)
    }
  }
  if (length(comma.member) == 0) {
    return(NA)
  } else {
    return(sort(unique(comma.member)))
  }
}

# Loyalty logic
loyalty.adjust.phi <- function(phi) {
  num.brand <- length(phi)
  temp.a <- num.brand / (num.brand - 1)
  temp.b <- 2 * sum(phi) / (num.brand - 1)
  temp.c <- sum(phi)^2 / (num.brand - 1) - sum(phi^2)
  temp.d <- temp.b^2 - 4 * temp.a * temp.c
  adjusted.phi.1 <- phi + (-temp.b - sqrt(temp.d)) / (2 * temp.a)
  adjusted.phi.1 <- adjusted.phi.1 / sum(adjusted.phi.1)
  adjusted.phi.2 <- phi + (-temp.b + sqrt(temp.d)) / (2 * temp.a)
  adjusted.phi.2 <- adjusted.phi.2 / sum(adjusted.phi.2)
  if (sum((phi - adjusted.phi.1)^2) < sum((phi - adjusted.phi.2)^2)) {
    return(adjusted.phi.1)
  } else {
    return(adjusted.phi.2)
  }
}

pseudo.inv <- function(x) {
  x.svd <- svd(x)
  singular.value.threshold <- max(dim(x)) * max(x.svd$d) * .Machine$double.eps
  is.positive <- x.svd$d > singular.value.threshold
  if (!any(is.positive)) {
    return(matrix(0, nrow=dim(x)[2], ncol=dim(x)[1]))
  } else {
    return(x.svd$v[,is.positive] %*% (1/x.svd$d[is.positive] *
                                        t(x.svd$u[,is.positive])))
  }
}

loyalty.m.model.computation <- function(contingency.table,
                                        significance.threshold=1.64, p.value.threshold=0.05, p.value.only=FALSE) {
  
  UpdateAlphaBeta <- function(beta.old, delta, phi, observed.row.freq,
                              observed.col.freq) {
    
    temp.m <- outer(1 - phi, phi)
    diag(temp.m) <- delta * phi
    temp.m <- exp(temp.m)
    
    num.iter <- 30L
    for (iter in seq_len(num.iter)) {
      alpha <- observed.row.freq / as.vector(temp.m %*% beta.old)
      beta <- observed.col.freq / as.vector(alpha %*% temp.m)
      scaling <- mean(sqrt(beta / alpha))
      alpha <- scaling * alpha
      beta <- beta / scaling
      
      if (sqrt(sum((beta - beta.old)^2)) < 0.00001) {
        break
      }
      beta.old <- beta
    }
    return(list(alpha=as.vector(alpha), beta=as.vector(beta)))
  }
  
  UpdateExpectedProbs <- function(alpha, beta, delta, phi) {
    temp.m <- outer(1 - phi, phi)
    diag(temp.m) <- delta * phi
    return(outer(alpha, beta) * exp(temp.m))
  }
  
  num.brand <- nrow(contingency.table)
  num.people <- sum(contingency.table)
  observed.freq <- contingency.table / num.people
  observed.row.freq <- rowSums(contingency.table) / num.people
  observed.col.freq <- colSums(contingency.table) / num.people
  
  # Initialize the model parameters phi, delta, alpha, and beta.
  # For alpha and beta.
  alpha <- observed.row.freq
  beta <- observed.col.freq
  # For delta.
  observed.diag <- diag(contingency.table) # Row matrix.
  temp.mlog <- log(outer(observed.diag, observed.diag) / contingency.table /
                     t(contingency.table))
  delta <- (sum(temp.mlog) / 2 - (num.brand - 2) / (num.brand - 1)) /
    (num.brand * (num.brand - 1))
  # For phi.
  temp.r <- colSums(temp.mlog)
  temp.disc <- 1 - 4 * (temp.r / 2 + (num.brand - 1) * delta)
  if (any(temp.disc < 0)) {
    phi <- loyalty.adjust.phi((observed.row.freq + observed.col.freq) / 2)
  } else {
    phi <- (-1 - sqrt(temp.disc)) / 2
  }
  # Adjust alpha and beta.
  updated.alpha.and.beta <- UpdateAlphaBeta(beta, delta, phi,
                                            observed.row.freq, observed.col.freq)
  alpha <- updated.alpha.and.beta$alpha
  beta <- updated.alpha.and.beta$beta
  
  expected.probs <- UpdateExpectedProbs(alpha, beta, delta, phi)
  num_iter <- 5000L
  for (iter in seq_len(num_iter)) {
    temp.m <- cbind(1, 2*phi)
    proj <- diag(length(phi)) - temp.m %*% solve(t(temp.m) %*% temp.m, t(temp.m))
    temp.d <- observed.freq - expected.probs
    d.diag <- diag(temp.d);
    diag(temp.d) <- 0
    grad.phi <- as.vector((1 - phi) %*% temp.d) - as.vector(temp.d %*% phi) + d.diag * delta
    phi.step <- as.vector(proj %*% grad.phi)
    
    delta.step <- sum(d.diag * phi)
    phi <- loyalty.adjust.phi(phi + phi.step)
    delta <- delta + delta.step
    updated.alpha.and.beta <- UpdateAlphaBeta(beta, delta,
                                              phi, observed.row.freq, observed.col.freq)
    alpha <- updated.alpha.and.beta$alpha
    beta <- updated.alpha.and.beta$beta
    expected.probs <- UpdateExpectedProbs(alpha, beta, delta, phi)
    
    if (sqrt(sum(phi.step^2) + delta.step^2) < 1e-7) {
      break
    }
  }
  # Compute predator and prey tables.
  expected.row.probs <- rowSums(expected.probs)
  expected.col.probs <- colSums(expected.probs)
  predator.table <- expected.probs / 
    matrix(expected.row.probs - diag(expected.probs), num.brand, num.brand)
  diag(predator.table) <- NA
  prey.table <- t(expected.probs) / 
    matrix(expected.col.probs - diag(expected.probs), num.brand, num.brand)
  diag(prey.table) <- NA
  
  # Compute TRL and TAL.
  tal <- sum(diag(observed.freq))
  trl <- tal / sum(pmin(observed.row.freq, observed.col.freq))
  
  # Compute BRL and BRA.
  bra <- phi
  brl <- diag(expected.probs) / pmin(expected.row.probs, expected.col.probs)
  
  # Compute Fisher information matrix.
  expected <- expected.probs * num.people
  
  if (p.value.only) {
    observed <- contingency.table
    l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
    d.o.f <- (num.brand - 1) * (num.brand - 2)
    p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
    model.is.fit <- p.value > p.value.threshold
    return(list(p.value=p.value,
                model.is.fit=model.is.fit))
  }
  
  expected.offdiag <- expected
  diag(expected.offdiag) <- 0
  expected.diag <- diag(expected)
  # For phi vs phi.
  fisher.info.phi.phi <- expected * (1 - outer(1 - phi, phi)) +
    t(expected) * (1 - outer(phi, 1 - phi))
  diag(fisher.info.phi.phi) <- 2 * num.people +
    as.vector(expected.offdiag %*% phi^2) +
    as.vector((1 - phi)^2 %*% expected.offdiag) +
    delta^2 * expected.diag
  # For phi vs alpha.
  fisher.info.phi.alpha <- t(expected) *
    matrix((1 - phi) / alpha, num.brand, num.brand, byrow=TRUE)
  diag(fisher.info.phi.alpha) <- (delta * expected.diag -
                                    as.vector(expected.offdiag %*% phi)) / alpha
  # For phi vs beta.
  fisher.info.phi.beta <- -expected *
    matrix(phi / beta, num.brand, num.brand, byrow=TRUE)
  diag(fisher.info.phi.beta) <- (delta * expected.diag +
                                   as.vector((1 - phi) %*% expected.offdiag)) / beta
  # For phi vs delta
  fisher.info.phi.delta <- (delta * phi - 1) * expected.diag
  # For alpha vs alpha.
  fisher.info.alpha.alpha <- matrix(0, nrow=num.brand, ncol=num.brand)
  diag(fisher.info.alpha.alpha) <- rowSums(expected) / alpha^2
  # For alpha vs beta.
  fisher.info.alpha.beta <- expected / outer(alpha, beta)
  # For delta vs alpha.
  fisher.info.delta.alpha <- expected.diag * phi / alpha
  # For beta vs beta.
  fisher.info.beta.beta <- matrix(0, nrow=num.brand, ncol=num.brand)
  diag(fisher.info.beta.beta) <- colSums(expected) / beta^2
  # For beta vs delta.
  fisher.info.delta.beta <- expected.diag * phi / beta
  # For delta vs delta.
  fisher.info.delta.delta <- sum(expected.diag * phi^2)
  # For eta1 vs ?.
  fisher.info.phi.eta1 <- as.vector((1 - phi) %*% expected.offdiag) -
    as.vector(expected.offdiag %*% phi) +
    delta * expected.diag
  fisher.info.alpha.eta1 <- rowSums(expected) / alpha
  fisher.info.beta.eta1 <- colSums(expected) / beta
  fisher.info.delta.eta1 <- delta * sum(expected.diag)
  # For eta2 vs ?.
  fisher.info.phi.eta2 <- rep(num.people, num.brand)
  # For eta3 vs ?.
  fisher.info.phi.eta3 <- 2 * phi * num.people
  fisher.info <- matrix(0, 3*num.brand+4, 3*num.brand+4)
  fisher.info[1:num.brand,1:num.brand] <- fisher.info.phi.phi
  fisher.info[1:num.brand,num.brand+1] <- fisher.info.phi.delta
  fisher.info[1:num.brand,num.brand+1+(1:num.brand)] <- fisher.info.phi.alpha
  fisher.info[1:num.brand,2*num.brand+1+(1:num.brand)] <- fisher.info.phi.beta
  fisher.info[1:num.brand,3*num.brand+2] <- fisher.info.phi.eta1
  fisher.info[1:num.brand,3*num.brand+3] <- fisher.info.phi.eta2
  fisher.info[1:num.brand,3*num.brand+4] <- fisher.info.phi.eta3
  fisher.info[num.brand+1,num.brand+1] <- fisher.info.delta.delta
  fisher.info[num.brand+1,num.brand+1+(1:num.brand)] <- fisher.info.delta.alpha
  fisher.info[num.brand+1,2*num.brand+1+(1:num.brand)] <- fisher.info.delta.beta
  fisher.info[num.brand+1,3*num.brand+2] <- fisher.info.delta.eta1
  fisher.info[num.brand+1+(1:num.brand),num.brand+1+(1:num.brand)] <- fisher.info.alpha.alpha
  fisher.info[num.brand+1+(1:num.brand),2*num.brand+1+(1:num.brand)] <- fisher.info.alpha.beta
  fisher.info[num.brand+1+(1:num.brand),3*num.brand+2] <- fisher.info.alpha.eta1
  fisher.info[2*num.brand+1+(1:num.brand),2*num.brand+1+(1:num.brand)] <- fisher.info.beta.beta
  fisher.info[2*num.brand+1+(1:num.brand),3*num.brand+2] <- fisher.info.beta.eta1
  fisher.info[lower.tri(fisher.info)] <- t(fisher.info)[lower.tri(fisher.info)]
  fisher.info <- fisher.info / num.people
  
  # Compute significance matrix.
  phi.idx <-1:num.brand
  cov.matrix <- pseudo.inv(fisher.info[phi.idx, phi.idx])
  diag.cov.matrix <- diag(cov.matrix)
  
  # DEBUG M Model
  print("DEBUG M MODEL: phi values:")
  print(phi)
  print("DEBUG M MODEL: diag.cov.matrix:")
  print(diag.cov.matrix)
  print("DEBUG M MODEL: cov.matrix (full):")
  print(cov.matrix)
  
  significance.stat <- abs(outer(phi, phi, "-")) /
    sqrt(outer(diag.cov.matrix, diag.cov.matrix, "+") - 2 * cov.matrix) *
    sqrt(num.people / 2)
  significance.table <- significance.stat > significance.threshold
  
  print("DEBUG M MODEL: significance.stat:")
  print(significance.stat)
  print("DEBUG M MODEL: significance.table:")
  print(significance.table)
  
  observed <- contingency.table
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- (num.brand - 1) * (num.brand - 2)
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  
  return(list(observed=observed,
              expected=expected,
              tau.given.row=tau.given.row,
              tau.given.col=tau.given.col,
              chi.sq.stat=chi.sq.stat,
              l.sq.stat=l.sq.stat,
              d.o.f=d.o.f,
              p.value=p.value,
              delta=delta,
              phi=phi,
              predator.table=predator.table,
              prey.table=prey.table,
              trl=trl,
              tal=tal,
              brl=brl,
              bra=bra,
              num.people=num.people,
              fisher.info=fisher.info,
              cov.matrix=cov.matrix,
              significance.stat=significance.stat,
              significance.table=significance.table,
              model.is.fit=model.is.fit,
              std.residuals=std.residuals,
              num.sig.residuals=num.sig.residuals))
}

loyalty.q.model.computation <- function(contingency.table,
                                        significance.threshold=1.64, p.value.threshold=0.05, p.value.only=FALSE) {
  
  UpdateAlphaBeta <- function(beta.old, phi, observed.row.freq,
                              observed.col.freq, diag.expected.probs) {
    
    temp.m <- outer(1 - phi, phi)
    temp.m <- exp(temp.m)
    diag(temp.m) <- 0
    num_iter <- 30L
    for (iter in seq_len(num_iter)) {
      alpha <- (observed.row.freq - diag.expected.probs) / as.vector(temp.m %*% beta.old)
      beta <- (observed.col.freq - diag.expected.probs) / as.vector(alpha %*% temp.m)
      if (sqrt(sum((beta - beta.old)^2)) < 0.00001) {
        break
      }
      beta.old <- beta
    }
    return(list(alpha=as.vector(alpha), beta=as.vector(beta)))
  }
  
  UpdateExpectedProbs <- function(alpha, beta, phi, diag.observed.freq) {
    temp.m <- outer(alpha, beta) * exp(outer(1 - phi, phi))
    diag(temp.m) <- diag.observed.freq
    return(temp.m)
  }
  
  num.brand <- nrow(contingency.table)
  num.people <- sum(contingency.table)
  observed.freq <- contingency.table / num.people
  observed.row.freq <- rowSums(contingency.table) / num.people
  observed.col.freq <- colSums(contingency.table) / num.people
  
  # Initialize the model parameters phi, delta, alpha, and beta.
  observed.diag <- diag(contingency.table)
  temp.mlog <- log(outer(observed.diag, observed.diag) / contingency.table /
                     t(contingency.table))
  temp.r <- colSums(temp.mlog)
  temp.dl <- temp.r - diag(temp.mlog)
  temp.sumdl <- sum(temp.dl)
  temp.disc <- (num.brand - 2)^2 - 4 * (temp.r - temp.sumdl +
                                          (num.brand - 2) * temp.dl)
  if (any(temp.disc < 0)) {
    phi <- loyalty.adjust.phi((observed.row.freq + observed.col.freq) / 2)
  } else {
    phi <- (-1 - sqrt(temp.disc)) / 2
  }
  
  updated.alpha.and.beta <- UpdateAlphaBeta(beta=observed.col.freq,
                                            phi, observed.row.freq, observed.col.freq, numeric(num.brand))
  alpha <- updated.alpha.and.beta$alpha
  beta <- updated.alpha.and.beta$beta
  
  expected.probs <- UpdateExpectedProbs(alpha, beta, phi, diag(observed.freq))
  num_iter <- 5000L
  for (iter in seq_len(num_iter)) {
    temp.m <- cbind(1, 2*phi)
    proj <- diag(length(phi)) - temp.m %*% solve(t(temp.m) %*% temp.m, t(temp.m))
    temp.d <- observed.freq - expected.probs
    diag(temp.d) <- 0
    grad.phi <- as.vector((1 - phi) %*% temp.d) - as.vector(temp.d %*% phi)
    phi.step <- as.vector(proj %*% grad.phi)
    
    phi <- loyalty.adjust.phi(phi + phi.step)
    updated.alpha.and.beta <- UpdateAlphaBeta(beta, phi,
                                              observed.row.freq, observed.col.freq, diag(expected.probs))
    alpha <- updated.alpha.and.beta$alpha
    beta <- updated.alpha.and.beta$beta
    expected.probs <- UpdateExpectedProbs(alpha, beta, phi, diag(observed.freq))
    
    if (sqrt(sum(phi.step^2)) < 1e-7) {
      break
    }
  }
  
  # Compute predator and prey tables.
  expected.row.probs <- rowSums(expected.probs)
  expected.col.probs <- colSums(expected.probs)
  predator.table <- expected.probs / 
    matrix(expected.row.probs - diag(expected.probs), num.brand, num.brand)
  diag(predator.table) <- NA
  prey.table <- t(expected.probs) / 
    matrix(expected.col.probs - diag(expected.probs), num.brand, num.brand)
  diag(prey.table) <- NA
  
  # Compute TRL and TAL.
  tal <- sum(diag(observed.freq))
  trl <- tal / sum(pmin(observed.row.freq, observed.col.freq))
  
  # Compute BRL and BRA.
  bra <- phi
  brl <- diag(observed.freq) / pmin(expected.row.probs, expected.col.probs)
  
  # Compute Fisher information matrix.
  expected <- expected.probs * num.people
  
  if (p.value.only) {
    observed <- contingency.table
    l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
    d.o.f <- (num.brand - 1) * (num.brand - 3)
    p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
    model.is.fit <- p.value > p.value.threshold
    return(list(p.value=p.value,
                model.is.fit=model.is.fit))
  }
  
  expected.offdiag <- expected
  diag(expected.offdiag) <- 0
  expected.diag <- diag(expected)
  # For phi vs phi.
  fisher.info.phi.phi <- expected * (1 - outer(1 - phi, phi)) +
    t(expected) * (1 - outer(phi, 1 - phi))
  diag(fisher.info.phi.phi) <- 2 * num.people +
    as.vector(expected.offdiag %*% phi^2) +
    as.vector((1 - phi)^2 %*% expected.offdiag)
  # For phi vs alpha.
  fisher.info.phi.alpha <- t(expected) *
    matrix((1 - phi) / alpha, num.brand, num.brand, byrow=TRUE)
  diag(fisher.info.phi.alpha) <- as.vector(expected.offdiag %*% phi) / alpha
  # For phi vs beta.
  fisher.info.phi.beta <- -expected *
    matrix(phi / beta, num.brand, num.brand, byrow=TRUE)
  diag(fisher.info.phi.beta) <- as.vector((1 - phi) %*% expected.offdiag) / beta
  # For alpha vs alpha.
  fisher.info.alpha.alpha <- matrix(0, nrow=num.brand, ncol=num.brand)
  diag(fisher.info.alpha.alpha) <- rowSums(expected.offdiag) / alpha^2
  # For beta vs beta.
  fisher.info.beta.beta <- matrix(0, nrow=num.brand, ncol=num.brand)
  diag(fisher.info.beta.beta) <- colSums(expected.offdiag) / beta^2
  # For eta1 vs ?.
  fisher.info.phi.eta1 <- as.vector((1 - phi) %*% expected.offdiag) -
    as.vector(expected.offdiag %*% phi)
  fisher.info.alpha.eta1 <- rowSums(expected.offdiag) / alpha
  fisher.info.beta.eta1 <- colSums(expected.offdiag) / beta
  # For eta2 vs ?.
  fisher.info.phi.eta2 <- rep(num.people, num.brand)
  # For eta3 vs ?.
  fisher.info.phi.eta3 <- 2 * phi * num.people
  # Put the pieces together.
  # browser()
  fisher.info <- matrix(0, 3*num.brand+3, 3*num.brand+3)
  fisher.info[1:num.brand,1:num.brand] <- fisher.info.phi.phi
  fisher.info[1:num.brand,num.brand+(1:num.brand)] <- fisher.info.phi.alpha
  fisher.info[1:num.brand,2*num.brand+(1:num.brand)] <- fisher.info.phi.beta
  fisher.info[1:num.brand,3*num.brand+1] <- fisher.info.phi.eta1
  fisher.info[1:num.brand,3*num.brand+2] <- fisher.info.phi.eta2
  fisher.info[1:num.brand,3*num.brand+3] <- fisher.info.phi.eta3
  fisher.info[num.brand+(1:num.brand),num.brand+(1:num.brand)] <- fisher.info.alpha.alpha
  fisher.info[num.brand+(1:num.brand),3*num.brand+1] <- fisher.info.alpha.eta1
  fisher.info[2*num.brand+(1:num.brand),2*num.brand+(1:num.brand)] <- fisher.info.beta.beta
  fisher.info[2*num.brand+(1:num.brand),3*num.brand+1] <- fisher.info.beta.eta1
  fisher.info[lower.tri(fisher.info)] <- t(fisher.info)[lower.tri(fisher.info)]
  fisher.info <- fisher.info / num.people
  
  # browser()
  # Compute significance matrix.
  non.a.b.idx <- c(1:num.brand, 3*num.brand+1:3)
  cov.matrix <- pseudo.inv(fisher.info[non.a.b.idx,non.a.b.idx])
  cov.matrix <- cov.matrix[1:num.brand,1:num.brand]
  diag.cov.matrix <- diag(cov.matrix)
  significance.stat <- abs(outer(phi, phi, "-")) /
    sqrt(outer(diag.cov.matrix, diag.cov.matrix, "+") - 2 * cov.matrix) *
    sqrt(num.people / 2)
  significance.table <- significance.stat > significance.threshold
  
  # DEBUG Q Model
  print("DEBUG Q MODEL: phi values:")
  print(phi)
  print("DEBUG Q MODEL: diag.cov.matrix:")
  print(diag.cov.matrix)
  print("DEBUG Q MODEL: cov.matrix (full):")
  print(cov.matrix)
  print("DEBUG Q MODEL: significance.stat:")
  print(significance.stat)
  print("DEBUG Q MODEL: significance.table:")
  print(significance.table)
  
  observed <- contingency.table
  expected <- expected.probs * num.people
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- (num.brand - 1) * (num.brand - 3)
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  # browser()
  return(list(observed=observed,
              expected=expected,
              tau.given.row=tau.given.row,
              tau.given.col=tau.given.col,
              chi.sq.stat=chi.sq.stat,
              l.sq.stat=l.sq.stat,
              d.o.f=d.o.f,
              p.value=p.value,
              phi=phi,
              predator.table=predator.table,
              prey.table=prey.table,
              trl=trl,
              tal=tal,
              brl=brl,
              bra=bra,
              num.people=num.people,
              fisher.info=fisher.info,
              cov.matrix=cov.matrix,
              significance.stat=significance.stat,
              significance.table=significance.table,
              model.is.fit=model.is.fit,
              std.residuals=std.residuals,
              num.sig.residuals=num.sig.residuals))
}

loyalty.explore.model.computation <- function(contingency.table) {
  
  num.brand <- nrow(contingency.table)
  num.people <- sum(contingency.table)
  observed.freq <- contingency.table / num.people
  observed.row.freq <- rowSums(contingency.table) / num.people
  observed.col.freq <- colSums(contingency.table) / num.people
  
  # Compute predator and prey tables.
  expected.probs <- observed.freq
  expected.row.probs <- observed.row.freq
  expected.col.probs <- observed.col.freq 
  predator.table <- expected.probs / 
    matrix(expected.row.probs - diag(expected.probs), num.brand, num.brand)
  diag(predator.table) <- NA
  prey.table <- t(expected.probs) / 
    matrix(expected.col.probs - diag(expected.probs), num.brand, num.brand)
  diag(prey.table) <- NA
  
  # Compute TRL and TAL.
  tal <- sum(diag(observed.freq))
  trl <- tal / sum(pmin(observed.row.freq, observed.col.freq))
  
  # Compute BRL and BRA.
  bra <- observed.row.freq / observed.col.freq
  bra <- bra / sum(bra)
  brl <- diag(observed.freq) / pmin(expected.row.probs, expected.col.probs)
  
  observed <- contingency.table
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  
  return(list(observed=observed,
              tau.given.row=tau.given.row,
              tau.given.col=tau.given.col,
              predator.table=predator.table,
              prey.table=prey.table,
              trl=trl,
              tal=tal,
              brl=brl,
              bra=bra))
}

# Ranking logic
consecutive.partition <- function(n, p) {
  if (p == 1) {
    return(matrix(1, nrow=n, ncol=1))
  } else if (p == n) {
    return(matrix(1:n, nrow=n, ncol=1))
  } else {
    partitions <- matrix(p, nrow=n, ncol=choose(n-1,p-1))
    current.col.idx <- 0
    for (size.of.last.set in (n-(p-1)):1) {
      partitions.of.first <- consecutive.partition(n - size.of.last.set,
                                                   p - 1)
      partitions[1:(n - size.of.last.set),
                 current.col.idx+(1:ncol(partitions.of.first))] <-
        partitions.of.first
      current.col.idx <- current.col.idx + ncol(partitions.of.first)
    }
    return(partitions)
  }
}

is.stochastically.less.than.or.equal <- function(lhs, rhs) {
  return(all(lhs[-length(lhs)] <= rhs[-length(rhs)]))
}

stochastic.ordering <- function(cdf) {
  cdf[,ncol(cdf)] <- 1
  current.ordering <- 1
  num.cdf <- nrow(cdf)
  if (num.cdf == 1) {
    return(current.ordering)
  }
  for (i in 2:num.cdf) {
    insert.to.end <- TRUE
    for (j in seq_along(current.ordering)) {
      if (is.stochastically.less.than.or.equal(cdf[current.ordering[j],], cdf[i,])) {
        next
      } else if (is.stochastically.less.than.or.equal(cdf[i,],cdf[current.ordering[j],])) {
        if (j == 1) {
          current.ordering <- c(i, current.ordering)
        } else {
          current.ordering <- c(current.ordering[1:(j-1)], i,
                                current.ordering[j:length(current.ordering)])
        }
        insert.to.end <- FALSE
        break
      } else {
        return(NULL)
      }
    }
    if (insert.to.end) {
      current.ordering <- c(current.ordering, i)  
    }
  }
  return(current.ordering)
}

independence.indep.computation2 <- function(contingency.table,
                                            p.value.threshold=0.05) {
  
  observed <- contingency.table
  expected <- independence.expected.count(contingency.table)
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- (nrow(contingency.table) - 1) * (ncol(contingency.table) - 1)
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  
  return(list(observed=observed,
              expected=expected,
              tau.given.row=tau.given.row,
              tau.given.col=tau.given.col,
              chi.sq.stat=chi.sq.stat,
              l.sq.stat=l.sq.stat,
              d.o.f=d.o.f,
              p.value=p.value,
              model.is.fit=model.is.fit,
              std.residuals=std.residuals,
              num.sig.residuals=num.sig.residuals))
}

independence.OrganizeGroups <- function(groups, total.num.element) {
  membership <- numeric(total.num.element)
  for (g in seq_along(groups)) {
    membership[groups[[g]]] <- g
  }
  g <- length(groups) + 1L
  for (i in seq_len(total.num.element)) {
    if(membership[i] == 0L) {
      membership[i] <- g
      g <- g + 1L
    }
  }
  organized.membership <- numeric(total.num.element)
  index.ordering <- membership[!duplicated(membership)]
  total.num.group <- length(index.ordering)
  for (i in seq_len(total.num.group)) {
    organized.membership[membership == index.ordering[i]] <- i
  }
  organized.groups <- list()
  for (i in seq_len(total.num.group)) {
    organized.groups[[i]] <- which(organized.membership == i)
  }
  return(organized.groups)
}

independence.CollapseContingencyTable <- function(contingency.table, groups,
                                                  by.row=TRUE) {
  groups <- independence.OrganizeGroups(groups, ifelse(by.row,
                                                       nrow(contingency.table), ncol(contingency.table)))
  
  contingency.table.per.group <- list()
  non.single.groups <- list()
  i = 1L
  for (g in groups) {
    if (length(g) > 1L) {
      if (by.row) {
        contingency.table.per.group[[i]] <-
          contingency.table[g,,drop=FALSE]
      } else {
        contingency.table.per.group[[i]] <-
          contingency.table[,g,drop=FALSE]
      }
      non.single.groups[[i]] <- g
      i <- i + 1L
    }
  }
  num.group <- length(groups)
  if (by.row) {
    collapsed.contingency.table <- matrix(0, num.group,
                                          ncol(contingency.table))
    for (i in seq_len(num.group)) {
      collapsed.contingency.table[i,] <-
        colSums(contingency.table[groups[[i]],,drop=FALSE])
    }
  } else {
    collapsed.contingency.table <- matrix(0, nrow(contingency.table),
                                          num.group)
    for (i in seq_len(num.group)) {
      collapsed.contingency.table[,i] <-
        rowSums(contingency.table[,groups[[i]],drop=FALSE])
    }
  }
  return(list(groups=non.single.groups,
              contingency.table.per.group=contingency.table.per.group,
              collapsed.contingency.table=collapsed.contingency.table))
}

independence.anoas.computation2 <- function(contingency.table,
                                            original.contingency.table, groups, by.row=TRUE,
                                            information.p.value.threshold=0.05) {
  
  full.result <- independence.indep.computation2(contingency.table)
  more.contingency.tables <- independence.CollapseContingencyTable(
    contingency.table, groups, by.row)
  groups.result <- list()
  for (i in seq_along(more.contingency.tables$contingency.table.per.group)) {
    groups.result[[i]] <- independence.indep.computation2(
      more.contingency.tables$contingency.table.per.group[[i]])
  }
  collapsed.result <- independence.indep.computation2(
    more.contingency.tables$collapsed.contingency.table)
  
  change.in.l.sq <- full.result$l.sq.stat - collapsed.result$l.sq.stat
  change.in.d.o.f <- full.result$d.o.f - collapsed.result$d.o.f
  information.p.value <- pchisq(change.in.l.sq, df=change.in.d.o.f,
                                lower.tail=FALSE)
  information.is.lost <- information.p.value <= information.p.value.threshold
  
  collapsed.original.contingency.table <-
    independence.CollapseContingencyTable(original.contingency.table,
                                          groups, by.row)$collapsed.contingency.table
  
  return(list(full.result=full.result,
              groups=more.contingency.tables$groups,
              groups.result=groups.result,
              collapsed.result=collapsed.result,
              collapsed.original.contingency.table=collapsed.original.contingency.table,
              change.in.l.sq=change.in.l.sq,
              change.in.d.o.f=change.in.d.o.f,
              information.p.value=information.p.value,
              information.is.lost=information.is.lost))
}

ranking.exploratory.computation <- function(contingency.table,
                                            upper.polarity.idx=NULL) {
  DistanceBetweenTwoBrands <- function(first.level, second.level) {
    observed <- cbind(first.level, second.level)
    expected <- independence.expected.count(observed)
    return(independence.l.sq.chi.sq(observed, expected))
  }
  
  # Clustering the brands.
  num.brand <- nrow(contingency.table)
  num.level <- ncol(contingency.table)
  predefined.dist.threshold <- c(2.71, 4.60 ,6.25 ,7.78 ,9.24,
                                 10.64, 12.02 ,13.36 ,14.68, 15.99, 17.27 ,18.55, 19.81, 21.06 ,22.31,
                                 23.54, 24.77, 25.99, 27.20, 28.41, 29.61)
  dist.threshold <- predefined.dist.threshold[num.level - 1]
  brand.dist <- matrix(0, nrow=num.brand, ncol=num.brand)
  for (j in 1:(num.brand-1)) {
    for (i in (j+1):num.brand) {
      brand.dist[i,j] <- DistanceBetweenTwoBrands(contingency.table[i,],
                                                  contingency.table[j,])
      brand.dist[j,i] <- brand.dist[i,j]
    }
  }
  diag(brand.dist) <- dist.threshold + 1
  # Iteratively cluster the brands.
  cluster.label <- 1:num.brand
  for (t in seq_len(num.brand)) {
    idx <- arrayInd(which.min(brand.dist), dim(brand.dist))
    b1 <- idx[1]
    b2 <- idx[2]
    if (brand.dist[b1,b2] > dist.threshold) {
      break
    }
    cluster.label[cluster.label == cluster.label[b2]] <- cluster.label[b1]
    merged.cluster.idx <- which(cluster.label == cluster.label[b1])
    for (b in seq_len(num.brand)) {
      dist.from.b <- max(brand.dist[b,merged.cluster.idx])
      brand.dist[b,merged.cluster.idx] <- dist.from.b
      brand.dist[merged.cluster.idx,b] <- dist.from.b
    }
  }
  temp.label <- numeric(num.brand)
  cluster.order <- cluster.label[!duplicated(cluster.label)]
  for (i in seq_along(cluster.order)) {
    temp.label[cluster.label == cluster.order[i]] <- i
  }
  cluster.label <- temp.label
  num.cluster <- length(unique(cluster.label))

  # Collapse the contingency table by clusters of brands.
  if (num.cluster == num.brand) {
    collapsed.table <- contingency.table
  } else {
    collapsed.table <- matrix(NA, nrow=num.cluster, ncol=num.level)
    for (i in seq_len(num.cluster)) {
      collapsed.table[i,] <-
        colSums(contingency.table[cluster.label==i,,drop=FALSE])
    }
  }
  collapsed.contingency.table.rows <- collapsed.table
  collapsed.table <- collapsed.table / rowSums(collapsed.table)
  
  found.stochastic.ordering <- FALSE
  highest.l.sq.stat <- -Inf
  best.partition <- NULL
  for (num.col in num.level:1) {
    partitions <- consecutive.partition(num.level, num.col)
    for (p in seq_len(ncol(partitions))) {
      partition <- as.vector(partitions[,p])
      collapsed.c.table <- matrix(NA, nrow=num.cluster, ncol=num.col)
      for (i in seq_len(num.col)) {
        collapsed.c.table[,i] <-
          rowSums(collapsed.table[,partition==i,drop=FALSE])
      }
      cdf <- t(apply(collapsed.c.table, 1, cumsum))
      sto.ordering <- stochastic.ordering(cdf)
      if (!is.null(sto.ordering)) {
        found.stochastic.ordering <- TRUE
        l.sq.stat <- independence.l.sq.chi.sq(collapsed.c.table,
                                              independence.expected.count(collapsed.c.table))
        if (l.sq.stat > highest.l.sq.stat) {
          highest.l.sq.stat <- l.sq.stat
          best.partition <- partition
        }
      }
    }
    if (found.stochastic.ordering) {
      break
    }
  }
  num.collapsed.level <- length(unique(best.partition))
  collapsed.level.table <- matrix(NA, nrow=num.cluster,
                                  ncol=num.collapsed.level)
  for (i in seq_len(num.collapsed.level)) {
    collapsed.level.table[,i] <-
      rowSums(collapsed.table[,best.partition==i,drop=FALSE])
  }
  cdf <- t(apply(collapsed.level.table, 1, cumsum))
  cdf[,ncol(cdf)] <- 1
  stochastic.ordering <- stochastic.ordering(cdf)
  
  # Perform ANOAS analysis on collapsed.table
  brand.groups <- list()
  for (i in seq_len(num.cluster)) {
    brand.groups[[i]] <- which(cluster.label==i)
  }
  anoas.result <- independence.anoas.computation2(contingency.table,
                                                  contingency.table, brand.groups)
  
  # Compute polarity index.
  if (is.null(upper.polarity.idx)) {
    polarity.index <- collapsed.level.table[,num.collapsed.level] /
      collapsed.level.table[,1]
  } else {
    first.upper.idx <- min(upper.polarity.idx)
    partition.i <- best.partition[first.upper.idx]
    if (partition.i == 1) {
      partition.i <- 2
    }
    polarity.index <- rowSums(
      collapsed.level.table[,partition.i:num.collapsed.level,drop=FALSE]) /
      rowSums(collapsed.level.table[,1:(partition.i-1),drop=FALSE])
  }
  
  # Compute basic rankings.
  full.pmf <- contingency.table / rowSums(contingency.table)
  avg.satisfaction <- full.pmf %*% 1:num.level
  rank.by.avg <- order(avg.satisfaction, decreasing=TRUE)
  rank.by.best <- order(full.pmf[,num.level], decreasing=TRUE)
  rank.by.best.two <- order(rowSums(full.pmf[,c(num.level-1,num.level)]),
                            decreasing=TRUE)
  rank.by.worst <- order(full.pmf[,1], decreasing=TRUE)
  rank.by.worst.two <- order(rowSums(full.pmf[,1:2]), decreasing=TRUE)
  
  return(list(observed=anoas.result$full.result$observed,
              collapsed.contingency.table.rows = collapsed.contingency.table.rows,
              full.l.sq.stat=anoas.result$full.result$l.sq.stat,
              full.d.o.f=anoas.result$full.result$d.o.f,
              full.p.value=anoas.result$full.result$p.value,
              critical.value=dist.threshold,
              brand.cluster=cluster.label,
              satisfaction.partition=best.partition,
              collapsed.pmf=collapsed.level.table,
              stochastic.ordering=stochastic.ordering,
              polarity.index=polarity.index,
              change.in.l.sq=anoas.result$change.in.l.sq,
              change.in.d.o.f=anoas.result$change.in.d.o.f,
              information.p.value=anoas.result$information.p.value,
              information.is.lost=anoas.result$information.is.lost,
              avg.satisfaction=avg.satisfaction,
              rank.by.avg=rank.by.avg,
              rank.by.best=rank.by.best,
              rank.by.best.two=rank.by.best.two,
              rank.by.worst=rank.by.worst,
              rank.by.worst.two=rank.by.worst.two
              ))
}

spacing.AdjustMuOrNu <- function(x, prob) {
  x.mean <- sum(x * prob)
  x.var <- sum((x - x.mean)^2 * prob)
  return((x - x.mean) / sqrt(x.var))
}

lagrange.interpolation<- function(x, fx, x.new) {
  num.x.new <- length(x.new)
  num.x <- length(x)
  temp.m <- matrix(NA, num.x.new, num.x)
  for (i in seq_len(num.x.new))
    for (j in seq_len(num.x)) {
      temp.m[i,j] <- prod((x.new[i] - x[-j]) / (x[j] - x[-j]))
    }
  return(as.vector(temp.m %*% fx))
}

pseudo.inv <- function(x) {
  x.svd <- svd(x)
  singular.value.threshold <- max(dim(x)) * max(x.svd$d) *
    .Machine$double.eps
  is.positive <- x.svd$d > singular.value.threshold
  if (!any(is.positive)) {
    return(matrix(0, nrow=dim(x)[2], ncol=dim(x)[1]))
  } else {
    return(x.svd$v[,is.positive] %*% (1/x.svd$d[is.positive] *
                                        t(x.svd$u[,is.positive])))
  }
}

spacing.exponential.spacing.computation <- function(contingency.table,
                                                    poly.deg.row, poly.deg.col, p.value.threshold=0.05) {
  
  observed <- contingency.table
  observed.sum <- sum(observed)
  observed.row.sum <- rowSums(observed)
  observed.col.sum <- colSums(observed)
  observed.row.freq <- observed.row.sum / observed.sum
  observed.col.freq <- observed.col.sum / observed.sum
  num.row <- nrow(observed)
  num.col <- ncol(observed)
  
  ComputeExpected <- function(alpha, beta, mu, nu, phi, observed.sum) {
    return(outer(alpha, beta) * exp(phi * outer(mu, nu)) * observed.sum)
  }
  
  UpdateAlphaBetaExpected <- function(alpha, beta, mu, nu, phi, observed.sum,
                                      observed.row.sum, observed.col.sum) {
    
    expected <- ComputeExpected(alpha, beta, mu, nu, phi, observed.sum)
    num.iter <- 25L
    for (iter in seq_len(num.iter)) {
      alpha <- alpha * observed.row.sum / rowSums(expected)
      expected <- ComputeExpected(alpha, beta, mu, nu, phi, observed.sum)
      beta <- beta * observed.col.sum / colSums(expected)
      expected <- ComputeExpected(alpha, beta, mu, nu, phi, observed.sum)
    }
    return(list(alpha=alpha, beta=beta, expected=expected))
  }
  
  # Initialize the model parameters alpha, beta, mu, nu, and phi.
  alpha <- rep(1 / num.row, num.row)
  beta <- rep(1 / num.col, num.col)
  mu <- spacing.AdjustMuOrNu(1:num.row, observed.row.freq)
  nu <- spacing.AdjustMuOrNu(1:num.col, observed.col.freq)
  phi <- 0.1
  
  total.num.iter <- 50L
  total.num.mu.nu.iter <- 36L
  for (t in seq_len(total.num.iter)) {
    updated.alpha.beta.expected <- UpdateAlphaBetaExpected(alpha, beta, mu,
                                                           nu, phi, observed.sum, observed.row.sum, observed.col.sum)
    alpha <- updated.alpha.beta.expected$alpha
    beta <- updated.alpha.beta.expected$beta
    expected <- updated.alpha.beta.expected$expected
    if (t <= total.num.mu.nu.iter) {
      temp.o <- observed[1:(poly.deg.row+1),]
      temp.e <- expected[1:(poly.deg.row+1),]
      fx <- as.vector((temp.e - temp.o) %*% nu)
      dfdx <- as.vector(temp.e %*% (nu^2) * phi)
      mu[1:(poly.deg.row+1)] <- mu[1:(poly.deg.row+1)] - fx / dfdx
      if (poly.deg.row + 1 < num.row) {
        mu[(poly.deg.row+2):num.row] <-
          lagrange.interpolation(1:(poly.deg.row+1),
                                 mu[1:(poly.deg.row+1)], (poly.deg.row+2):num.row)
      }
      mu <- spacing.AdjustMuOrNu(mu, observed.row.freq)
      temp.o <- observed[,1:(poly.deg.col+1)]
      temp.e <- expected[,1:(poly.deg.col+1)]
      fx <- as.vector(mu %*% (temp.e - temp.o))
      dfdx <- as.vector((mu^2) %*% temp.e) * phi
      nu[1:(poly.deg.col+1)] <- nu[1:(poly.deg.col+1)] - fx / dfdx
      if (poly.deg.col + 1 < num.col) {
        nu[(poly.deg.col+2):num.col] <-
          lagrange.interpolation(1:(poly.deg.col+1),
                                 nu[1:(poly.deg.col+1)], (poly.deg.col+2):num.col)
      }
      nu <- spacing.AdjustMuOrNu(nu, observed.col.freq)
    }
    fx <- sum((expected - observed) * outer(mu, nu))
    dfdx <- sum(expected * outer(mu^2, nu^2))
    phi <- phi - fx / dfdx
  }
  updated.alpha.beta.expected <- UpdateAlphaBetaExpected(alpha, beta, mu,
                                                         nu, phi, observed.sum, observed.row.sum, observed.col.sum)
  alpha <- updated.alpha.beta.expected$alpha
  beta <- updated.alpha.beta.expected$beta
  expected <- updated.alpha.beta.expected$expected
  
  # Compute Fisher information matrix.
  # For mu vs mu.
  fisher.info.mu.mu <- matrix(0, num.row, num.row)
  diag(fisher.info.mu.mu) <- as.vector(expected %*% (nu^2)) * phi^2
  # For nu vs nu.
  fisher.info.nu.nu <- matrix(0, num.col, num.col)
  diag(fisher.info.nu.nu) <- as.vector((mu^2) %*% expected) * phi^2
  # For mu vs nu.
  fisher.info.mu.nu <- expected * ((phi^2) * outer(mu, nu) - phi)
  # For mu vs phi.
  fisher.info.mu.phi <- phi * mu * as.vector(expected %*% (nu^2))
  # For nu vs phi.
  fisher.info.nu.phi <- phi * nu * as.vector((mu^2) %*% expected)
  # For alpha vs mu.
  fisher.info.alpha.mu <- matrix(0, num.row, num.row)
  diag(fisher.info.alpha.mu) <- phi / alpha * as.vector(expected %*% nu)
  # For beta vs nu.
  fisher.info.beta.nu <- matrix(0, num.col, num.col)
  diag(fisher.info.beta.nu) <- phi / beta * as.vector(mu %*% expected)
  # For beta vs mu.
  fisher.info.beta.mu <- phi * t(expected) * (nu / beta)
  # For alpha vs nu.
  fisher.info.alpha.nu <- phi * expected * (mu / alpha)
  # For alpha vs alpha
  fisher.info.alpha.alpha <- matrix(0, num.row, num.row)
  diag(fisher.info.alpha.alpha) <- rowSums(expected) / (alpha^2)
  # For beta vs beta.
  fisher.info.beta.beta <- matrix(0, num.col, num.col)
  diag(fisher.info.beta.beta) <- colSums(expected) / (beta^2)
  # For alpha vs beta.
  fisher.info.alpha.beta <- expected / outer(alpha, beta)
  # For alpha vs phi.
  fisher.info.alpha.phi <- as.vector(expected %*% nu) * mu / alpha
  # For beta vs phi.
  fisher.info.beta.phi <- as.vector(mu %*% expected) * nu / beta
  # For phi vs phi.
  fisher.info.phi.phi <- sum(expected * outer(mu^2, nu^2))
  # For alpha vs eta.
  fisher.info.alpha.eta1 <- rowSums(expected) / alpha
  fisher.info.alpha.eta2 <- rowSums(expected) * mu / alpha
  fisher.info.alpha.eta3 <- as.vector(expected %*% nu) / alpha
  fisher.info.alpha.eta4 <- rowSums(expected) * (mu^2) / alpha
  fisher.info.alpha.eta5 <- as.vector(expected %*% (nu^2)) / alpha
  # For beta vs eta.
  fisher.info.beta.eta1 <- colSums(expected) / beta
  fisher.info.beta.eta2 <- as.vector(mu %*% expected) / beta
  fisher.info.beta.eta3 <- colSums(expected) * nu / beta
  fisher.info.beta.eta4 <- as.vector((mu^2) %*% expected) / beta
  fisher.info.beta.eta5 <- colSums(expected) * (nu^2) / beta
  # For phi vs eta.
  fisher.info.phi.eta1 <- sum(expected * outer(mu, nu))
  fisher.info.phi.eta2 <- sum(expected * outer(mu^2, nu))
  fisher.info.phi.eta3 <- sum(expected * outer(mu, nu^2))
  fisher.info.phi.eta4 <- sum(expected * outer(mu^3, nu))
  fisher.info.phi.eta5 <- sum(expected * outer(mu, nu^3))
  # For mu vs eta.
  fisher.info.mu.eta1 <- phi * as.vector(expected %*% nu)
  fisher.info.mu.eta2 <- rowSums(expected * (1 + phi * outer(mu, nu)))
  fisher.info.mu.eta3 <- phi * as.vector(expected %*% (nu^2))
  fisher.info.mu.eta4 <- rowSums(expected * (2 * mu + phi * outer(mu^2, nu)))
  fisher.info.mu.eta5 <- phi * as.vector(expected %*% (nu^3))
  # For nu vs eta.
  fisher.info.nu.eta1 <- phi * as.vector(mu %*% expected)
  fisher.info.nu.eta2 <- phi * as.vector((mu^2) %*% expected)
  fisher.info.nu.eta3 <- colSums(expected * (1 + phi * outer(mu, nu)))
  fisher.info.nu.eta4 <- phi * as.vector((mu^3) %*% expected)
  fisher.info.nu.eta5 <- rowSums(t(expected) * (2 * nu + phi * outer(nu^2, mu)))
  fisher.info <- matrix(0, 2*(num.row+num.col)+6, 2*(num.row+num.col)+6)
  fisher.info[1:num.row,1:num.row] <- fisher.info.alpha.alpha
  fisher.info[1:num.row,num.row+1:num.col] <- fisher.info.alpha.beta
  fisher.info[1:num.row,num.row+num.col+1:num.row] <- fisher.info.alpha.mu
  fisher.info[1:num.row,2*num.row+num.col+1:num.col] <- fisher.info.alpha.nu
  fisher.info[1:num.row,2*(num.row+num.col)+1] <- fisher.info.alpha.phi
  fisher.info[1:num.row,2*(num.row+num.col)+2] <- fisher.info.alpha.eta1
  fisher.info[1:num.row,2*(num.row+num.col)+3] <- fisher.info.alpha.eta2
  fisher.info[1:num.row,2*(num.row+num.col)+4] <- fisher.info.alpha.eta3
  fisher.info[1:num.row,2*(num.row+num.col)+5] <- fisher.info.alpha.eta4
  fisher.info[1:num.row,2*(num.row+num.col)+6] <- fisher.info.alpha.eta5
  fisher.info[num.row+1:num.col,num.row+1:num.col] <- fisher.info.beta.beta
  fisher.info[num.row+1:num.col,num.row+num.col+1:num.row] <- fisher.info.beta.mu
  fisher.info[num.row+1:num.col,2*num.row+num.col+1:num.col] <- fisher.info.beta.nu
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+1] <- fisher.info.beta.phi
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+2] <- fisher.info.beta.eta1
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+3] <- fisher.info.beta.eta2
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+4] <- fisher.info.beta.eta3
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+5] <- fisher.info.beta.eta4
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+6] <- fisher.info.beta.eta5
  fisher.info[num.row+num.col+1:num.row,num.row+num.col+1:num.row] <- fisher.info.mu.mu
  fisher.info[num.row+num.col+1:num.row,2*num.row+num.col+1:num.col] <- fisher.info.mu.nu
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+1] <- fisher.info.mu.phi
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+2] <- fisher.info.mu.eta1
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+3] <- fisher.info.mu.eta2
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+4] <- fisher.info.mu.eta3
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+5] <- fisher.info.mu.eta4
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+6] <- fisher.info.mu.eta5
  fisher.info[2*num.row+num.col+1:num.col,2*num.row+num.col+1:num.col] <- fisher.info.nu.nu
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+1] <- fisher.info.nu.phi
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+2] <- fisher.info.nu.eta1
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+3] <- fisher.info.nu.eta2
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+4] <- fisher.info.nu.eta3
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+5] <- fisher.info.nu.eta4
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+6] <- fisher.info.nu.eta5
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+1] <- fisher.info.phi.phi
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+2] <- fisher.info.phi.eta1
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+3] <- fisher.info.phi.eta2
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+4] <- fisher.info.phi.eta3
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+5] <- fisher.info.phi.eta4
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+6] <- fisher.info.phi.eta5
  fisher.info[lower.tri(fisher.info)] <- t(fisher.info)[lower.tri(fisher.info)]
  fisher.info <- fisher.info / observed.sum
  
  # Compute EASD.
  cov.matrix <-
    pseudo.inv(fisher.info)[(num.row+num.col+1):(2*(num.row+num.col)+1),
                            (num.row+num.col+1):(2*(num.row+num.col)+1)] / observed.sum
  # For mu.
  mu.easd <- diag(cov.matrix[1:num.row,1:num.row])
  mu.easd[mu.easd < 0] <- NA
  mu.easd <- sqrt(mu.easd)
  # For nu.
  nu.easd <- diag(cov.matrix[num.row+1:num.col,num.row+1:num.col])
  nu.easd[nu.easd < 0] <- NA
  nu.easd <- sqrt(nu.easd)
  # For phi.
  phi.easd <- cov.matrix[num.row+num.col+1,num.row+num.col+1]
  if (phi.easd < 0) {
    phi.easd <- NA
  } else {
    phi.easd <- sqrt(phi.easd)
  }
  
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- (num.row - 1) * (num.col - 1) - poly.deg.row - poly.deg.col + 1
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  
  max.odd.ratio <- -Inf
  min.odd.ratio <- Inf
  for (i1 in 1:(nrow(expected) - 1)) {
    for (i2 in (i1+1):nrow(expected)) {
      for (j1 in 1:(ncol(expected) - 1)) {
        for (j2 in (j1+1):ncol(expected)) {
          odd.ratio <- expected[i1,j1] * expected[i2,j2] / expected[i2,j1] / expected[i1,j2]
          if (odd.ratio > max.odd.ratio) {
            max.odd.ratio <- odd.ratio
            row.of.max.or <- c(i1, i2)
            col.of.max.or <- c(j1, j2)
          }
          if (odd.ratio < min.odd.ratio) {
            min.odd.ratio <- odd.ratio
            row.of.min.or <- c(i1, i2)
            col.of.min.or <- c(j1, j2)
          }
        }
      }
    }
  }
  if (poly.deg.row == 1) {
    row.of.min.or <- "Any"
  }
  if (poly.deg.col == 1) {
    col.of.min.or <- "Any"
  }
  
  return(list(observed=observed,
              expected=expected,
              tau.given.row=tau.given.row,
              tau.given.col=tau.given.col,
              chi.sq.stat=chi.sq.stat,
              l.sq.stat=l.sq.stat,
              d.o.f=d.o.f,
              p.value=p.value,
              poly.deg.row=poly.deg.row,
              poly.deg.col=poly.deg.col,
              mu=mu,
              nu=nu,
              phi=phi,
              mu.easd=mu.easd,
              nu.easd=nu.easd,
              phi.easd=phi.easd,
              cov.matrix=cov.matrix,
              model.is.fit=model.is.fit,
              std.residuals=std.residuals,
              num.sig.residuals=num.sig.residuals,
              row.of.max.or=row.of.max.or,
              col.of.max.or=col.of.max.or,
              max.odd.ratio=max.odd.ratio,
              row.of.min.or=row.of.min.or,
              col.of.min.or=col.of.min.or,
              min.odd.ratio=min.odd.ratio))
}

ranking.confirmatory.computation <- function(contingency.table,
                                             upper.polarity.idx=NULL, mu.diff.threshold=1.64, org.only=FALSE) {
  num.brand <- nrow(contingency.table)
  num.level <- ncol(contingency.table)
  org.exp.spacing.result <- spacing.exponential.spacing.computation(
    contingency.table, num.brand-1, num.level-1)
  
  if (org.only) {
    return(list(org.exp.spacing.result=org.exp.spacing.result))
  }
  # Iteratively cluster the brands.
  cluster.label <- numeric(num.brand)
  for (t in seq_len(num.brand)) {
    unclustered.idx <- which(cluster.label == 0)
    if (length(unclustered.idx) == 0) {
      break
    }
    b.idx <- unclustered.idx[which.max(org.exp.spacing.result$mu[unclustered.idx])]
    b.mu <- org.exp.spacing.result$mu[b.idx]
    b.var <- org.exp.spacing.result$cov.matrix[b.idx,b.idx]
    in.next.cluster <- logical(length(unclustered.idx))
    for (i in seq_along(unclustered.idx)) {
      i.idx <- unclustered.idx[i]
      if (i.idx == b.idx) {
        in.next.cluster[i] <- TRUE
        next
      }
      i.mu <- org.exp.spacing.result$mu[i.idx]
      i.var <- org.exp.spacing.result$cov.matrix[i.idx,i.idx]
      b.i.cov <- org.exp.spacing.result$cov.matrix[b.idx, i.idx]
      mu.diff <- (b.mu - i.mu) / sqrt(b.var + i.var - 2 * b.i.cov)
      if (mu.diff <= mu.diff.threshold) {
        in.next.cluster[i] <- TRUE
      }
    }
    cluster.label[unclustered.idx[in.next.cluster]] <- t
  }
  
  temp.label <- numeric(num.brand)
  cluster.order <- cluster.label[!duplicated(cluster.label)]
  for (i in seq_along(cluster.order)) {
    temp.label[cluster.label == cluster.order[i]] <- i
  }
  cluster.label <- temp.label
  num.cluster <- length(unique(cluster.label))
  if (num.cluster == num.brand) {
    collapsed.table <- contingency.table
  } else {
    collapsed.table <- matrix(NA, nrow=num.cluster, ncol=num.level)
    for (i in seq_len(num.cluster)) {
      collapsed.table[i,] <-
        colSums(contingency.table[cluster.label==i,,drop=FALSE])
    }
  }
  
  MergeOppositeTrend <- function(x, increasing=TRUE) {
    if (length(x) == 1) {
      return(1)
    }
    if (increasing) {
      if (x[2] <= x[1]) {
        possible.j <- which(x > x[1])
        if (length(possible.j) == 0) {
          return(rep(1, length(x)))
        }
        j <- possible.j[1]
      } else {
        j <- 2
      }
    } else {
      if (x[2] > x[1]) {
        possible.j <- which(x <= x[1])
        if (length(possible.j) == 1) {
          return(rep(1, length(x)))
        }
        j <- possible.j[2]
      } else {
        j <- 2
      }
    }
    return(c(rep(1, j-1),
             MergeOppositeTrend(x[j:length(x)], increasing) + 1))
  }
  
  current.partition <- 1:num.level
  no.more.collapsing <- FALSE
  while (tail(current.partition, 1) > 2 && !no.more.collapsing) {
    num.col <- tail(current.partition, 1)
    collapsed.c.table <- matrix(NA, nrow=num.cluster, ncol=num.col)
    for (i in seq_len(num.col)) {
      collapsed.c.table[,i] <-
        rowSums(collapsed.table[,current.partition==i,drop=FALSE])
    }
    exp.spacing.result <- spacing.exponential.spacing.computation(
      collapsed.c.table, num.cluster-1, num.col-1)
    
    nu.partition <- MergeOppositeTrend(exp.spacing.result$nu,
                                       exp.spacing.result$phi > 0)
    if (tail(nu.partition, 1) == num.col) {
      no.more.collapsing <- TRUE
    } else {
      for (i in seq_along(nu.partition)) {
        current.partition[current.partition==i] <- nu.partition[i]
      }
    }
  }
  num.collapsed.level <- tail(current.partition, 1)
  collapsed.level.table <- matrix(NA, nrow=num.cluster,
                                  ncol=num.collapsed.level)
  for (i in seq_len(num.collapsed.level)) {
    collapsed.level.table[,i] <-
      rowSums(collapsed.table[,current.partition==i,drop=FALSE])
  }
  
  # Fit an exponential spacing model to collapsed.level.table.
  collapsed.exp.spacing.result <- spacing.exponential.spacing.computation(
    collapsed.level.table, num.cluster-1, num.collapsed.level-1)
  expected.collapsed.pmf <- collapsed.exp.spacing.result$expected /
    rowSums(collapsed.exp.spacing.result$expected)
  cdf <- t(apply(expected.collapsed.pmf, 1, cumsum))
  cdf[,ncol(cdf)] <- 1 # To avoid numerical issue.
  stochastic.ordering <- stochastic.ordering(cdf)
  has.stochastic.ordering <- !is.null(stochastic.ordering)
  
  # Perform ANOAS analysis on collapsed.table
  brand.groups <- list()
  for (i in seq_len(num.cluster)) {
    brand.groups[[i]] <- which(cluster.label==i)
  }
  anoas.result <- independence.anoas.computation2(contingency.table,
                                                  contingency.table, brand.groups)
  
  # Compute polarity index.
  if (is.null(upper.polarity.idx)) {
    polarity.index <- expected.collapsed.pmf[,num.collapsed.level] /
      expected.collapsed.pmf[,1]
  } else {
    first.upper.idx <- min(upper.polarity.idx)
    partition.i <- current.partition[first.upper.idx]
    if (partition.i == 1) { # Ad-hoc to avoid no lower tail.
      partition.i <- 2
    }
    polarity.index <- rowSums(
      expected.collapsed.pmf[, partition.i:num.collapsed.level, drop=FALSE]) /
      rowSums(expected.collapsed.pmf[, 1:(partition.i-1), drop=FALSE])
  }
  
  # Compute basic rankings.
  full.pmf <- contingency.table / rowSums(contingency.table)
  avg.satisfaction <- full.pmf %*% 1:num.level
  rank.by.avg <- order(avg.satisfaction, decreasing=TRUE)
  rank.by.best <- order(full.pmf[,num.level], decreasing=TRUE)
  rank.by.best.two <- order(rowSums(full.pmf[,c(num.level-1,num.level)]),
                            decreasing=TRUE)
  rank.by.worst <- order(full.pmf[,1], decreasing=TRUE)
  rank.by.worst.two <- order(rowSums(full.pmf[,1:2]), decreasing=TRUE)
  return(list(org.exp.spacing.result=org.exp.spacing.result,
              brand.cluster=cluster.label,
              satisfaction.partition=current.partition,
              collapsed.exp.spacing.result=collapsed.exp.spacing.result,
              expected.collapsed.pmf=expected.collapsed.pmf,
              stochastic.ordering=stochastic.ordering,
              has.stochastic.ordering=has.stochastic.ordering,
              polarity.index=polarity.index,
              anoas.result=anoas.result,
              avg.satisfaction=avg.satisfaction,
              rank.by.avg=rank.by.avg,
              rank.by.best=rank.by.best,
              rank.by.best.two=rank.by.best.two,
              rank.by.worst=rank.by.worst,
              rank.by.worst.two=rank.by.worst.two))
}

# Spacing logic
spacing.canonical.correlation.computation <- function(contingency.table,
                                                      poly.deg.row, poly.deg.col, p.value.threshold=0.05) {
  
  observed <- contingency.table
  observed.sum <- sum(observed)
  observed.row.sum <- rowSums(observed)
  observed.col.sum <- colSums(observed)
  observed.row.freq <- observed.row.sum / observed.sum
  observed.col.freq <- observed.col.sum / observed.sum
  num.row <- nrow(observed)
  num.col <- ncol(observed)
  
  # Initialize the model parameters alpha, beta, mu, nu, and phi.
  alpha <- observed.row.freq
  beta <- observed.col.freq
  mu <- spacing.AdjustMuOrNu(1:num.row, observed.row.freq)
  nu <- spacing.AdjustMuOrNu(1:num.col, observed.col.freq)
  phi <- abs(sum(observed * outer(mu, nu)) / observed.sum)
  
  # Use iterative algorithm to estimate the model parameters.
  total.num.iter <- 50L
  for (t in seq_len(total.num.iter)) {
    temp.one <- observed[1:(poly.deg.row+1),] /
      (1 + phi * outer(mu[1:(poly.deg.row+1)], nu))
    temp.two <-
      outer(observed.row.sum[1:(poly.deg.row+1)], observed.col.sum)
    fx <- as.vector((temp.one - temp.two) %*% nu)
    temp <- observed[1:(poly.deg.row+1),] /
      (1 + phi * outer(mu[1:(poly.deg.row+1)], nu))^2
    dfdx <- -phi * as.vector(temp %*% (nu^2))
    mu[1:(poly.deg.row+1)] <- mu[1:(poly.deg.row+1)] - fx / dfdx
    if (poly.deg.row + 1 < num.row) {
      mu[(poly.deg.row+2):num.row] <-
        lagrange.interpolation(1:(poly.deg.row+1),
                               mu[1:(poly.deg.row+1)], (poly.deg.row+2):num.row)
    }
    mu <- spacing.AdjustMuOrNu(mu, observed.row.freq)
    temp.one <- observed[,1:(poly.deg.col+1)] /
      (1 + phi * outer(mu, nu[1:(poly.deg.col+1)]))
    temp.two <-
      outer(observed.row.sum, observed.col.sum[1:(poly.deg.col+1)])
    fx <- as.vector(mu %*% (temp.one - temp.two))
    temp <- observed[,1:(poly.deg.col+1)] /
      (1 + phi * outer(mu, nu[1:(poly.deg.col+1)]))^2
    dfdx <- -phi * as.vector((mu^2) %*% temp)
    nu[1:(poly.deg.col+1)] <- nu[1:(poly.deg.col+1)] - fx / dfdx
    if (poly.deg.col + 1 < num.col) {
      nu[(poly.deg.col+2):num.col] <-
        lagrange.interpolation(1:(poly.deg.col+1),
                               nu[1:(poly.deg.col+1)], (poly.deg.col+2):num.col)
    }
    nu <- spacing.AdjustMuOrNu(nu, observed.col.freq)
    temp.omn <- outer(mu, nu)
    fx <- sum((observed / (1 + phi * temp.omn) -
                 outer(observed.row.sum, observed.col.sum)) * temp.omn)
    dfdx <- -sum(observed * outer(mu^2, nu^2) / (1 + phi * temp.omn)^2)
    phi <- phi - fx / dfdx
  }
  
  # Compute expected data.
  expected <- outer(observed.row.freq, observed.col.freq) *
    (1 + phi * outer(mu, nu)) * observed.sum
  
  # Compute Fisher information matrix.
  zeta <- 1 + phi * outer(mu, nu)
  # For mu vs mu.
  fisher.info.mu.mu <- matrix(0, num.row, num.row)
  diag(fisher.info.mu.mu) <- phi^2 *
    as.vector((expected / zeta^2) %*% (nu^2)) / observed.sum
  # For nu vs nu.
  fisher.info.nu.nu <- matrix(0, num.col, num.col)
  diag(fisher.info.nu.nu) <- phi^2 *
    as.vector((mu^2) %*% (expected / zeta^2)) / observed.sum
  # For mu vs nu.
  fisher.info.mu.nu <- phi^2 * (expected * outer(mu, nu) / zeta^2 /
                                  observed.sum) + phi * outer(alpha, beta)
  # For mu vs phi.
  fisher.info.mu.phi <- phi * (as.vector((expected / zeta^2) %*% (nu^2)) *
                                 mu / observed.sum + sum(beta * nu) * alpha)
  # For nu vs phi.
  fisher.info.nu.phi <- phi * (as.vector((mu^2) %*% (expected / zeta^2)) *
                                 nu / observed.sum + sum(alpha * mu) * beta)
  # For alpha vs mu.
  fisher.info.alpha.mu <- matrix(0, num.row, num.row)
  diag(fisher.info.alpha.mu) <- phi * sum(beta * nu)
  # For beta vs nu.
  fisher.info.beta.nu <- matrix(0, num.col, num.col)
  diag(fisher.info.beta.nu) <- phi * sum(alpha * mu)
  # For beta vs mu.
  fisher.info.beta.mu <- phi * outer(nu, alpha)
  # for alpha vs nu.
  fisher.info.alpha.nu <- phi * outer(mu, beta)
  # For alpha vs alpha
  fisher.info.alpha.alpha <- matrix(0, num.row, num.row)
  diag(fisher.info.alpha.alpha) <- observed.row.freq / (alpha^2)
  # For beta vs beta.
  fisher.info.beta.beta <- matrix(0, num.col, num.col)
  diag(fisher.info.beta.beta) <- observed.col.freq / (beta^2)
  # For alpha vs beta.
  fisher.info.alpha.beta <- zeta
  # For alpha vs phi.
  fisher.info.alpha.phi <- sum(beta * nu) * mu
  # For beta vs phi.
  fisher.info.beta.phi <- sum(alpha * mu) * nu
  # For phi vs phi.
  fisher.info.phi.phi <- sum(outer(mu^2, nu^2) * expected / zeta^2) /
    observed.sum
  # For alpha vs eta.
  fisher.info.alpha.eta1 <- rep(1, num.row)
  fisher.info.alpha.eta2 <- rep(0, num.row)
  fisher.info.alpha.eta3 <- mu
  fisher.info.alpha.eta4 <- rep(0, num.row)
  fisher.info.alpha.eta5 <- mu^2
  fisher.info.alpha.eta6 <- rep(0, num.row)
  # For beta vs eta.
  fisher.info.beta.eta1 <- rep(0, num.col)
  fisher.info.beta.eta2 <- rep(1, num.col)
  fisher.info.beta.eta3 <- rep(0, num.col)
  fisher.info.beta.eta4 <- nu
  fisher.info.beta.eta5 <- rep(0, num.col)
  fisher.info.beta.eta6 <- nu^2
  # For mu vs eta.
  fisher.info.mu.eta1 <- rep(0, num.row)
  fisher.info.mu.eta2 <- rep(0, num.row)
  fisher.info.mu.eta3 <- alpha
  fisher.info.mu.eta4 <- rep(0, num.row)
  fisher.info.mu.eta5 <- 2 * mu * alpha
  fisher.info.mu.eta6 <- rep(0, num.row)
  # For nu vs eta.
  fisher.info.nu.eta1 <- rep(0, num.col)
  fisher.info.nu.eta2 <- rep(0, num.col)
  fisher.info.nu.eta3 <- rep(0, num.col)
  fisher.info.nu.eta4 <- beta
  fisher.info.nu.eta5 <- rep(0, num.col)
  fisher.info.nu.eta6 <- 2 * nu * beta
  # Put the pieces together.
  fisher.info <- matrix(0, 2*(num.row+num.col)+7, 2*(num.row+num.col)+7)
  fisher.info[1:num.row,1:num.row] <- fisher.info.alpha.alpha
  fisher.info[1:num.row,num.row+1:num.col] <- fisher.info.alpha.beta
  fisher.info[1:num.row,num.row+num.col+1:num.row] <- fisher.info.alpha.mu
  fisher.info[1:num.row,2*num.row+num.col+1:num.col] <- fisher.info.alpha.nu
  fisher.info[1:num.row,2*(num.row+num.col)+1] <- fisher.info.alpha.phi
  fisher.info[1:num.row,2*(num.row+num.col)+2] <- fisher.info.alpha.eta1
  fisher.info[1:num.row,2*(num.row+num.col)+3] <- fisher.info.alpha.eta2
  fisher.info[1:num.row,2*(num.row+num.col)+4] <- fisher.info.alpha.eta3
  fisher.info[1:num.row,2*(num.row+num.col)+5] <- fisher.info.alpha.eta4
  fisher.info[1:num.row,2*(num.row+num.col)+6] <- fisher.info.alpha.eta5
  fisher.info[1:num.row,2*(num.row+num.col)+7] <- fisher.info.alpha.eta6
  fisher.info[num.row+1:num.col,num.row+1:num.col] <- fisher.info.beta.beta
  fisher.info[num.row+1:num.col,num.row+num.col+1:num.row] <- fisher.info.beta.mu
  fisher.info[num.row+1:num.col,2*num.row+num.col+1:num.col] <- fisher.info.beta.nu
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+1] <- fisher.info.beta.phi
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+2] <- fisher.info.beta.eta1
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+3] <- fisher.info.beta.eta2
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+4] <- fisher.info.beta.eta3
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+5] <- fisher.info.beta.eta4
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+6] <- fisher.info.beta.eta5
  fisher.info[num.row+1:num.col,2*(num.row+num.col)+7] <- fisher.info.beta.eta6
  fisher.info[num.row+num.col+1:num.row,num.row+num.col+1:num.row] <- fisher.info.mu.mu
  fisher.info[num.row+num.col+1:num.row,2*num.row+num.col+1:num.col] <- fisher.info.mu.nu
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+1] <- fisher.info.mu.phi
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+2] <- fisher.info.mu.eta1
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+3] <- fisher.info.mu.eta2
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+4] <- fisher.info.mu.eta3
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+5] <- fisher.info.mu.eta4
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+6] <- fisher.info.mu.eta5
  fisher.info[num.row+num.col+1:num.row,2*(num.row+num.col)+7] <- fisher.info.mu.eta6
  fisher.info[2*num.row+num.col+1:num.col,2*num.row+num.col+1:num.col] <- fisher.info.nu.nu
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+1] <- fisher.info.nu.phi
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+2] <- fisher.info.nu.eta1
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+3] <- fisher.info.nu.eta2
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+4] <- fisher.info.nu.eta3
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+5] <- fisher.info.nu.eta4
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+6] <- fisher.info.nu.eta5
  fisher.info[2*num.row+num.col+1:num.col,2*(num.row+num.col)+7] <- fisher.info.nu.eta6
  fisher.info[2*(num.row+num.col)+1,2*(num.row+num.col)+1] <- fisher.info.phi.phi
  fisher.info[lower.tri(fisher.info)] <- t(fisher.info)[lower.tri(fisher.info)]
  
  # Compute EASD.
  cov.matrix <-
    pseudo.inv(fisher.info)[(num.row+num.col+1):(2*(num.row+num.col)+1),
                            (num.row+num.col+1):(2*(num.row+num.col)+1)]/ observed.sum
  # For mu.
  mu.easd <- diag(cov.matrix[1:num.row,1:num.row])
  mu.easd[mu.easd < 0] <- NA
  mu.easd <- sqrt(mu.easd)
  # For nu.
  nu.easd <- diag(cov.matrix[num.row+1:num.col,num.row+1:num.col])
  nu.easd[nu.easd < 0] <- NA
  nu.easd <- sqrt(nu.easd)
  # For phi.
  phi.easd <- cov.matrix[num.row+num.col+1,num.row+num.col+1]
  if (phi.easd < 0) {
    phi.easd <- NA
  } else {
    phi.easd <- sqrt(phi.easd)
  }
  
  tau.given.row <- independence.goodman.kruskal.tau(observed, by.col.given.row=TRUE)
  tau.given.col <- independence.goodman.kruskal.tau(observed, by.col.given.row=FALSE)
  chi.sq.stat <- independence.pearson.chi.sq(observed, expected)
  l.sq.stat <- independence.l.sq.chi.sq(observed, expected)
  d.o.f <- (num.row - 1) * (num.col - 1) - poly.deg.row - poly.deg.col + 1
  p.value <- pchisq(l.sq.stat, df=d.o.f, lower.tail=FALSE)
  model.is.fit <- p.value > p.value.threshold
  std.residuals <- independence.pearson.residuals(observed, expected)
  num.sig.residuals <- independence.num.signif.residual(std.residuals)
  
  return(list(observed=observed,
              expected=expected,
              tau.given.row=tau.given.row,
              tau.given.col=tau.given.col,
              chi.sq.stat=chi.sq.stat,
              l.sq.stat=l.sq.stat,
              d.o.f=d.o.f,
              p.value=p.value,
              poly.deg.row=poly.deg.row,
              poly.deg.col=poly.deg.col,
              mu=mu,
              nu=nu,
              phi=phi,
              mu.easd=mu.easd,
              nu.easd=nu.easd,
              phi.easd=phi.easd,
              cov.matrix=cov.matrix,
              model.is.fit=model.is.fit,
              std.residuals=std.residuals,
              num.sig.residuals=num.sig.residuals))
}

# Scale conversion logic
scale.conversion.computation <- function(row.prob, col.prob) {
  num.row <- length(row.prob)
  num.col <- length(col.prob)
  joint.prob <- matrix(0, nrow=num.row, ncol=num.col)
  i <- 1
  j <- 1
  while (i <= num.row && j <= num.col) {
    joint.prob[i,j] <- min(row.prob[i], col.prob[j])
    if (row.prob[i] > col.prob[j]) {
      row.prob[i] <- row.prob[i] - joint.prob[i,j]
      j <- j + 1
    } else {
      col.prob[j] <- col.prob[j] - joint.prob[i,j]
      i <- i + 1
    }
  }
  return(joint.prob)
}

# Regression logic
regression.hoslem.test <- function(y, y.hat, g=10) {
  bin.edge <- unique(quantile(y.hat, probs=seq(0, 1, 1 / g)))
  y.hat.factor <- cut(y.hat, breaks=bin.edge, include.lowest=TRUE)
  observed <- xtabs(cbind(y0=1-y, y1=y) ~ y.hat.factor)
  expected <- xtabs(cbind(y0.hat=1-y.hat, y1.hat=y.hat) ~ y.hat.factor)
  is.zero <- expected == 0 # Avoid numerical issue.
  chi.sq <- sum((observed[!is.zero] - expected[!is.zero]) ^ 2 /
                  expected[!is.zero])
  nonzero.g <- sum(!is.zero) / 2
  d.o.f <- nonzero.g  - 2
  p.value <- pchisq(chi.sq, df=d.o.f, lower.tail=FALSE)
  return(list(chi.sq=chi.sq, d.o.f=d.o.f, p.value=p.value))
}

regression.create.forest.graph.obj <- function(odd, lower.bound, upper.bound,
                                               variable.name, p.value, p.value.threshold) {
  # Create a data frame.
  p.value.status <- as.factor(ifelse(p.value <= p.value.threshold,
                                     paste0("p-value =< ", p.value.threshold),
                                     paste0("p-value > ", p.value.threshold)))
  df <- data.frame(odd=odd, lower.bound=lower.bound, upper.bound=upper.bound,
                   variable.name=variable.name, p.value.status=p.value.status)
  if (length(levels(p.value.status)) == 2) {
    fill.color <- c(blue.color, red.color)
  } else if (p.value[1] <= p.value.threshold[1]) {
    fill.color <- blue.color
  } else {
    fill.color <- red.color
  }
  # Create ggplot object.
  plot.obj <- ggplot(df, aes(x=odd, y=variable.name)) +
    xlab("Odds Estimate") +
    ylab("Explanatory Variable") +
    ggtitle("Forest Plot") +
    geom_vline(xintercept=1, color="darkgray", linetype="longdash") +
    geom_errorbarh(aes(xmin=lower.bound, xmax=upper.bound), height=0.1) +
    geom_point(aes(fill=p.value.status), size=6, shape=21) +
    scale_fill_manual(values=fill.color, name="") +
    theme_grey(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot.obj)
}

regression.create.roc.graph.obj <- function(tpr, tpr.lower, tpr.upper, fpr,
                                            fpr.lower, fpr.upper, auc, is.best) {
  # Create the data frame.
  df <- data.frame(tpr=tpr, fpr=fpr)
  df.lower <- data.frame(tpr.lower=tpr.lower, fpr.upper=fpr.upper)
  df.upper <- data.frame(tpr.upper=tpr.upper, fpr.lower=fpr.lower)
  df.best <- subset(df, is.best)
  # Create ggplot object.
  plot.obj <- ggplot() +
    xlab("1 - Specificity (False Positive Rate FPR)") +
    ylab("Sensitivity (True Positive Rate TPR)") +
    ggtitle(paste0("ROC Plot (AUC=",
                   round(auc, 4),")")) +
    xlim(0, 1) +
    ylim(0, 1) +
    geom_abline(intercept=0, slope=1, color="darkgray", linetype="longdash") + 
    geom_path(aes(fpr, tpr), df, color=blue.color) +
    geom_path(aes(fpr.upper, tpr.lower), df.lower, color=blue.color,
              linetype="dashed") +
    geom_path(aes(fpr.lower, tpr.upper), df.upper, color=blue.color,
              linetype="dashed") +
    geom_point(aes(fpr, tpr), data=df.best, size=5, shape=21, color="white",
               fill=red.color) + 
    theme_grey(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot.obj)
}

regression.create.acc.graph.obj <- function(tau, acc, is.best) {
  # Create the data frame.
  df <- data.frame(tau=tau, acc=acc)
  df.best <- subset(df, is.best)
  # Create ggplot object.
  plot.obj <- ggplot() +
    xlab("Cut-Off Level") +
    ylab("Accuracy") +
    ggtitle("Accuracy Plot") +
    xlim(0, 1) +
    ylim(0, 1) +
    geom_path(aes(tau, acc), df, color=blue.color) +
    geom_point(aes(tau, acc), data=df.best, size=5, shape=21, color="white",
               fill=red.color) +
    theme_grey(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot.obj)
}

regression.create.auc.graph.obj <- function(tpr, auc, is.best) {
  # Create the data frame.
  df <- data.frame(tpr=tpr, auc=auc)
  df.best <- subset(df, is.best)
  # Create ggplot object.
  plot.obj <- ggplot() +
    xlab("Sensitivity (True Positive Rate TPR)") +
    ylab("AUC") +
    ggtitle("Cumulative Area Under Curve Plot") +
    xlim(0, 1) +
    ylim(0, 1) +
    geom_path(aes(tpr, auc), df, color=blue.color) +
    geom_point(aes(tpr, auc), data=df.best, size=5, shape=21, color="white",
               fill=red.color) +
    theme_grey(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot.obj)
}

server <- function(input, output, session) {
  rv <- reactiveValues()
  
  # Independence variable
  rv$independence.data <- NULL
  independence.tab.counter <- 0
  
  observeEvent(input$independenceLogOddOrientation, {
    if (input$independenceLogOddOrientation == "Row") {
      updateTextInput(session, "independenceLogOddWhichTwo",
                      label = "Which two rows?",
                      value = input$independenceLogOddWhichTwo)
    } else {
      updateTextInput(session, "independenceLogOddWhichTwo",
                      label = "Which two columns?",
                      value = input$independenceLogOddWhichTwo)
    }
  })
  
  output$independenceData <- renderRHandsontable({
    req(rv$independence.data)
    rhandsontable(rv$independence.data, useTypes = FALSE) %>%
      hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
      hot_cols(colWidths = 80) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  observeEvent(input$independenceDataCreate, {
    updateTabsetPanel(session, "independenceTab", selected = "Data")
    if (!string.is.dimension(input$independenceDataDimension, 2)) {
      showModal(modalDialog(title = "Oops!", "Invalid data dimensions.",
                            easyClose = TRUE))
      return()
    }
    d <- parse.dimension.string(input$independenceDataDimension)
    if (is.null(rv$independence.data)) {
      rv$independence.data <- empty.data(d[1], d[2])
    } else if (!all(dim(rv$independence.data) == d)) {
      showModal(modalDialog(title = "Confirmation", "Modify the data dimensions?",
                            footer = tagList(actionButton("independenceDataModifyOk", "OK"),
                                             actionButton("independenceDataModifyCancel", "Cancel"))))
    }
  })
  
  observeEvent(input$independenceDataModifyOk, {
    d <- parse.dimension.string(input$independenceDataDimension)
    row <- min(nrow(rv$independence.data), d[1])
    col <- min(ncol(rv$independence.data), d[2])
    df <- empty.data(d[1], d[2])
    df[1:row,1:col] <- hot_to_r(input$independenceData)[1:row,1:col]
    rv$independence.data <- df
    removeModal()
  })
  
  observeEvent(input$independenceDataModifyCancel, {
    updateTextInput(session, "independenceDataDimension",
                    value = paste(dim(rv$independence.data), collapse = ","))
    removeModal()
  })
  
  observeEvent(input$independenceDataFile, {
    showModal(modalDialog(title = "File Information",
                          radioButtons("independenceDataFileFormat",
                                       label = "Data format",
                                       choices = list("Rows of subjects", "Contingency table")),
                          textInput("independenceDataFileRow", label = "Which column in file for row?"),
                          textInput("independenceDataFileColumn", label = "Which column in file for column?"),
                          footer = tagList(actionButton("independenceDataFileOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  observe({
    req(input$independenceDataFileFormat)
    if (input$independenceDataFileFormat == "Rows of subjects") {
      shinyjs::enable("independenceDataFileRow")
      shinyjs::enable("independenceDataFileColumn")
    } else {
      shinyjs::disable("independenceDataFileRow")
      shinyjs::disable("independenceDataFileColumn")
    }
  })
  
  observeEvent(input$independenceDataFileOk, {
    if (input$independenceDataFileFormat == "Rows of subjects") {
      if (!string.is.positive.integer(input$independenceDataFileRow)) {
        showNotification("Oops! Invalid row number.", type = "message")
        return()
      }
      if (!string.is.positive.integer(input$independenceDataFileColumn)) {
        showNotification("Oops! Invalid column number.", type = "message")
        return()
      }
      row <- as.numeric(input$independenceDataFileRow)
      col <- as.numeric(input$independenceDataFileColumn)
      if (row == col) {
        showNotification("Oops! Use different row and column numbers.",
                         type = "message")
        return()
      }
      df <- read.csv(input$independenceDataFile$datapath, header = FALSE,
                     strip.white = TRUE, stringsAsFactors = FALSE)
      if (max(row, col) > ncol(df)) {
        showNotification(paste("Oops! Only", ncol(df), "columns in the file."),
                         type = "message")
        return()
      }
      rv$independence.data <- table.data(table(df[,c(row, col)]))
      removeModal()
    } else {
      df <- read.csv(input$independenceDataFile$datapath, header = FALSE,
                     strip.white = TRUE, stringsAsFactors = FALSE)
      rv$independence.data <- matrix.data(data.matrix(df))
      removeModal()
    }
    updateTabsetPanel(session, "independenceTab", selected = "Data")
  })
  
  observeEvent(input$independenceDataCollapse, {
    updateTabsetPanel(session, "independenceTab", selected = "Data")
    rows <- parse.group.string(input$independenceDataCollapseRow)
    if (length(rows) == 1 && is.na(rows) ||
        length(rows) > 0 && max(unlist(rows)) > nrow(hot_to_r(input$independenceData))) {
      showModal(modalDialog(title = "Oops!", "Invalid row groups.",
                            easyClose = TRUE))
      return()
    }
    cols <- parse.group.string(input$independenceDataCollapseColumn)
    if (length(cols) == 1 && is.na(cols) ||
        length(cols) > 0 && max(unlist(cols)) > ncol(hot_to_r(input$independenceData))) {
      showModal(modalDialog(title = "Oops!", "Invalid column groups.",
                            easyClose = TRUE))
      return()
    }
    if (any(is.na(suppressWarnings(data.numeric.matrix(hot_to_r(input$independenceData)))))) {
      showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                            easyClose = TRUE))
      return()
    }
    showModal(modalDialog(title = "Confirmation", "Collapse the data?",
                          footer = tagList(actionButton("independenceDataCollapseOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  observeEvent(input$independenceDataCollapseOk, {
    rows <- parse.group.string(input$independenceDataCollapseRow)
    cols <- parse.group.string(input$independenceDataCollapseColumn)
    rv$independence.data <- as.data.frame(collapse.data(data.numeric.matrix(hot_to_r(input$independenceData)),
                                                        rows, cols))
    colnames(rv$independence.data) <- 1:ncol(rv$independence.data)
    removeModal()
  })
  
  output$independenceDataDownload <- downloadHandler(
    filename = "independence_data.csv",
    content = function(file) {
      write.table(hot_to_r(input$independenceData), file, quote = FALSE,
                  sep = ",", row.names = FALSE, col.names = FALSE,
                  qmethod = "double")
    }
  )
  
  observeEvent(input$independenceDataClear, {
    showModal(modalDialog(title = "Confirmation", "Clear the data?",
                          footer = tagList(actionButton("independenceDataClearOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  observeEvent(input$independenceDataClearOk, {
    updateTabsetPanel(session, "independenceTab", selected = "Data")
    rv$independence.data <- NULL
    removeModal()
  })
  
  observe({
    if (is.null(rv$independence.data)) {
      updateTextInput(session, "independenceDataDimension", value = "")
      updateActionButton(session, "independenceDataCreate", label = "Create")
      updateTextInput(session, "independenceDataCollapseRow", value = "")
      updateTextInput(session, "independenceDataCollapseColumn", value = "")
      shinyjs::disable("independenceDataCollapse")
      shinyjs::disable("independenceDataDownload")
      shinyjs::disable("independenceDataClear")
    } else {
      updateTextInput(session, "independenceDataDimension",
                      value = paste(dim(rv$independence.data), collapse = ","))
      updateActionButton(session, "independenceDataCreate", label = "Modify")
      shinyjs::enable("independenceDataCollapse")
      shinyjs::enable("independenceDataDownload")
      shinyjs::enable("independenceDataClear")
    }
  })
  
  independence.show.result <- function(s) {
    x <- independence.tab.counter <<- independence.tab.counter + 1
    text.id <- paste0("independenceResult", x)
    
    appendTab("independenceTab",
              tabPanel(paste("Result", x),
                       tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                       br(), textOutput(text.id), br(),
                       actionButton(paste0("independenceResultClose", x),
                                    label = "Close"),
                       br(), br()),
              select = TRUE)
    
    output[[text.id]] <- renderText({s})
    
    code <- gsub('%x', x,
                 'observeEvent(input$independenceResultClose%x, {
        removeTab("independenceTab", "Result %x")
      })', fixed=TRUE)
    eval(parse(text=code))
  }
  
  observeEvent(input$independenceGo, {
    # Data is clean?
    if (is.null(rv$independence.data)) {
      showModal(modalDialog(title = "Oops!", "There is no data.",
                            easyClose = TRUE))
      return()
    }
    
    m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$independenceData)))
    if (any(is.na(m))) {
      showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                            easyClose = TRUE))
      return()
    }
    if (any(m < 0)) {
      showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                            easyClose = TRUE))
      return()
    }
    if (input$independenceModelName == "Independence Model") {
      independence.show.result(independence.independence.report(
        independence.independence.computation(m)))
    } else if (input$independenceModelName == "ANOAS Model") {
      group <- parse.group.string(input$independenceAnoasGroup)
      if (length(group) == 0 || length(group) == 1 && is.na(group)) {
        showModal(modalDialog(title = "Oops!", "Invalid ANOAS groups.",
                              easyClose = TRUE))
        return()
      }
      largest <- max(unlist(group))
      is.row <- input$independenceAnoasOrientation == "Row"
      if (is.row && largest > nrow(m)) {
        showModal(modalDialog(title = "Oops!", "Row group exceeds the number of rows in data.",
                              easyClose = TRUE))
        return()
      }
      if (!is.row && largest > ncol(m)) {
        showModal(modalDialog(title = "Oops!", "Column group exceeds the number of columns in data.",
                              easyClose = TRUE))
        return()
      }
      independence.show.result(independence.anoas.report(
        independence.anoas.computation(m, group, is.row)))
    } else if (input$independenceModelName == "Predictors' Proportion") {
      independence.show.result(independence.predictors.proportion.report(m))
    } else if (input$independenceModelName == "Log-Odds Z-Test") {
      by.row <- input$independenceLogOddOrientation == "Row"
      if (!string.is.dimension(input$independenceLogOddWhichTwo, 2)) {
        if (by.row) {
          error.msg <- "Invalid input for two rows."
        } else {
          error.msg <- "Invalid input for two columns."
        }
        showModal(modalDialog(title = "Oops!", error.msg, easyClose = TRUE))
        return()
      }
      which.two <- parse.dimension.string(input$independenceLogOddWhichTwo)
      if (by.row && max(which.two) > nrow(m)) {
        showModal(modalDialog(title = "Oops!",
                              "Row input exceeds the number of rows in the data.", easyClose = TRUE))
        return()
      }
      if (!by.row && max(which.two) > ncol(m)) {
        showModal(modalDialog(title = "Oops!",
                              "Column input exceeds the number of columns in the data.", easyClose = TRUE))
        return()
      }
      if (which.two[1] == which.two[2]) {
        if (by.row) {
          error.msg <- "Use two different rows."
        } else {
          error.msg <- "Use two different columns."
        }
        showModal(modalDialog(title = "Oops!", error.msg, easyClose = TRUE))
        return()
      }
      independence.show.result(independence.ztest.report(
        independence.ztest.computation(m, which.two, by.row)))
    } else if (input$independenceModelName == "Collapse Chi-Sq Test") {
      rows <- parse.group.string(input$independenceCollapseChiSqRowGroup)
      if (length(rows) == 1 && is.na(rows)) {
        showModal(modalDialog(title = "Oops!", "Invalid row groups.",
                              easyClose = TRUE))
        return()
      }
      cols <- parse.group.string(input$independenceCollapseChiSqColumnGroup)
      if (length(cols) == 1 && is.na(cols)) {
        showModal(modalDialog(title = "Oops!", "Invalid column groups.",
                              easyClose = TRUE))
        return()
      }
      if (length(rows) >= 1 && max(unlist(rows)) > nrow(m)) {
        showModal(modalDialog(title = "Oops!", "Row group exceeds the number of rows in data.",
                              easyClose = TRUE))
        return()
      }
      if (length(cols) >= 1 && max(unlist(cols)) > ncol(m)) {
        showModal(modalDialog(title = "Oops!", "Column group exceeds the number of columns in data.",
                              easyClose = TRUE))
        return()
      }
      independence.show.result(independence.chisq.test.report(
        independence.chisq.test.computation(m, rows, cols)))
    }
  })
  
  # 3-Dimensional variable
  rv$three.dimensional.data <- NULL
  three.dimensional.tab.counter <- 0
  three.dimensional.computed <- list()
  
  observe({
    if(!input$threeDimensionalSelectCoefX) {
      updateCheckboxInput(session, "threeDimensionalSelectCoefXY", value = FALSE)
      updateCheckboxInput(session, "threeDimensionalSelectCoefXZ", value = FALSE)
    }
  })
  
  observe({
    if(!input$threeDimensionalSelectCoefY) {
      updateCheckboxInput(session, "threeDimensionalSelectCoefXY", value = FALSE)
      updateCheckboxInput(session, "threeDimensionalSelectCoefYZ", value = FALSE)
    }
  })
  
  observe({
    if(!input$threeDimensionalSelectCoefZ) {
      updateCheckboxInput(session, "threeDimensionalSelectCoefXZ", value = FALSE)
      updateCheckboxInput(session, "threeDimensionalSelectCoefYZ", value = FALSE)
    }
  })
  
  observe({
    if(input$threeDimensionalSelectCoefXY) {
      updateCheckboxInput(session, "threeDimensionalSelectCoefX", value = TRUE)
      updateCheckboxInput(session, "threeDimensionalSelectCoefY", value = TRUE)
    }
  })
  
  observe({
    if(input$threeDimensionalSelectCoefXZ) {
      updateCheckboxInput(session, "threeDimensionalSelectCoefX", value = TRUE)
      updateCheckboxInput(session, "threeDimensionalSelectCoefZ", value = TRUE)
    }
  })
  
  observe({
    if(input$threeDimensionalSelectCoefYZ) {
      updateCheckboxInput(session, "threeDimensionalSelectCoefY", value = TRUE)
      updateCheckboxInput(session, "threeDimensionalSelectCoefZ", value = TRUE)
    }
  })
  
  output$threeDimensionalData <- renderRHandsontable({
    req(rv$three.dimensional.data)
    rhandsontable(rv$three.dimensional.data, useTypes = FALSE) %>%
      hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
      hot_cols(colWidths = 80) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  observeEvent(input$threeDimensionalDataCreate, {
    updateTabsetPanel(session, "threeDimensionalTab", selected = "Data")
    if (!string.is.dimension(input$threeDimensionalDataDimension, 3)) {
      showModal(modalDialog(title = "Oops!", "Invalid data dimensions.",
                            easyClose = TRUE))
      return()
    }
    d <- parse.dimension.string(input$threeDimensionalDataDimension)
    if (is.null(rv$three.dimensional.data)) {
      rv$three.dimensional.data <- array.data(array("", d))
    } else if (!all(parse.dimension.string(tail(rownames(rv$three.dimensional.data), 1)) == d)) {
      showModal(modalDialog(title = "Confirmation", "Modify the data dimensions?",
                            footer = tagList(actionButton("threeDimensionalDataModifyOk", "OK"),
                                             actionButton("threeDimensionalDataModifyCancel", "Cancel"))))
    }
  })
  
  observeEvent(input$threeDimensionalDataModifyOk, {
    d <- parse.dimension.string(input$threeDimensionalDataDimension)
    rv$three.dimensional.data <- array.data(array("", d))
    removeModal()
  })
  
  observeEvent(input$threeDimensionalDataModifyCancel, {
    updateTextInput(session, "threeDimensionalDataDimension",
                    value = tail(rownames(rv$three.dimensional.data), 1))
    removeModal()
  })
  
  observeEvent(input$threeDimensionalDataFile, {
    showModal(modalDialog(title = "File Information",
                          textInput("threeDimensionalDataFileX", label = "Which column in file for variable X?"),
                          textInput("threeDimensionalDataFileY", label = "Which column in file for variable Y?"),
                          textInput("threeDimensionalDataFileZ", label = "Which column in file for variable Z?"),
                          footer = tagList(actionButton("threeDimensionalDataFileOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  observeEvent(input$threeDimensionalDataFileOk, {
    if (!string.is.positive.integer(input$threeDimensionalDataFileX)) {
      showNotification("Oops! Invalid column number for variable X.", type = "message")
      return()
    }
    if (!string.is.positive.integer(input$threeDimensionalDataFileY)) {
      showNotification("Oops! Invalid column number for variable Y.", type = "message")
      return()
    }
    if (!string.is.positive.integer(input$threeDimensionalDataFileZ)) {
      showNotification("Oops! Invalid column number for variable Z.", type = "message")
      return()
    }
    x <- as.numeric(input$threeDimensionalDataFileX)
    y <- as.numeric(input$threeDimensionalDataFileY)
    z <- as.numeric(input$threeDimensionalDataFileZ)
    if (any(duplicated(c(x, y, z)))) {
      showNotification("Oops! Use distinct column numbers.",
                       type = "message")
      return()
    }
    df <- read.csv(input$threeDimensionalDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (max(c(x, y, z)) > ncol(df)) {
      showNotification(paste("Oops! Only", ncol(df), "columns in the file."),
                       type = "message")
      return()
    }
    rv$three.dimensional.data <- array.data(table(df[,c(x, y, z)]))
    removeModal()
    updateTabsetPanel(session, "threeDimensionalTab", selected = "Data")
  })
  
  output$threeDimensionalDataDownload <- downloadHandler(
    filename = function() {
      which.two <- tolower(strsplit(input$threeDimensionalWhichTwoWay, " ")[[1]])
      return(paste("three_dimensional_data_", paste(which.two, collapse = ""),
                   ".csv", sep = ""))
    },
    content = function(file) {
      a <- suppressWarnings(data.numeric.matrix(hot_to_r(input$threeDimensionalData)))
      if (any(is.na(a))) {
        a[is.na(a)] <- 0
        showNotification("Warning! Non-numeric data is set to 0.", type = "message")
      }
      d <- parse.dimension.string(tail(rownames(a), 1))
      dim(a) <- d
      # Aggregate two-way data
      which.two <- strsplit(input$threeDimensionalWhichTwoWay, " ")[[1]]
      idx <- unlist(list(X = 1, Y = 2, Z = 3)[which.two])
      m <- apply(a, sort(idx), sum)
      if (idx[1] > idx[2]) {
        m <- t(m)
      }
      write.table(m, file, quote = FALSE, sep = ",", row.names = FALSE,
                  col.names = FALSE, qmethod = "double")
    }
  )
  
  observeEvent(input$threeDimensionalDataClear, {
    showModal(modalDialog(title = "Confirmation", "Clear the data?",
                          footer = tagList(actionButton("threeDimensionalDataClearOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  observeEvent(input$threeDimensionalDataClearOk, {
    updateTabsetPanel(session, "threeDimensionalTab", selected = "Data")
    rv$three.dimensional.data <- NULL
    removeModal()
  })
  
  observe({
    if (is.null(rv$three.dimensional.data)) {
      updateTextInput(session, "threeDimensionalDataDimension", value = "")
      updateActionButton(session, "threeDimensionalDataCreate", label = "Create")
      shinyjs::disable("threeDimensionalDataDownload")
      shinyjs::disable("threeDimensionalDataClear")
      updateSelectInput(session, "threeDimensionalWhichTarget", choices = "None")
    } else {
      s <- tail(rownames(rv$three.dimensional.data), 1)
      updateTextInput(session, "threeDimensionalDataDimension", value = s)
      updateActionButton(session, "threeDimensionalDataCreate", label = "Modify")
      shinyjs::enable("threeDimensionalDataDownload")
      shinyjs::enable("threeDimensionalDataClear")
      d <- parse.dimension.string(s)
      if (any(d == 2)) {
        updateSelectInput(session, "threeDimensionalWhichTarget",
                          choices = c("X", "Y", "Z")[d == 2])
      } else {
        updateSelectInput(session, "threeDimensionalWhichTarget", choices = "None")
      }
    }
  })
  
  three.dimensional.show.result <- function(s) {
    x <- three.dimensional.tab.counter <<- three.dimensional.tab.counter + 1
    text.id <- paste0("threeDimensionalResult", x)
    
    appendTab("threeDimensionalTab",
              tabPanel(paste("Result", x),
                       tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                       br(), textOutput(text.id), br(),
                       actionButton(paste0("threeDimensionalResultClose", x),
                                    label = "Close"),
                       br(), br()),
              select = TRUE)
    
    output[[text.id]] <- renderText({s})
    
    code <- gsub('%x', x,
                 'observeEvent(input$threeDimensionalResultClose%x, {
        removeTab("threeDimensionalTab", "Result %x")
      })', fixed=TRUE)
    eval(parse(text=code))
  }
  
  three.dimensional.show.model.value <- function(computed) {
    x <- three.dimensional.tab.counter
    three.dimensional.computed[[x]] <- computed
    
    appendTab("threeDimensionalTab",
              tabPanel(
                paste("Model Value", x),
                br(),
                fluidRow(
                  column(width = 5,
                         wellPanel(
                           helpText('Model Value Plot'),
                           tags$b("Reward (O = observed, P = predicted)"),
                           fluidRow(
                             column(width = 4, offset = 3, align = "center", helpText('P = 1')),
                             column(width = 4, align = "center", helpText('P = 2'))
                           ),
                           fluidRow(
                             column(width = 3, align = "right", helpText('O = 1')),
                             column(width = 4,
                                    textInput(paste0("threeDimensionalRewardO1P1", x),
                                              label = NULL, value = "1")),
                             column(width = 4,
                                    textInput(paste0("threeDimensionalRewardO1P2", x),
                                              label = NULL, value = "-1"))
                           ),
                           fluidRow(
                             column(width = 3, align = "right", helpText('O = 2')),
                             column(width = 4,
                                    textInput(paste0("threeDimensionalRewardO2P1", x),
                                              label = NULL, value = "-1")),
                             column(width = 4,
                                    textInput(paste0("threeDimensionalRewardO2P2", x),
                                              label = NULL, value = "1"))
                           ),
                           actionButton(paste0("threeDimensionalModelValuePlotButton", x),
                                        label = "Plot!"),
                           hr(style="border-color: lightgray;"),
                           helpText("Sensitivity and Specificity"),
                           textInput(paste0("threeDimensionalPropensityThreshold", x),
                                     label = "Propensity Threshold"),
                           actionButton(paste0("threeDimensionalModelValueCompute", x),
                                        label = "Compute!")
                         ),
                         actionButton(paste0("threeDimensionalModelValueClose", x),
                                      label = "Close")
                  ),
                  column(width = 7,
                         plotOutput(paste0("threeDimensionalModelValuePlot", x)),
                         plotOutput(paste0("threeDimensionalModelAccuracyPlot", x)),
                         br(),
                         verbatimTextOutput(paste0("threeDimensionalModelValueResult", x)))
                ),
                br()
              )
    )
    
    code <- gsub('%x', x,
                 'observeEvent(input$threeDimensionalModelValuePlotButton%x, {
        # Reward is clean?
        tp.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO1P1%x))
        fn.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO1P2%x))
        fp.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO2P1%x))
        tn.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO2P2%x))
        if (is.na(tp.reward) || is.na(tn.reward) || is.na(fp.reward) ||
            is.na(fn.reward)) {
          showModal(modalDialog(title = "Oops!", "Reward values should be numeric.",
                                easyClose = TRUE))
          return()
        }
        # Compute model value at each threshold
        propensity.matrix <- three.dimensional.computed[[%x]]$propensity.matrix
        threshold <- sort(unique(as.vector(propensity.matrix)))
        model.value <- numeric(length(threshold))
        for (i in seq_along(threshold)) {
        
          model.value[i] <- three.dimensional.model.value.computation(
            three.dimensional.computed[[%x]]$data,
            three.dimensional.computed[[%x]]$explanatory.variables,
            propensity.matrix, threshold[i], tp.reward, tn.reward,
            fp.reward, fn.reward)$model.value
        }
        # 2014 Dec 5: Zvi decided to plot only the thresholds as dots.
        
        
        output$threeDimensionalModelValuePlot%x <- renderPlot(
          ggplot(data.frame(threshold = threshold, model.value = model.value),
            aes(x = threshold, y = model.value)) +
            xlab("Propensity Threshold") +
            ylab("Model Value") + 
            ggtitle("Model Values", subtitle = paste0("Maximal value of: ", max(model.value), " is achieved by threshold: ",threshold[which.max(model.value)])) +
            geom_point(colour = "#0072B2", size = 3) +
            theme_grey(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5)))
            
        # compute accuracy for each threshold
        model.accuracy <- numeric(length(threshold))
        for (i in seq_along(threshold)) {
          model <- three.dimensional.model.value.computation(
            three.dimensional.computed[[%x]]$data,
            three.dimensional.computed[[%x]]$explanatory.variables,
            propensity.matrix, threshold[i], tp.reward, tn.reward,
            fp.reward, fn.reward)
            model.accuracy[i] <- (model$sensitivity + model$specificity)/2
        }
        
        output$threeDimensionalModelAccuracyPlot%x <- renderPlot(
          ggplot(data.frame(threshold = threshold, model.accuracy = model.accuracy),
            aes(x = threshold, y = model.accuracy)) +
            xlab("Propensity Threshold") +
            ylab("Model Balanced Accuracy") + 
            ggtitle("Model Balanced Accuracies", subtitle = paste0("Maximal accuracy of: ", round(max(model.accuracy),4), " is achieved by threshold: ",round(threshold[which.max(model.accuracy)],4))) +
            geom_point(colour = "#0072B2", size = 3) +
            theme_grey(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5)))
        
      })', fixed=TRUE)
    eval(parse(text=code))
    
    code <- gsub('%x', x,
                 'observeEvent(input$threeDimensionalModelValueCompute%x, {
        # Reward is clean?
        tp.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO1P1%x))
        fn.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO1P2%x))
        fp.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO2P1%x))
        tn.reward <- suppressWarnings(as.numeric(input$threeDimensionalRewardO2P2%x))
        if (is.na(tp.reward) || is.na(tn.reward) || is.na(fp.reward) ||
            is.na(fn.reward)) {
          showModal(modalDialog(title = "Oops!", "Reward values should be numeric.",
                                easyClose = TRUE))
          return()
        }
        # Propensity threshold is clean?
        threshold <- suppressWarnings(as.numeric(input$threeDimensionalPropensityThreshold%x))
        if (is.na(threshold) || threshold < 0 || threshold > 1) {
          showModal(modalDialog(title = "Oops!", "Propensity threshold should be between 0 and 1.",
                                easyClose = TRUE))
          return()
        }
        # Compute model value at threshold
        computed <- three.dimensional.model.value.computation(
          three.dimensional.computed[[%x]]$data,
          three.dimensional.computed[[%x]]$explanatory.variables,
          three.dimensional.computed[[%x]]$propensity.matrix,
          threshold, tp.reward, tn.reward, fp.reward, fn.reward)
        s <- ""
        s <- paste0(s, "Model Value Result\n")
        s <- paste0(s, "==================\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Reward (O = observed, P = predicted)\n")
        s <- paste0(s, "--------------------------------------\n")
        m <- matrix("", 3, 3)
        m[1,2] <- "P = 1"
        m[1,3] <- "P = 2"
        m[2,1] <- "O = 1"
        m[2,2] <- tp.reward
        m[2,3] <- fn.reward
        m[3,1] <- "O = 2"
        m[3,2] <- fp.reward
        m[3,3] <- tn.reward
        s <- paste0(s, matrix.string(m, left.justified = FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Propensity threshold = ", threshold, "\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Sensitivity = ", signif(computed$sensitivity, 4), "\n")
        s <- paste0(s, "Specificity = ", signif(computed$specificity, 4), "\n")
        s <- paste0(s, "Accuracy = ", signif((computed$num.tp+computed$num.tn)/(computed$num.tp+computed$num.tn+computed$num.fp+computed$num.fn), 4), "\n" )
        s <- paste0(s, "Balanced Accuracy = ", signif((computed$sensitivity+computed$specificity)/2, 4) )
        s <- paste0(s, "\n")
        s <- paste0(s, "C-Matrix (O = observed, P = predicted)\n")
        s <- paste0(s, "--------------------------------------\n")
        m <- matrix("", 3, 3)
        m[1,2] <- "P = 1"
        m[1,3] <- "P = 2"
        m[2,1] <- "O = 1"
        m[2,2] <- computed$num.tp
        m[2,3] <- computed$num.fn
        m[3,1] <- "O = 2"
        m[3,2] <- computed$num.fp
        m[3,3] <- computed$num.tn
        s <- paste0(s, matrix.string(m, left.justified = FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Model value = ", round(computed$model.value, 2))
        output$threeDimensionalModelValueResult%x <- renderText({s})
      })', fixed=TRUE)
    eval(parse(text=code))
    
    code <- gsub('%x', x,
                 'observeEvent(input$threeDimensionalModelValueClose%x, {
        removeTab("threeDimensionalTab", "Model Value %x")
      })', fixed=TRUE)
    eval(parse(text=code))
  }
  
  observeEvent(input$threeDimensionalGo, {
    # Data is clean?
    if (is.null(rv$three.dimensional.data)) {
      showModal(modalDialog(title = "Oops!", "There is no data.",
                            easyClose = TRUE))
      return()
    }
    a <- suppressWarnings(data.numeric.matrix(hot_to_r(input$threeDimensionalData)))
    if (any(is.na(a))) {
      showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                            easyClose = TRUE))
      return()
    }
    if (any(a < 0)) {
      showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                            easyClose = TRUE))
      return()
    }
    dim(a) <- parse.dimension.string(tail(rownames(a), 1))
    if (input$threeDimensionalModelChoice == "All coefficients") {
      three.dimensional.show.result(three.dimensional.all.coef.report(
        three.dimensional.all.coef.computation(a)))
    } else if (input$threeDimensionalModelChoice == "Select coefficients") {
      which.x <- list()
      if (input$threeDimensionalSelectCoefX) {
        which.x <- c(which.x, list(c(1)))
      }
      if (input$threeDimensionalSelectCoefY) {
        which.x <- c(which.x, list(c(2)))
      }
      if (input$threeDimensionalSelectCoefZ) {
        which.x <- c(which.x, list(c(3)))
      }
      if (input$threeDimensionalSelectCoefXY) {
        which.x <- c(which.x, list(c(1,2)))
      }
      if (input$threeDimensionalSelectCoefXZ) {
        which.x <- c(which.x, list(c(1,3)))
      }
      if (input$threeDimensionalSelectCoefYZ) {
        which.x <- c(which.x, list(c(2,3)))
      }
      three.dimensional.show.result(three.dimensional.select.coef.report(
        three.dimensional.select.coef.computation(a, which.x)))
    } else if (input$threeDimensionalModelChoice == "Target variable") {
      if (input$threeDimensionalWhichTarget == "None") {
        showModal(modalDialog(title = "Oops!", "Data does not have any binary target.",
                              easyClose = TRUE))
        return()
      }
      target <- which(c("X", "Y", "Z") == input$threeDimensionalWhichTarget)
      computed <- three.dimensional.target.computation(a, target)
      three.dimensional.show.result(three.dimensional.target.report(computed))
      three.dimensional.show.model.value(computed)
    }
  })
  
  # N-dimensional variable
  rv$n.dimensional.data <- NULL
  n.dimensional.tab.counter <- 0
  n.dimensional.tab.computed <- list()
  
  output$nDimensionalData <- renderRHandsontable({
    req(rv$n.dimensional.data)
    rhandsontable(rv$n.dimensional.data, useTypes = FALSE) %>%
      hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
      hot_cols(colWidths = 80) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  observeEvent(input$nDimensionalDataCreate, {
    updateTabsetPanel(session, "nDimensionalTab", selected = "Data")
    if (!string.is.dimension(input$nDimensionalDataDimension, 2)) {
      showModal(modalDialog(title = "Oops!", "Invalid data dimensions.",
                            easyClose = TRUE))
      return()
    }
    d <- parse.dimension.string(input$nDimensionalDataDimension)
    if (is.null(rv$n.dimensional.data)) {
      rv$n.dimensional.data <- empty.data(d[1], d[2], col.name = paste0("X", 1:d[2]))
    } else if (!all(dim(rv$n.dimensional.data) == d)) {
      showModal(modalDialog(title = "Confirmation", "Modify the data dimensions?",
                            footer = tagList(actionButton("nDimensionalDataModifyOk", "OK"),
                                             actionButton("nDimensionalDataModifyCancel", "Cancel"))))
    }
  })
  
  observeEvent(input$nDimensionalDataModifyOk, {
    d <- parse.dimension.string(input$nDimensionalDataDimension)
    row <- min(nrow(rv$n.dimensional.data), d[1])
    col <- min(ncol(rv$n.dimensional.data), d[2])
    df <- empty.data(d[1], d[2], col.name = paste0("X", 1:d[2]))
    df[1:row,1:col] <- hot_to_r(input$nDimensionalData)[1:row,1:col]
    rv$n.dimensional.data <- df
    removeModal()
  })
  
  observeEvent(input$nDimensionalDataModifyCancel, {
    updateTextInput(session, "nDimensionalDataDimension",
                    value = paste(dim(rv$n.dimensional.data), collapse = ","))
    removeModal()
  })
  
  observeEvent(input$nDimensionalDataFile, {
    df <- read.csv(input$nDimensionalDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    rv$n.dimensional.data <- matrix.data(data.matrix(df),
                                         col.name = paste0("X", 1:ncol(df)))
    updateTabsetPanel(session, "nDimensionalTab", selected = "Data")
  })
  
  output$nDimensionalDataDownload <- downloadHandler(
    filename = "n_dimensional_data.csv",
    content = function(file) {
      write.table(hot_to_r(input$nDimensionalData), file, quote = FALSE,
                  sep = ",", row.names = FALSE, col.names = FALSE,
                  qmethod = "double")
    }
  )
  
  observeEvent(input$nDimensionalDataClear, {
    showModal(modalDialog(title = "Confirmation", "Clear the data?",
                          footer = tagList(actionButton("nDimensionalDataClearOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  observeEvent(input$nDimensionalDataClearOk, {
    updateTabsetPanel(session, "nDimensionalTab", selected = "Data")
    rv$n.dimensional.data <- NULL
    removeModal()
  })
  
  observe({
    if (is.null(rv$n.dimensional.data)) {
      updateTextInput(session, "nDimensionalDataDimension", value = "")
      updateActionButton(session, "nDimensionalDataCreate", label = "Create")
      shinyjs::disable("nDimensionalDataDownload")
      shinyjs::disable("nDimensionalDataClear")
    } else {
      s <- apply(rv$n.dimensional.data, 2, max)
      updateTextInput(session, "nDimensionalDataDimension",
                      value = paste(dim(rv$n.dimensional.data), collapse = ","))
      updateActionButton(session, "nDimensionalDataCreate", label = "Modify")
      shinyjs::enable("nDimensionalDataDownload")
      shinyjs::enable("nDimensionalDataClear")
    }
  })
  
  observeEvent(input$nDimensionalGo, {
    # Data is clean?
    if (is.null(rv$n.dimensional.data)) {
      showModal(modalDialog(title = "Oops!", "There is no data.",
                            easyClose = TRUE))
      return()
    }
    m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$nDimensionalData)))
    if (any(is.na(m))) {
      showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                            easyClose = TRUE))
      return()
    }
    if (ncol(m) == 1) {
      showModal(modalDialog(title = "Oops!", "Data should have at least 2 columns.",
                            easyClose = TRUE))
      return()
    }
    possible.target <- which(apply(m, 2, function(x) length(unique(x))) == 2)
    if (!any(possible.target)) {
      showModal(modalDialog(title = "Oops!", "At least one column should be binary.",
                            easyClose = TRUE))
      return()
    }

    # Specify predictor and target variables
    showModal(modalDialog(title = "Specify predictor and target variables",
                          textInput("nDimensionalPredictor", label = "Predictor variable(s)"),
                          selectInput("nDimensionalTarget", label = "Target variable",
                                      choices = paste0("X", possible.target)),
                          textInput("nDimensionalClientConstraints",
                                      label = "Client constraints"),
                          footer = tagList(actionButton("nDimensionalGoOk", "OK"),
                                           modalButton("Cancel"))))
  })
  
  n.dimensional.show.result <- function(s) {
    x <- n.dimensional.tab.counter <<- n.dimensional.tab.counter + 1
    text.id <- paste0("nDimensionalResult", x)
    
    appendTab("nDimensionalTab",
              tabPanel(paste("Result", x),
                       tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                       br(), textOutput(text.id), br(),
                       actionButton(paste0("nDimensionalResultClose", x),
                                    label = "Close"),
                       br(), br()),
              select = TRUE)
    
    output[[text.id]] <- renderText({s})
    
    code <- gsub('%x', x,
                 'observeEvent(input$nDimensionalResultClose%x, {
        removeTab("nDimensionalTab", "Result %x")
      })', fixed=TRUE)
    eval(parse(text=code))
  }
  
  n.dimensional.show.model.selection <- function(computed) {
    x <- n.dimensional.tab.counter <- n.dimensional.tab.counter 
    n.dimensional.tab.computed[[x]] <- computed

    
    # if(length(computed[["coef.text"]]) > 1){
    #   choices <- c("Model of highest complexity", rev(seq(length(computed[["coef.text"]])-1)))
    # } else {
    #   choices <- c("Model of highest complexity")
    # }
    # 
    
    appendTab("nDimensionalTab",
              tabPanel(
                paste("Detailed models", x),
                br(),
                fluidRow(
                  column(width = 5,
                         wellPanel(
                           helpText("Detailed models"),
                           selectInput(paste0("nDimensionalModeldetail", x),
                                       label = h3("Which model would you like to view in detail?"),
                                       choices = 1:length(computed$model)),
                           actionButton(paste0("nDimensionalModeldetailCompute", x),
                                        label = "Show!")
                         ),
                         actionButton(paste0("nDimensionalModeldetailClose", x),
                                      label = "Close")
                  ),
                  column(width = 7, verbatimTextOutput(paste0("nDimensionalModeldetailResult", x)))
                ),
                br()
              )
    )
    
    code <- gsub('%x', x,
                 'observeEvent(input$nDimensionalModeldetailCompute%x, {
                 
                  model <- computed$model[[as.numeric(input$nDimensionalModeldetail%x) ]]
                  # }
                s <- n.dimensional.model.select.report(model) 
                output$nDimensionalModeldetailResult%x <- renderText({s})
      })', fixed=TRUE)
    eval(parse(text=code))
    
    code <- gsub('%x', x,
                 'observeEvent(input$nDimensionalModeldetailClose%x, {
        removeTab("nDimensionalTab", "Detailed models %x")
      })', fixed=TRUE)
    eval(parse(text=code))
  }

n.dimensional.model.value <- function(computed) {
  
  x <- n.dimensional.tab.counter
  n.dimensional.tab.computed[[x]] <- computed 

  
  appendTab("nDimensionalTab",
            tabPanel(
              paste("Model Value", x),
              br(),
              fluidRow(
                column(width = 5,
                       wellPanel(
                         selectInput(
                           paste0("nDimensionalModelValueModel", x),
                           label = "Select model:",
                           choices = 1:length(computed$model)
                         ),
                         # selectInput(
                         #   paste0("nDimensionalModelValueVariables", x),
                         #   label = "Variables in model:",
                         #   choices = sort(computed$sorted.by.predictivens),
                         #   multiple = TRUE
                         # ),
                      
                         helpText('Model Value Plot'),
                         tags$b("Reward (O = observed, P = predicted)"),
                         fluidRow(
                           column(width = 4, offset = 3, align = "center", helpText('P = 1')),
                           column(width = 4, align = "center", helpText('P = 2'))
                         ),
                         fluidRow(
                           column(width = 3, align = "right", helpText('O = 1')),
                           column(width = 4,
                                  textInput(paste0("nDimensionalRewardO1P1", x),
                                            label = NULL, value = "1")),
                           column(width = 4,
                                  textInput(paste0("nDimensionalRewardO1P2", x),
                                            label = NULL, value = "-1"))
                         ),
                         fluidRow(
                           column(width = 3, align = "right", helpText('O = 2')),
                           column(width = 4,
                                  textInput(paste0("nDimensionalRewardO2P1", x),
                                            label = NULL, value = "-1")),
                           column(width = 4,
                                  textInput(paste0("nDimensionalRewardO2P2", x),
                                            label = NULL, value = "1"))
                         ),
                         actionButton(paste0("nDimensionalModelValuePlotButton", x),
                                      label = "Plot!"),
                         hr(style="border-color: lightgray;"),
                         helpText("Sensitivity and Specificity"),
                         textInput(paste0("nDimensionalPropensityThreshold", x),
                                   label = "Propensity Threshold"),
                         actionButton(paste0("nDimensionalModelValueCompute", x),
                                      label = "Compute!")
                       ),
                       actionButton(paste0("nDimensionalModelValueClose", x),
                                    label = "Close")
                ),
                column(width = 7,
                       plotOutput(paste0("nDimensionalModelValuePlot", x)),
                       plotOutput(paste0("nDimensionalModelAccuracyPlot", x)),
                       br(),
                       verbatimTextOutput(paste0("nDimensionalModelValueResult", x)))
              ),
              br()
            )
  )

  code <- gsub('%x', x,
               'observeEvent(input$nDimensionalModelValuePlotButton%x, {
               
        # Reward is clean?

        model.num <- suppressWarnings(as.numeric(input$nDimensionalModelValueModel%x))
        tp.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO1P1%x))
        fn.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO1P2%x))
        fp.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO2P1%x))
        tn.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO2P2%x))
        if (is.na(tp.reward) || is.na(tn.reward) || is.na(fp.reward) ||
            is.na(fn.reward)) {
          showModal(modalDialog(title = "Oops!", "Reward values should be numeric.",
                                easyClose = TRUE))
          return()
        }

        model <- computed$model[[model.num]]
        propensity.matrix <- n.dimensional.propensity.computation(model)
        threshold <- sort(unique(as.vector(propensity.matrix[,ncol(propensity.matrix)])))
        model.value <- numeric(length(threshold))
        
        for (i in seq_along(threshold)) {
          model.value[i] <- n.dimensional.model.value.computation(
            model$data,
            propensity.matrix, threshold[i], tp.reward, tn.reward,
            fp.reward, fn.reward)$model.value
        }
        
        # 2014 Dec 5: Zvi decided to plot only the thresholds as dots.
        

        output$nDimensionalModelValuePlot%x <- renderPlot(
          ggplot(data.frame(threshold = threshold, model.value = model.value),
            aes(x = threshold, y = model.value)) +
            xlab("Propensity Threshold") +
            ylab("Model Value") + 
            ggtitle("Model Values", subtitle = paste0("Maximal value of: ", max(model.value), " is achieved by threshold: ",round(threshold[which.max(model.value)],2))) +
            geom_point(colour = "#0072B2", size = 3) +
            theme_grey(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5)))
            
        # compute accuracy for each threshold
        model.accuracy <- numeric(length(threshold))
        for (i in seq_along(threshold)) {
          model1 <- n.dimensional.model.value.computation(
            model$data,
            propensity.matrix, threshold[i], tp.reward, tn.reward,
            fp.reward, fn.reward)
            model.accuracy[i] <- (model1$sensitivity + model1$specificity)/2
        }
        
        output$nDimensionalModelAccuracyPlot%x <- renderPlot(
          ggplot(data.frame(threshold = threshold, model.accuracy = model.accuracy),
            aes(x = threshold, y = model.accuracy)) +
            xlab("Propensity Threshold") +
            ylab("Model Balanced Accuracy") + 
            ggtitle("Model Balanced Accuracies", subtitle = paste0("Maximal accuracy of: ", round(max(model.accuracy),2), " is achieved by threshold: ",round(threshold[which.max(model.accuracy)],2))) +
            geom_point(colour = "#0072B2", size = 3) +
            theme_grey(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5)))
            
            
      })', fixed=TRUE)
  eval(parse(text=code))
  
  code <- gsub('%x', x,
               'observeEvent(input$nDimensionalModelValueCompute%x, {
        # Reward is clean?
        # model.type <- suppressWarnings(input$nDimensionalModelValueType%x)
        # model.variables <-suppressWarnings(as.numeric(input$nDimensionalModelValueVariables%x))
        model.num <- suppressWarnings(as.numeric(input$nDimensionalModelValueModel%x))
        tp.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO1P1%x))
        fn.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO1P2%x))
        fp.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO2P1%x))
        tn.reward <- suppressWarnings(as.numeric(input$nDimensionalRewardO2P2%x))
        if (is.na(tp.reward) || is.na(tn.reward) || is.na(fp.reward) ||
            is.na(fn.reward)) {
          showModal(modalDialog(title = "Oops!", "Reward values should be numeric.",
                                easyClose = TRUE))
          return()
        }
        # Propensity threshold is clean?
        threshold <- suppressWarnings(as.numeric(input$nDimensionalPropensityThreshold%x))
        if (is.na(threshold) || threshold < 0 || threshold > 1) {
          showModal(modalDialog(title = "Oops!", "Propensity threshold should be between 0 and 1.",
                                easyClose = TRUE))
          return()
        }

        model <- computed$model[[model.num]]
        propensity.matrix <- n.dimensional.propensity.computation(model)
        value <- n.dimensional.model.value.computation(
        model$data, propensity.matrix,
        threshold, tp.reward, tn.reward, fp.reward, fn.reward
        )

        s <- ""
        s <- paste0(s, "Model Value Result\n")
        s <- paste0(s, "==================\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Reward (O = observed, P = predicted)\n")
        s <- paste0(s, "--------------------------------------\n")
        m <- matrix("", 3, 3)
        m[1,2] <- "P = 1"
        m[1,3] <- "P = 2"
        m[2,1] <- "O = 1"
        m[2,2] <- tp.reward
        m[2,3] <- fn.reward
        m[3,1] <- "O = 2"
        m[3,2] <- fp.reward
        m[3,3] <- tn.reward
        s <- paste0(s, matrix.string(m, left.justified = FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Propensity threshold = ", threshold, "\n")
        s <- paste0(s, "\n","\n")
        s <- paste0(s, "Sensitivity = ", signif(value$sensitivity, 4), "\n")
        s <- paste0(s, "Specificity = ", signif(value$specificity, 4), "\n")
        s <- paste0(s, "Accuracy = ", signif((value$num.tp+value$num.tn)/(value$num.tp+value$num.tn+value$num.fp+value$num.fn), 4), "\n" )
        s <- paste0(s, "Balanced Accuracy = ", signif((value$sensitivity+value$specificity)/2, 4) )
        s <- paste0(s, "\n")
        s <- paste0(s, "C-Matrix (O = observed, P = predicted)\n")
        s <- paste0(s, "--------------------------------------\n")
        m <- matrix("", 3, 3)
        m[1,2] <- "P = 1"
        m[1,3] <- "P = 2"
        m[2,1] <- "O = 1"
        m[2,2] <- value$num.tp
        m[2,3] <- value$num.fn
        m[3,1] <- "O = 2"
        m[3,2] <- value$num.fp
        m[3,3] <- value$num.tn
        s <- paste0(s, matrix.string(m, left.justified = FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Model value = ", round(value$model.value, 2))
        
        output$nDimensionalModelValueResult%x <- renderText({s})
      })', fixed=TRUE)
  eval(parse(text=code))
  
  code <- gsub('%x', x,
               'observeEvent(input$nDimensionalModelValueClose%x, {
        removeTab("nDimensionalTab", "Model Value %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

observeEvent(input$nDimensionalGoOk, {
  predictor.variable <- parse.group.string(input$nDimensionalPredictor)
  if (length(predictor.variable) != 1 || is.na(predictor.variable)) {
    showNotification("Oops! Invalid predictor variable(s).", type = "message")
    return()
  }
  predictor.variable <- predictor.variable[[1]]
  m <- data.numeric.matrix(hot_to_r(input$nDimensionalData))
  if (max(predictor.variable) > ncol(m)) {
    showNotification("Oops! Some predictor variable exceeds the number of columns in the data.",
                     type = "message")
    return()
  }
  target.variable <- as.numeric(substring(input$nDimensionalTarget, 2))
  if (is.element(target.variable, predictor.variable)) {
    showNotification("Oops! The target variable should not be a predictor variable.",
                     type = "message")
    return()
  }
  client.constraints <- parse.group.string(input$nDimensionalClientConstraints)
  if (length(client.constraints) > 0){
  if (length(client.constraints) != 1 || is.na(client.constraints)) {
    showNotification("Oops! Invalid client constraints.", type = "message")
    return()
  }
  client.constraints = client.constraints[[1]]
  if (any(!(client.constraints %in% predictor.variable))) {
    showNotification("Oops! all client constraints should be predictor variables.",
                     type = "message")
    return()
  }
  }
  
  
  removeModal()
  if(input$nDimensionalModelChoice == "Independence") {

    n.dimensional.show.result(n.dimensional.indep.report(
      n.dimensional.indep.computation(m, predictor.variable, target.variable)))
  } else if(input$nDimensionalModelChoice == "Two-way interaction"){
    n.dimensional.show.result(n.dimensional.twoway.report(
      n.dimensional.twoway.computation(m, predictor.variable, target.variable)))
  } 
  else if(input$nDimensionalModelChoice == "Variable selection"){
    
    predictor.variable <- parse.group.string(input$nDimensionalPredictor)[[1]]
    target.variable <- as.numeric(substring(input$nDimensionalTarget, 2))
    client.constraints <- parse.group.string(input$nDimensionalClientConstraints)
    if (length(client.constraints)>0){
      client.constraints = client.constraints[[1]]
    }
    m <- data.numeric.matrix(hot_to_r(input$nDimensionalData))
    computed <- n.dimensional.target.variable(m, predictor.variable, target.variable, client.constraints)
    n.dimensional.show.result(n.dimensional.target.variable.report(computed)) # main screen
    n.dimensional.show.model.selection(computed)
    n.dimensional.model.value(computed)
    
    # showModal(modalDialog(title = "Choose level of complexity",
    #                       selectInput("nDimensionalLevel", label = "Number of levels:",
    #                                   choices = (1:length(predictor.variable))),
    #                       footer = tagList(actionButton("nDimensionalGoLevelOk", "OK"),
    #                                        modalButton("Cancel"))))
    
  }
})

observeEvent(input$nDimensionalGoLevelOk, {
  level <- as.numeric(input$nDimensionalLevel)
  predictor.variable <- parse.group.string(input$nDimensionalPredictor)[[1]]
  target.variable <- as.numeric(substring(input$nDimensionalTarget, 2))
  m <- data.numeric.matrix(hot_to_r(input$nDimensionalData))
  
  removeModal()
  
  computed <- n.dimensional.target.variable(m, predictor.variable, target.variable, level = level)
  #n.dimensional.model.select.report(computed) #view a model after it's selected
  n.dimensional.show.result(n.dimensional.target.variable.report(computed)) # main screen
  n.dimensional.show.model.selection(computed)
  n.dimensional.model.value(computed)
})


# Survival variable
rv$survival.data <- NULL
rv$survival.explanatory.data <- NULL
survival.tab.counter <- 0
survival.computed <- list()
survival.contingency.vector <- NULL
survival.training.portion <- NULL
survival.training.vector <- NULL
survival.test.vector <- NULL

output$survivalData <- renderRHandsontable({
  req(rv$survival.data)
  rhandsontable(rv$survival.data, useTypes = FALSE) %>%
    hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
    hot_cols(colWidths = 80) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$survivalDataCreate, {
  updateTabsetPanel(session, "survivalTab", selected = "Data")
  if (!string.is.dimension(input$survivalDataDimension, 1)) {
    showModal(modalDialog(title = "Oops!", "Invalid number of stages.",
                          easyClose = TRUE))
    return()
  }
  d <- parse.dimension.string(input$survivalDataDimension)
  if (is.null(rv$survival.data)) {
    rv$survival.data <- empty.data(d + 1, 1,
                                   row.name = c(paste("Stage", 1:d), "Survive"), col.name = "Count")
  } else if (nrow(rv$survival.data) != d + 1) {
    showModal(modalDialog(title = "Confirmation", "Modify the number of stages?",
                          footer = tagList(actionButton("survivalDataModifyOk", "OK"),
                                           actionButton("survivalDataModifyCancel", "Cancel"))))
  }
})

observeEvent(input$survivalDataModifyOk, {
  d <- parse.dimension.string(input$survivalDataDimension)
  row <- min(nrow(rv$survival.data), d + 1)
  df <- empty.data(d + 1, 1, row.name = c(paste("Stage", 1:d), "Survive"),
                   col.name = "Count")
  df[1:row,] <- hot_to_r(input$survivalData)[1:row,]
  rv$survival.data <- df
  removeModal()
})

observeEvent(input$survivalDataModifyCancel, {
  updateTextInput(session, "survivalDataDimension",
                  value = nrow(rv$survival.data) - 1)
  removeModal()
})

observeEvent(input$survivalDataFile, {
  showModal(modalDialog(title = "File Information",
                        radioButtons("survivalDataFileFormat",
                                     label = "Data format",
                                     choices = list("Rows of subjects", "Contingency table")),
                        textInput("survivalDataFileColumn", label = "Which column in file?"),
                        footer = tagList(actionButton("survivalDataFileOk", "OK"),
                                         modalButton("Cancel"))))
})

observe({
  req(input$survivalDataFileFormat)
  if (input$survivalDataFileFormat == "Rows of subjects") {
    shinyjs::enable("survivalDataFileColumn")
  } else {
    shinyjs::disable("survivalDataFileColumn")
  }
})

observeEvent(input$survivalDataFileOk, {
  if (input$survivalDataFileFormat == "Rows of subjects") {
    if (!string.is.positive.integer(input$survivalDataFileColumn)) {
      showNotification("Oops! Invalid column number.", type = "message")
      return()
    }
    col <- as.numeric(input$survivalDataFileColumn)
    df <- read.csv(input$survivalDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (col > ncol(df)) {
      showNotification(paste("Oops! Only", ncol(df), "columns in the file."),
                       type = "message")
      return()
    }
    rv$survival.data <- survival.subject.to.data(df[,col])
    removeModal()
  } else {
    df <- read.csv(input$survivalDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (nrow(df) > 1) {
      row.name <- c(paste("Stage", 1:(nrow(df) - 1)), "Survive")
    } else {
      row.name <- "Survive"
    }
    rv$survival.data <- matrix.data(data.matrix(df), row.name = row.name,
                                    col.name = "Count")
    removeModal()
  }
  updateTabsetPanel(session, "survivalTab", selected = "Data")
})

observeEvent(input$survivalDataRemove, {
  updateTabsetPanel(session, "survivalTab", selected = "Data")
  rows <- parse.group.string(input$survivalDataRemoveRow)
  if (length(rows) == 0) {
    return()
  }
  if (length(rows) > 1 || is.na(rows) ||
      max(unlist(rows)) > nrow(hot_to_r(input$survivalData)) - 1) {
    showModal(modalDialog(title = "Oops!", "Invalid stage(s).",
                          easyClose = TRUE))
    return()
  }
  if (any(is.na(suppressWarnings(data.numeric.matrix(hot_to_r(input$survivalData)))))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  showModal(modalDialog(title = "Confirmation", "Remove the stage(s)?",
                        footer = tagList(actionButton("survivalDataRemoveOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$survivalDataRemoveOk, {
  rows <- parse.group.string(input$survivalDataRemoveRow)[[1]]
  m <- data.frame(hot_to_r(input$survivalData)[-rows,])
  if (nrow(m) > 1) {
    row.name <- c(paste("Stage", 1:(nrow(m) - 1)), "Survive")
  } else {
    row.name <- "Survive"
  }
  dimnames(m) <- list(row.name, "Count")
  rv$survival.data <- m
  removeModal()
})

observeEvent(input$survivalDataCollapse, {
  updateTabsetPanel(session, "survivalTab", selected = "Data")
  rows <- parse.group.string(input$survivalDataCollapseRow)
  if (length(rows) == 0) {
    return()
  }
  if (length(rows) == 1 && is.na(rows) ||
      length(rows) > 0 && max(unlist(rows)) > nrow(hot_to_r(input$survivalData)) - 1) {
    showModal(modalDialog(title = "Oops!", "Invalid stage groups.",
                          easyClose = TRUE))
    return()
  }
  if (!all(is.consecutive.group(rows))) {
    showModal(modalDialog(title = "Oops!", "Each stage group should be consecutive.",
                          easyClose = TRUE))
    return()
  }
  if (any(is.na(suppressWarnings(data.numeric.matrix(hot_to_r(input$survivalData)))))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  showModal(modalDialog(title = "Confirmation", "Collapse the data?",
                        footer = tagList(actionButton("survivalDataCollapseOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$survivalDataCollapseOk, {
  rows <- parse.group.string(input$survivalDataCollapseRow)
  m <- collapse.data(data.numeric.matrix(hot_to_r(input$survivalData)), row.group = rows, as.data.metrix = TRUE)
  if (nrow(m) > 1) {
    row.name <- c(paste("Stage", 1:(nrow(m) - 1)), "Survive")
  } else {
    row.name <- "Survive"
  }
  dimnames(m) <- list(row.name, "Count")
  rv$survival.data <- m
  removeModal()
})


output$survivalDataDownload <- downloadHandler(
  filename = "survival_data.csv",
  content = function(file) {
    write.table(hot_to_r(input$survivalData), file, quote = FALSE,
                sep = ",", row.names = FALSE, col.names = FALSE,
                qmethod = "double")
  }
)

observeEvent(input$survivalDataClear, {
  showModal(modalDialog(title = "Confirmation", "Clear the data?",
                        footer = tagList(actionButton("survivalDataClearOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$survivalDataClearOk, {
  updateTabsetPanel(session, "survivalTab", selected = "Data")
  rv$survival.data <- NULL
  removeModal()
})

observe({
  if (is.null(rv$survival.data)) {
    updateTextInput(session, "survivalDataDimension", value = "")
    updateActionButton(session, "survivalDataCreate", label = "Create")
    updateTextInput(session, "survivalDataRemoveRow", value = "")
    updateTextInput(session, "survivalDataCollapseRow", value = "")
    shinyjs::disable("survivalDataRemove")
    shinyjs::disable("survivalDataCollapse")
    shinyjs::disable("survivalDataDownload")
    shinyjs::disable("survivalDataClear")
  } else {
    updateTextInput(session, "survivalDataDimension",
                    value = nrow(rv$survival.data) - 1)
    updateActionButton(session, "survivalDataCreate", label = "Modify")
    shinyjs::enable("survivalDataRemove")
    shinyjs::enable("survivalDataCollapse")
    shinyjs::enable("survivalDataDownload")
    shinyjs::enable("survivalDataClear")
  }
})

survival.show.result <- function(title, s, select = FALSE) {
  id <- survival.tab.counter <<- survival.tab.counter + 1
  title <- paste(title, id, collapse=" ")
  text.id <- paste0("survivalResult", id)
  
  appendTab("survivalTab",
            tabPanel(title,
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(),
                     actionButton(paste0("survivalResultClose", id),
                                  label = "Close"),
                     br(), br()),
            select = select)
  
  code <- gsub('%s', s, 'output[[text.id]] <- renderText({"%s"})')
  eval(parse(text=code))
  
  code <- gsub('%id', id,
               'observeEvent(input$survivalResultClose%id, {
        removeTab("survivalTab", "%title")
      })', fixed=TRUE)
  code <- gsub('%title', title, code, fixed=TRUE)
  eval(parse(text=code))
}

survival.show.plot <- function(title, uncond.survival.freq,
                               uncond.survival.freq.envelope, expected.uncond.survival.prob,
                               expected.uncond.survival.prob.envelope, first.stage.idx) {
  
  id <- survival.tab.counter <<- survival.tab.counter + 1
  title <- paste(title, id, collapse=" ")
  survival.computed[[id]] <- list(
    title = title,
    uncond.survival.freq = uncond.survival.freq,
    uncond.survival.freq.envelope = uncond.survival.freq.envelope,
    expected.uncond.survival.prob = expected.uncond.survival.prob,
    expected.uncond.survival.prob.envelope = expected.uncond.survival.prob.envelope,
    first.stage.idx = first.stage.idx,
    n = length(uncond.survival.freq),
    y.min = min(tail(uncond.survival.freq, 1) -
                  tail(uncond.survival.freq.envelope,1),
                tail(expected.uncond.survival.prob, 1) -
                  tail(expected.uncond.survival.prob.envelope, 1)))
  
  appendTab("survivalTab",
            tabPanel(
              title,
              br(),
              fluidRow(
                column(width = 4,
                       wellPanel(
                         helpText('Plot Envelope'),
                         textInput(paste0("survivalProbabilityValue", id),
                                   label = "Probability value"),
                         actionButton(paste0("survivalComputeEnvelope", id),
                                      label = "Compute!")
                       ),
                       verbatimTextOutput(paste0("survivalEnvelopeResult", id)),
                       actionButton(paste0("survivalResultClose", id),
                                    label = "Close")
                ),
                column(width = 4,
                       plotOutput(paste0("survivalEmpiricalPlot", id))
                ),
                column(width = 4,
                       plotOutput(paste0("survivalModelPlot", id))
                )
              ),
              br()
            )
  )
  
  output[[paste0("survivalEmpiricalPlot", id)]] <- renderPlot(
    ggplot(data.frame(stage=(first.stage.idx - 1) + (1:survival.computed[[id]]$n) - 1,
                      lower=uncond.survival.freq - uncond.survival.freq.envelope,
                      value=uncond.survival.freq, upper=uncond.survival.freq +
                        uncond.survival.freq.envelope), aes(x=stage)) +
      xlab("Stage") +
      ylab("Frequency") + 
      ggtitle("Empirical Plot") +
      theme(title = element_text(size=10)) +
      geom_line(aes(y=lower), color="#0072B2", linetype="dashed") +
      geom_line(aes(y=value), color="#D55E00") +
      geom_line(aes(y=upper), color="#0072B2", linetype="dashed") +
      ylim(survival.computed[[id]]$y.min, 1) +
      theme_grey(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5)))
  
  output[[paste0("survivalModelPlot", id)]] <- renderPlot(
    ggplot(data.frame(stage=(first.stage.idx - 1) + (1:survival.computed[[id]]$n) - 1,
                      lower=expected.uncond.survival.prob - expected.uncond.survival.prob.envelope,
                      value=expected.uncond.survival.prob, upper=expected.uncond.survival.prob +
                        expected.uncond.survival.prob.envelope), aes(x=stage)) +
      xlab("Stage") +
      ylab("Probability") + 
      ggtitle("Model Plot") +
      theme(title = element_text(size=10)) +
      geom_line(aes(y=lower), color="#0072B2", linetype="dashed") +
      geom_line(aes(y=value), color="#D55E00") +
      geom_line(aes(y=upper), color="#0072B2", linetype="dashed") +
      ylim(survival.computed[[id]]$y.min, 1) +
      theme_grey(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5)))
  
  code <- gsub('%id', id,
               'observeEvent(input$survivalComputeEnvelope%id, {
        # Propensity threshold is clean?
        prob.value <- suppressWarnings(as.numeric(input$survivalProbabilityValue%id))
        if (is.na(prob.value) || prob.value < 0 || prob.value > 1) {
          showModal(modalDialog(title = "Oops!", "Probability value should be between 0 and 1.",
                                easyClose = TRUE))
          return()
        }
        # Perform linear interpolation with R built-in "approx" function to find
        # margins.
        n <- length(survival.computed[[%id]]$uncond.survival.freq)
        temp.s <- 1:n-1
        # For empirical
        empirical.l <- approx(survival.computed[[%id]]$uncond.survival.freq -
          survival.computed[[%id]]$uncond.survival.freq.envelope, temp.s,
          prob.value)$y
        empirical.m <- approx(survival.computed[[%id]]$uncond.survival.freq,
          temp.s, prob.value)$y
        empirical.u <- approx(survival.computed[[%id]]$uncond.survival.freq +
          survival.computed[[%id]]$uncond.survival.freq.envelope, temp.s,
          prob.value)$y
        empirical.w <- survival.margin.from.prob(temp.s,
            survival.computed[[%id]]$uncond.survival.freq,
            survival.computed[[%id]]$uncond.survival.freq +
            survival.computed[[%id]]$uncond.survival.freq.envelope, prob.value)
        s <- ""
        s <- paste0(s, "Plot Envelope Result\n")
        s <- paste0(s, "====================\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Empirical Envelope\n")
        s <- paste0(s, "------------------\n")
        s <- paste0(s, "Left stage =      ", round(empirical.l, 2), "\n")
        s <- paste0(s, "Middle stage =    ", round(empirical.m, 2), "\n")
        s <- paste0(s, "Right stage =     ", round(empirical.u, 2), "\n")
        s <- paste0(s, "Vertical margin = ", signif(empirical.w, 4), "\n")
        # For model
        model.l <- approx(survival.computed[[%id]]$expected.uncond.survival.prob -
          survival.computed[[%id]]$expected.uncond.survival.prob.envelope, temp.s,
          prob.value)$y
        model.m <- approx(survival.computed[[%id]]$expected.uncond.survival.prob, temp.s,
          prob.value)$y
        model.u <- approx(survival.computed[[%id]]$expected.uncond.survival.prob +
          survival.computed[[%id]]$expected.uncond.survival.prob.envelope, temp.s,
          prob.value)$y
        model.w <- survival.margin.from.prob(temp.s,
          survival.computed[[%id]]$expected.uncond.survival.prob,
          survival.computed[[%id]]$expected.uncond.survival.prob +
          survival.computed[[%id]]$expected.uncond.survival.prob.envelope,
          prob.value)
        s <- paste0(s, "\n")
        s <- paste0(s, "Model Envelope\n")
        s <- paste0(s, "--------------\n")
        s <- paste0(s, "Left stage =      ", round(model.l, 2), "\n")
        s <- paste0(s, "Middle stage =    ", round(model.m, 2), "\n")
        s <- paste0(s, "Right stage =     ", round(model.u, 2), "\n")
        s <- paste0(s, "Vertical margin = ", signif(model.w, 4), "\n")
        output$survivalEnvelopeResult%id <- renderText({s})
        
        # Re-plot with horizontal gray lines at prob.value height
        output$survivalEmpiricalPlot%id <- renderPlot(
          ggplot(data.frame(stage=(survival.computed[[%id]]$first.stage.idx - 1) + (1:survival.computed[[%id]]$n) - 1,
            lower=survival.computed[[%id]]$uncond.survival.freq - survival.computed[[%id]]$uncond.survival.freq.envelope,
            value=survival.computed[[%id]]$uncond.survival.freq, upper=survival.computed[[%id]]$uncond.survival.freq +
            survival.computed[[%id]]$uncond.survival.freq.envelope), aes(x=stage)) +
            xlab("Stage") +
            ylab("Frequency") + 
            ggtitle("Empirical Plot") +
            theme(title = element_text(size=10)) +
            geom_line(aes(y=lower), color="#0072B2", linetype="dashed") +
            geom_line(aes(y=value), color="#D55E00") +
            geom_line(aes(y=upper), color="#0072B2", linetype="dashed") +
            ylim(survival.computed[[%id]]$y.min, 1) +
            geom_hline(aes(yintercept=prob.value), colour="#777777") +
            theme_grey(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5)))

        output$survivalModelPlot%id <- renderPlot(
          ggplot(data.frame(stage=(survival.computed[[%id]]$first.stage.idx - 1) + (1:survival.computed[[%id]]$n) - 1,
            lower=survival.computed[[%id]]$expected.uncond.survival.prob - survival.computed[[%id]]$expected.uncond.survival.prob.envelope,
            value=survival.computed[[%id]]$expected.uncond.survival.prob, upper=survival.computed[[%id]]$expected.uncond.survival.prob +
            survival.computed[[%id]]$expected.uncond.survival.prob.envelope), aes(x=stage)) +
            xlab("Stage") +
            ylab("Probability") + 
            ggtitle("Model Plot") +
            theme(title = element_text(size=10)) +
            geom_line(aes(y=lower), color="#0072B2", linetype="dashed") +
            geom_line(aes(y=value), color="#D55E00") +
            geom_line(aes(y=upper), color="#0072B2", linetype="dashed") +
            ylim(survival.computed[[%id]]$y.min, 1) +
            geom_hline(aes(yintercept=prob.value), colour="#777777") +
            theme_grey(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5)))
      })', fixed=TRUE)
  eval(parse(text=code))
  
  code <- gsub('%id', id,
               'observeEvent(input$survivalResultClose%id, {
        removeTab("survivalTab", "%title")
      })', fixed=TRUE)
  code <- gsub('%title', title, code, fixed=TRUE)
  eval(parse(text=code))
}

observeEvent(input$survivalGoHomoAccdc, {
  # Data is clean?
  if (is.null(rv$survival.data)) {
    showModal(modalDialog(title = "Oops!", "There is no data.",
                          easyClose = TRUE))
    return()
  }
  m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$survivalData)))
  if (any(is.na(m))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  if (any(m < 0)) {
    showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                          easyClose = TRUE))
    return()
  }
  if (nrow(m) < 2) {
    showModal(modalDialog(title = "Oops!", "Data should have at least 1 stage.",
                          easyClose = TRUE))
    return()
  }
  data <- as.vector(m)
  num.stage <- length(data) - 1
  # Splining?
  splining.group <- survival.parse.splining.string(input$survivalSplining,
                                                   num.stage)
  if (length(splining.group) == 1 && is.na(splining.group)) {
    showModal(modalDialog(title = "Oops!", "Invalid splining groups.",
                          easyClose = TRUE))
    return()
  }
  show.graph <- input$survivalShowGraph
  # Do computation
  original <- data
  data[data == 0] <- 0.5
  is.first.tab <- TRUE
  for (g in splining.group) {
    original.subset <- c(original[g[1]:g[2]], sum(original[-seq_len(g[2])]))
    data.subset <- c(data[g[1]:g[2]], sum(data[-seq_len(g[2])]))
    s <- ""
    # Homogeneous model
    homo.computed <- survival.homogeneous.computation(
      data.subset[-length(data.subset)], tail(data.subset, 1))
    homo.computed$observed <- c(original.subset[-length(original.subset)],
                                tail(original.subset, 1))
    s <- paste0(s, survival.homogeneous.report(homo.computed, g[1]))
    s <- paste0(s, "\n")
    s <- paste0(s, "\n")
    # ACC/DC model
    acc.dc.computed <- survival.acc.dc.computation(
      data.subset[-length(data.subset)], tail(data.subset, 1))
    acc.dc.computed$observed <- c(original.subset[-length(original.subset)],
                                  tail(original.subset, 1))
    s <- paste0(s, survival.acc.dc.report(acc.dc.computed, g[1]))
    # Show result
    if (length(g) == 1) {
      title <- as.character(g)
    } else {
      title <- paste0(g[1], "-", g[2])
    }
    survival.show.result(paste0("Stage ", title, " Result "),
                         s, select = is.first.tab)
    is.first.tab <- FALSE
    # Show plot
    if (show.graph) {
      survival.show.plot(paste0("Stage ", title, " Homogeneous Plot"),
                         homo.computed$uncond.survival.freq,
                         homo.computed$uncond.survival.freq.envelope,
                         homo.computed$expected.uncond.survival.prob,
                         homo.computed$expected.uncond.survival.prob.envelope, g[1])
      if (!acc.dc.computed$computation.has.singularity) {
        survival.show.plot(paste0("Stage ", title, " ACC/DC Plot"),
                           acc.dc.computed$uncond.survival.freq,
                           acc.dc.computed$uncond.survival.freq.envelope,
                           acc.dc.computed$expected.uncond.survival.prob,
                           acc.dc.computed$expected.uncond.survival.prob.envelope, g[1])
      }
    }
  }
})

observeEvent(input$survivalGoTraining, {
  # Data is clean?
  if (is.null(rv$survival.data)) {
    showModal(modalDialog(title = "Oops!", "There is no data.",
                          easyClose = TRUE))
    return()
  }
  m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$survivalData)))
  if (any(is.na(m))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  if (any(m < 0)) {
    showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                          easyClose = TRUE))
    return()
  }
  if (nrow(m) < 2) {
    showModal(modalDialog(title = "Oops!", "Data should have at least 1 stage.",
                          easyClose = TRUE))
    return()
  }
  data <- as.vector(m)
  num.stage <- length(data) - 1
  # Training portion is clean?
  training.portion <- suppressWarnings(as.numeric(input$survivalTrainingPortion))
  if (is.na(training.portion) || training.portion <= 0 ||
      training.portion >= 100) {
    showModal(modalDialog(title = "Oops!", "Training portion must be between 0 and 100.",
                          easyClose = TRUE))
    return()
  }
  training.fraction <- training.portion / 100
  reproducible <- input$survivalReproducible
  # Do sampling
  death.per.period <- data[-length(data)]
  still.survive <- tail(data, 1)
  training.test.vector <- survival.oos.sampling(death.per.period,
                                                still.survive, training.fraction, reproducible)
  training.vector <- training.test.vector$training.vector
  test.vector <- training.test.vector$test.vector
  survival.contingency.vector <<- data
  survival.training.portion <<- training.portion
  survival.training.vector <<- training.vector
  survival.test.vector <<- test.vector
  computed <- survival.oos.training.computation(
    training.vector[-length(training.vector)], tail(training.vector, 1))
  s <- survival.oos.training.report(computed, training.portion)
  survival.show.result("Training Result", s, select = TRUE)
})

observeEvent(input$survivalGoTesting, {
  # Data is clean?
  if (is.null(rv$survival.data)) {
    showModal(modalDialog(title = "Oops!", "There is no data.",
                          easyClose = TRUE))
    return()
  }
  m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$survivalData)))
  if (any(is.na(m))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  if (any(m < 0)) {
    showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                          easyClose = TRUE))
    return()
  }
  if (nrow(m) < 2) {
    showModal(modalDialog(title = "Oops!", "Data should have at least 1 stage.",
                          easyClose = TRUE))
    return()
  }
  data <- as.vector(m)
  num.stage <- length(data) - 1
  # Training portion is clean?
  training.portion <- suppressWarnings(as.numeric(input$survivalTrainingPortion))
  if (is.na(training.portion) || training.portion <= 0 ||
      training.portion >= 100) {
    showModal(modalDialog(title = "Oops!", "Training portion must be between 0 and 100.",
                          easyClose = TRUE))
    return()
  }
  training.fraction <- training.portion / 100
  # Reproducible?
  reproducible <- input$survivalReproducible
  # Splining?
  splining.group <- survival.parse.splining.string(input$survivalSpliningTesting,
                                                   num.stage)
  if (length(splining.group) == 1 && is.na(splining.group)) {
    showModal(modalDialog(title = "Oops!", "Invalid splining groups.",
                          easyClose = TRUE))
    return()
  }
  show.graph <- input$survivalShowGraphTesting
  # Do sampling?
  death.per.period <- data[-length(data)]
  still.survive <- tail(data, 1)
  if (!identical(data, survival.contingency.vector) ||
      !identical(training.portion, survival.training.portion)) {
    training.test.vector <- survival.oos.sampling(death.per.period,
                                                  still.survive, training.fraction, reproducible)
    training.vector <- training.test.vector$training.vector
    test.vector <- training.test.vector$test.vector
    survival.contingency.vector <<- data
    survival.training.portion <<- training.portion
    survival.training.vector <<- training.vector
    survival.test.vector <<- test.vector
  } else {
    test.vector <- survival.test.vector
  }
  # Do computation
  data <- original <- test.vector
  data[data == 0] <- 0.5
  is.first.tab <- TRUE
  for (g in splining.group) {
    original.subset <- c(original[g[1]:g[2]], sum(original[-seq_len(g[2])]))
    data.subset <- c(data[g[1]:g[2]], sum(data[-seq_len(g[2])]))
    s <- ""
    # Homogeneous model
    homo.computed <- survival.homogeneous.computation(
      data.subset[-length(data.subset)], tail(data.subset, 1))
    homo.computed$observed <- c(original.subset[-length(original.subset)],
                                tail(original.subset, 1))
    s <- paste0(s, survival.homogeneous.report(homo.computed, g[1],
                                               100 - training.portion))
    s <- paste0(s, "\n")
    s <- paste0(s, "\n")
    # ACC/DC model
    acc.dc.computed <- survival.acc.dc.computation(
      data.subset[-length(data.subset)], tail(data.subset, 1))
    acc.dc.computed$observed <- c(original.subset[-length(original.subset)],
                                  tail(original.subset, 1))
    s <- paste0(s, survival.acc.dc.report(acc.dc.computed, g[1]))
    # Show result
    if (length(g) == 1) {
      title <- as.character(g)
    } else {
      title <- paste0(g[1], "-", g[2])
    }
    survival.show.result(paste0("Stage ", title, " Result "),
                         s, select = is.first.tab)
    is.first.tab <- FALSE
    # Show plot
    if (show.graph) {
      survival.show.plot(paste0("Stage ", title, " Homogeneous Plot"),
                         homo.computed$uncond.survival.freq,
                         homo.computed$uncond.survival.freq.envelope,
                         homo.computed$expected.uncond.survival.prob,
                         homo.computed$expected.uncond.survival.prob.envelope, g[1])
      if (!acc.dc.computed$computation.has.singularity) {
        survival.show.plot(paste0("Stage ", title, " ACC/DC Plot"),
                           acc.dc.computed$uncond.survival.freq,
                           acc.dc.computed$uncond.survival.freq.envelope,
                           acc.dc.computed$expected.uncond.survival.prob,
                           acc.dc.computed$expected.uncond.survival.prob.envelope, g[1])
      }
    }
  }
})

observeEvent(input$survivalGoExplain, {
  showModal(modalDialog(title = "Survival Analysis Explanatory Variable",
                        helpText('Step 1: Import Data'),
                        fileInput("survivalExplanatoryDataFile", label = NULL,
                                  accept = c(".csv")),
                        hr(style="border-color: lightgray;"),
                        helpText('Step 2: Create Target Variable'),
                        selectInput("survivalExplanatoryTargetColumn",
                                    label = "Which target column in file?", choices = NULL),
                        textInput("survivalExplanatoryStage", label = "Which stages?"),
                        hr(style="border-color: lightgray;"),
                        helpText('Step 3: Export Data With Target Variable'),
                        shinyjs::disabled(downloadButton("survivalExplanatoryDataDownload", label = "Download")),
                        hr(style="border-color: lightgray;"),
                        helpText('Step 4: Sort Explanatory Variables By Descriptiveness'),
                        textInput("survivalExplanatoryVariableColumn",
                                  label = "Which explanatory columns in file?"),
                        actionButton("survivalExplanatorySort", label = "Sort!"),
                        br(),
                        br(),
                        verbatimTextOutput("survivalExplanatoryDescriptiveness"),
                        footer = tagList(modalButton("Close"))))
})

observeEvent(input$survivalExplanatoryDataFile, {
  rv$survival.explanatory.data <- read.csv(
    input$survivalExplanatoryDataFile$datapath, header = FALSE,
    strip.white = TRUE, stringsAsFactors = FALSE)
})

observe({
  req(rv$survival.explanatory.data)
  num.col <- ncol(rv$survival.explanatory.data)
  updateSelectInput(session, "survivalExplanatoryTargetColumn",
                    choices = seq_len(num.col))
  shinyjs::enable("survivalExplanatoryDataDownload")
})

output$survivalExplanatoryDataDownload <- downloadHandler(
  filename = "survival_explanatory_data.csv",
  content = function(file) {
    stages <- survival.parse.stage.string(input$survivalExplanatoryStage)
    if (length(stages) == 1 && is.na(stages)) {
      showNotification("Oops! Invalid input for stages.", type = "message")
      return()
    }
    # Create binary target variable
    org.data <- rv$survival.explanatory.data
    binary.target.var <- rep(2, nrow(org.data))
    target.col <- as.numeric(input$survivalExplanatoryTargetColumn)
    binary.target.var[org.data[,target.col] %in% stages] <- 1
    new.data <- cbind(org.data, binary.target.var)
    # Write to file
    write.table(new.data, file, quote = FALSE, sep = ",", row.names = FALSE,
                col.names = FALSE, qmethod = "double")
  }
)

observeEvent(input$survivalExplanatorySort, {
  # Check that stages are valid
  stages <- survival.parse.stage.string(input$survivalExplanatoryStage)
  if (length(stages) == 1 && is.na(stages)) {
    showNotification("Oops! Invalid input for the stages.", type = "message")
    return()
  }
  # Check that explanatory variable columns are valid
  explanatory.col <- parse.group.string(input$survivalExplanatoryVariableColumn)
  if (length(explanatory.col) == 0 || length(explanatory.col) == 1 && is.na(explanatory.col)) {
    showNotification("Oops! Invalid input for the explanatory column.", type = "message")
    return()
  }
  org.data <- rv$survival.explanatory.data
  explanatory.col <- explanatory.col[[1]]
  if (max(explanatory.col) > ncol(org.data)) {
    showNotification("Oops! Explanatory column exceeds the number of columns in the data file.", type = "message")
    return()
  }
  target.col <- as.numeric(input$survivalExplanatoryTargetColumn)
  if (any(explanatory.col == target.col)) {
    showNotification("Oops! Explanatory columns should not contain the target column.", type = "message")
    return()
  }
  # Create the binary target variable
  binary.target.var <- rep(2, nrow(org.data))
  binary.target.var[org.data[,target.col] %in% stages] <- 1
  if (length(unique(binary.target.var)) == 1) {
    showNotification("Oops! The target variable is not binary.", type = "message")
    return()
  }
  # Sort the explanatory variable columns
  new.data <- cbind(org.data, binary.target.var)
  explanatory.ordering <- threedimensional.sort.explanatory.variables(
    new.data, ncol(new.data), explanatory.col)
  output$survivalExplanatoryDescriptiveness <- renderText({
    paste("Sorted by descriptiveness:", paste(explanatory.ordering, collapse=", "))})
})

# Loyalty variable
rv$loyalty.data <- NULL
rv$loyalty.brand.omission.data <- NULL
rv$loyalty.explanatory.data <- NULL
loyalty.tab.counter <- 0

output$loyaltyData <- renderRHandsontable({
  req(rv$loyalty.data)
  rhandsontable(rv$loyalty.data, useTypes = FALSE) %>%
    hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
    hot_cols(colWidths = 80) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$loyaltyDataCreate, {
  updateTabsetPanel(session, "loyaltyTab", selected = "Data")
  if (!string.is.dimension(input$loyaltyDataDimension, 1)) {
    showModal(modalDialog(title = "Oops!", "Invalid number of brands.",
                          easyClose = TRUE))
    return()
  }
  d <- parse.dimension.string(input$loyaltyDataDimension)
  if (is.null(rv$loyalty.data)) {
    name <- paste("Brand", 1:d)
    rv$loyalty.data <- empty.data(d, d, row.name = name, col.name = name)
  } else if (nrow(rv$loyalty.data) != d) {
    showModal(modalDialog(title = "Confirmation", "Modify the number of brands?",
                          footer = tagList(actionButton("loyaltyDataModifyOk", "OK"),
                                           actionButton("loyaltyDataModifyCancel", "Cancel"))))
  }
})

observeEvent(input$loyaltyDataModifyOk, {
  d <- parse.dimension.string(input$loyaltyDataDimension)
  name <- paste("Brand", 1:d)
  df <- empty.data(d, d, row.name = name, col.name = name)
  row <- min(nrow(rv$loyalty.data), d)
  df[1:row,1:row] <- hot_to_r(input$loyaltyData)[1:row,1:row]
  rv$loyalty.data <- df
  removeModal()
})

observeEvent(input$loyaltyDataModifyCancel, {
  updateTextInput(session, "loyaltyDataDimension",
                  value = nrow(rv$loyalty.data))
  removeModal()
})

observeEvent(input$loyaltyDataFile, {
  showModal(modalDialog(title = "File Information",
                        radioButtons("loyaltyDataFileFormat",
                                     label = "Data format",
                                     choices = list("Rows of subjects", "Contingency table")),
                        textInput("loyaltyDataFileRow", label = "Which column in file for 1st purchase?"),
                        textInput("loyaltyDataFileColumn", label = "Which column in file for 2nd purchase?"),
                        footer = tagList(actionButton("loyaltyDataFileOk", "OK"),
                                         modalButton("Cancel"))))
})

observe({
  req(input$loyaltyDataFileFormat)
  if (input$loyaltyDataFileFormat == "Rows of subjects") {
    shinyjs::enable("loyaltyDataFileRow")
    shinyjs::enable("loyaltyDataFileColumn")
  } else {
    shinyjs::disable("loyaltyDataFileRow")
    shinyjs::disable("loyaltyDataFileColumn")
  }
})

observeEvent(input$loyaltyDataFileOk, {
  if (input$loyaltyDataFileFormat == "Rows of subjects") {
    if (!string.is.positive.integer(input$loyaltyDataFileRow)) {
      showNotification("Oops! Invalid row number.", type = "message")
      return()
    }
    if (!string.is.positive.integer(input$loyaltyDataFileColumn)) {
      showNotification("Oops! Invalid column number.", type = "message")
      return()
    }
    row <- as.numeric(input$loyaltyDataFileRow)
    col <- as.numeric(input$loyaltyDataFileColumn)
    if (row == col) {
      showNotification("Oops! Use different columns for 1st and 2nd purchases.",
                       type = "message")
      return()
    }
    df <- read.csv(input$loyaltyDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (max(row, col) > ncol(df)) {
      showNotification(paste("Oops! Only", ncol(df), "columns in the file."),
                       type = "message")
      return()
    }
    m <- table.data(table(df[,c(row, col)]))
    if (nrow(m) != ncol(m)) {
      showNotification("Oops! The 1st and 2nd purchases have different numbers of brands.",
                       type = "message")
      return()
    }
    name <- paste("Brand", 1:nrow(m))
    dimnames(m) <- list(name, name)
    rv$loyalty.data <- m
    removeModal()
  } else {
    df <- read.csv(input$loyaltyDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (nrow(df) != ncol(df)) {
      showNotification("Oops! The 1st and 2nd purchases have different numbers of brands.",
                       type = "message")
      return()
    }
    name <- paste("Brand", 1:nrow(df))
    rv$loyalty.data <- matrix.data(data.matrix(df), row.name = name,
                                   col.name = name)
    removeModal()
  }
  updateTabsetPanel(session, "loyaltyTab", selected = "Data")
})

observeEvent(input$loyaltyOmitBrand, {
  updateTabsetPanel(session, "loyaltyTab", selected = "Data")
  brand <- parse.group.string(input$loyaltyBrandOmission)
  if (length(brand) != 1 || is.na(brand)) {
    showModal(modalDialog(title = "Oops!", "Invalid brands.",
                          easyClose = TRUE))
    return()
  }
  brand <- brand[[1]]
  data <- hot_to_r(input$loyaltyData)
  if (max(brand) > nrow(data)) {
    showModal(modalDialog(title = "Oops!", "Invalid brands.",
                          easyClose = TRUE))
    return()
  }
  if (length(brand) == nrow(data)) {
    showModal(modalDialog(title = "Oops!", "Cannot omit all brands.",
                          easyClose = TRUE))
    return()
  }
  showModal(modalDialog(title = "Confirmation", "Omit the brands?",
                        footer = tagList(actionButton("loyaltyOmitBrandOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$loyaltyOmitBrandOk, {
  brand <- parse.group.string(input$loyaltyBrandOmission)[[1]]
  data <- data.numeric.matrix(hot_to_r(input$loyaltyData))[-brand,-brand]
  if (length(data) == 1) {
    num.brand <- 1
  } else {
    num.brand <- nrow(data)
  }
  brand.name <- paste("Brand", seq_len(num.brand))
  rv$loyalty.data <- matrix.data(data, brand.name, brand.name)
  removeModal()
})

output$loyaltyDataDownload <- downloadHandler(
  filename = "loyalty_data.csv",
  content = function(file) {
    write.table(hot_to_r(input$loyaltyData), file, quote = FALSE,
                sep = ",", row.names = FALSE, col.names = FALSE,
                qmethod = "double")
  }
)

observeEvent(input$loyaltyDataClear, {
  showModal(modalDialog(title = "Confirmation", "Clear the data?",
                        footer = tagList(actionButton("loyaltyDataClearOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$loyaltyDataClearOk, {
  updateTabsetPanel(session, "loyaltyTab", selected = "Data")
  rv$loyalty.data <- NULL
  removeModal()
})

observe({
  if (is.null(rv$loyalty.data)) {
    updateTextInput(session, "loyaltyDataDimension", value = "")
    updateActionButton(session, "loyaltyDataCreate", label = "Create")
    shinyjs::disable("loyaltyOmitBrand")
    shinyjs::disable("loyaltyDataDownload")
    shinyjs::disable("loyaltyDataClear")
  } else {
    updateTextInput(session, "loyaltyDataDimension",
                    value = nrow(rv$loyalty.data))
    updateActionButton(session, "loyaltyDataCreate", label = "Modify")
    shinyjs::enable("loyaltyOmitBrand")
    shinyjs::enable("loyaltyDataDownload")
    shinyjs::enable("loyaltyDataClear")
  }
})

loyalty.show.result <- function(s) {
  x <- loyalty.tab.counter <<- loyalty.tab.counter + 1
  text.id <- paste0("loyaltyResult", x)
  
  appendTab("loyaltyTab",
            tabPanel(paste("Result", x),
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(),
                     actionButton(paste0("loyaltyResultClose", x),
                                  label = "Close"),
                     br(), br()),
            select = TRUE)
  
  output[[text.id]] <- renderText({s})
  
  code <- gsub('%x', x,
               'observeEvent(input$loyaltyResultClose%x, {
        removeTab("loyaltyTab", "Result %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

loyalty.show.plot <- function(bra, brl) {
  x <- loyalty.tab.counter
  plot.id <- paste0("loyaltyPlot", x)
  
  appendTab("loyaltyTab",
            tabPanel(paste("Plot", x),
                     br(), plotOutput(plot.id),
                     actionButton(paste0("loyaltyPlotClose", x),
                                  label = "Close"),
                     br(), br()))
  
  output[[plot.id]] <- renderPlot(
    ggplot(data.frame(bra=bra, brl=brl), aes(x=bra, y=brl)) + 
      xlab("BRA") +
      ylab("BRL") + 
      ggtitle("Brand Loyalty vs Appeal") +
      geom_point(colour="#0072B2", size=8, shape=21, fill="white") + 
      geom_text(aes(label=1:length(bra)), size=5) + 
      theme_grey(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5)))
  
  code <- gsub('%x', x,
               'observeEvent(input$loyaltyPlotClose%x, {
        removeTab("loyaltyTab", "Plot %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

observeEvent(input$loyaltyGo, {
  if (input$loyaltyModelName == "Brand Omission") {
    showModal(modalDialog(title = "Loyalty Analysis Brand Omission",
                          helpText('Step 1: Import Data'),
                          fileInput("loyaltyBrandOmissionDataFile", label = NULL,
                                    accept = c(".csv")),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 2: Data Format'),
                          radioButtons("loyaltyBrandOmissionDataFileFormat",
                                       label=NULL,
                                       choices=list("Rows of subjects", "Contingency table")),
                          textInput("loyaltyBrandOmissionDataFileFirst",
                                    label="Which column in file for 1st purchase?"),
                          textInput("loyaltyBrandOmissionDataFileSecond",
                                    label="Which column in file for 2nd purchase?"),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 3: Omit Brand(s)'),
                          textInput("loyaltyBrandOmissionWhich",
                                    label="Which brand(s) to be omitted?"),       
                          hr(style="border-color: lightgray;"),
                          helpText('Step 4: Export Data'),
                          shinyjs::disabled(downloadButton("loyaltyBrandOmissionDownload", label = "Download")),
                          footer = tagList(modalButton("Close"))))
  } else if (input$loyaltyModelName == "Explanatory Variable") {
    showModal(modalDialog(title = "Loyalty Analysis Explanatory Variable",
                          helpText('Step 1: Import Data'),
                          fileInput("loyaltyExplanatoryDataFile", label = NULL,
                                    accept = c(".csv")),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 2: Determine brand columns'),
                          textInput('loyaltyExplanatoryVariableFirstBrandColumn',
                                    label = "Which column for first purchase?"),
                          textInput('loyaltyExplanatoryVariableSecondBrandColumn',
                                    label = "Which column for second purchase?"),
                          helpText('Step 3: Create Target Variable'),
                          fluidRow(
                            column(width = 3, align="right",  tags$b("1st purchase is")),
                            column(width = 3,
                                   selectInput("loyaltyExplanatoryTargetWith1", label = NULL,
                                               choices = c("with", "not with"))
                            ),
                            column(width = 5,
                                   textInput("loyaltyExplanatoryTargetBrand1", label = NULL)
                            )
                          ),
                          fluidRow(
                            column(width = 3, align="right",  tags$b("2nd purchase is")),
                            column(width = 3,
                                   selectInput("loyaltyExplanatoryTargetWith2", label = NULL,
                                               choices = c("with", "not with"))
                            ),
                            column(width = 5,
                                   textInput("loyaltyExplanatoryTargetBrand2", label = NULL)
                            )
                          ),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 4: Export Data With Target Variable'),
                          shinyjs::disabled(downloadButton("loyaltyExplanatoryDataDownload", label = "Download")),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 5: Sort Explanatory Variables By Descriptiveness'),
                          textInput("loyaltyExplanatoryVariableColumn",
                                    label = "Which explanatory columns in file?"),
                          actionButton("loyaltyExplanatorySort", label = "Sort!"),
                          br(),
                          br(),
                          verbatimTextOutput("loyaltyExplanatoryDescriptiveness"),
                          footer = tagList(modalButton("Close"))))
  } else {
    # Data is clean?
    if (is.null(rv$loyalty.data)) {
      showModal(modalDialog(title = "Oops!", "There is no data.",
                            easyClose = TRUE))
      return()
    }
    m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$loyaltyData)))
    if (any(is.na(m))) {
      showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                            easyClose = TRUE))
      return()
    }
    if (any(m < 0)) {
      showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                            easyClose = TRUE))
      return()
    }
    if (input$loyaltyModelName == "M Model") {
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      # Perform the computation.
      result <- loyalty.m.model.computation(contingency.table)
      s <- ""
      s <- paste0(s, "M Model Result\n")
      s <- paste0(s, "==============\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Expected Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(result$expected, 2),
                                   left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, paste0("TAU(Y|X)= ",
                            signif(result$tau.given.row, 4), "  TAU(X|Y)= ",
                            signif(result$tau.given.col, 4)))
      s <- paste0(s, "\n\n")
      s <- paste0(s, paste0("X-Square= ",
                            round(result$chi.sq.stat, 2), "  L-Square= ",
                            round(result$l.sq.stat, 2), "  D.O.F.= ", result$d.o.f,
                            "  p-value= ", signif(result$p.value, 4)))
      s<- paste0(s, "\n\n")
      s <- paste0(s, "Parameter Estimates\n")
      s <- paste0(s, "-------------------\n")
      s <- paste0(s, "Delta: ", signif(result$delta, 4), "\n")
      s <- paste0(s, "Phi:\n")
      for (i in seq_along(result$phi)) {
        s <- paste0(s, "Phi[", i, "] = ", signif(result$phi[i], 4), "\n")
      }
      s <- paste0(s, "\n")
      s <- paste0(s, "Predator Table (1 is very dangerous, 0 is not)\n")
      s <- paste0(s, "----------------------------------------------\n")
      s <- paste0(s, matrix.string(round(result$predator.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Prey Table (1 is very weak, 0 is not)\n")
      s <- paste0(s, "-------------------------------------\n")
      s <- paste0(s, matrix.string(round(result$prey.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Measures\n")
      s <- paste0(s, "--------\n")
      s <- paste0(s, "TRL = ", signif(result$trl, 4), "   TAL = ",
                  signif(result$tal, 4), "\n")
      s <- paste0(s, "BRL: ", paste(round(result$brl, 2), collapse=" "), "\n")
      s <- paste0(s, "BRA: ", paste(round(result$bra, 2), collapse=" "), "\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Significance Matrix\n")
      s <- paste0(s, "-------------------\n")
      num.brand <- length(result$phi)
      temp <- matrix("", 2+num.brand, 1+num.brand)
      phi.ordering <- order(result$phi, decreasing=TRUE)
      
      # DEBUG M DISPLAY
      print(paste("DEBUG M DISPLAY: phi.ordering =", paste(phi.ordering, collapse=", ")))
      
      for(i in seq_len(num.brand)) {
        temp[2+i,1] <- paste0("Phi[", phi.ordering[i], "]")
      }
      for (j in seq_len(num.brand)) {
        temp[1,1+j] <- paste0("[", phi.ordering[j], "]",
                              round(result$phi[phi.ordering[j]], 2))
        temp[2,1+j] <- paste(rep("=", nchar(temp[1,1+j])+1), collapse="")
      }
      for (j in seq_len(num.brand)) {
        for (i in seq_len(num.brand)) {
          if (isTRUE(result$significance.table[i,j]) &&
              phi.ordering[i] < phi.ordering[j]) {
            print(paste("DEBUG M DISPLAY: X at i=", i, "j=", j, "phi.ordering[i]=", phi.ordering[i], "phi.ordering[j]=", phi.ordering[j]))
            temp[2+phi.ordering[i],1+phi.ordering[j]] <- "X"
          }
        }
      }
      s <- paste0(s, matrix.string(temp, spacing=2))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Model Diagnostics\n")
      s <- paste0(s, "-----------------\n")
      if (result$model.is.fit) {
        s <- paste0(s, "Model fits the observed data.\n")
      } else {
        s <- paste0(s, "Model does not fit the observed data.\n")
      }
      s <- paste0(s, "\n")
      s <- paste0(s, "Standardized Residuals\n")
      s <- paste0(s, "----------------------\n")
      s <- paste0(s, matrix.string(round(result$std.residuals,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, paste0("Number of significant residuals = ",
                            result$num.sig.residuals))
      loyalty.show.result(s)
      loyalty.show.plot(result$bra, result$brl)
    } else if (input$loyaltyModelName == "Q Model") {
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      # Perform the computation.
      result <- loyalty.q.model.computation(contingency.table)
      s <- ""
      s <- paste0(s, "Q Model Result\n")
      s <- paste0(s, "==============\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Expected Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(result$expected, 2),
                                   left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, paste0("TAU(Y|X)= ",
                            signif(result$tau.given.row, 4), "  TAU(X|Y)= ",
                            signif(result$tau.given.col, 4)))
      s <- paste0(s, "\n\n")
      s <- paste0(s, paste0("X-Square= ",
                            round(result$chi.sq.stat, 2), "  L-Square= ",
                            round(result$l.sq.stat, 2), "  D.O.F.= ", result$d.o.f,
                            "  p-value= ", signif(result$p.value, 4)))
      s<- paste0(s, "\n\n")
      s <- paste0(s, "Parameter Estimates\n")
      s <- paste0(s, "-------------------\n")
      s <- paste0(s, "Phi:\n")
      for (i in seq_along(result$phi)) {
        s <- paste0(s, "Phi[", i, "] = ", signif(result$phi[i], 4), "\n")
      }
      s <- paste0(s, "\n")
      s <- paste0(s, "Predator Table (1 is very dangerous, 0 is not)\n")
      s <- paste0(s, "----------------------------------------------\n")
      s <- paste0(s, matrix.string(round(result$predator.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Prey Table (1 is very weak, 0 is not)\n")
      s <- paste0(s, "-------------------------------------\n")
      s <- paste0(s, matrix.string(round(result$prey.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Measures\n")
      s <- paste0(s, "--------\n")
      s <- paste0(s, "TRL = ", signif(result$trl, 4), "   TAL = ",
                  signif(result$tal, 4), "\n")
      s <- paste0(s, "BRL: ", paste(round(result$brl, 2), collapse=" "), "\n")
      s <- paste0(s, "BRA: ", paste(round(result$bra, 2), collapse=" "), "\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Significance Matrix\n")
      s <- paste0(s, "-------------------\n")
      num.brand <- length(result$phi)
      temp <- matrix("", 2+num.brand, 1+num.brand)
      phi.ordering <- order(result$phi, decreasing=TRUE)
      
      # DEBUG Q DISPLAY
      print(paste("DEBUG Q DISPLAY: phi.ordering =", paste(phi.ordering, collapse=", ")))
      
      for(i in seq_len(num.brand)) {
        temp[2+i,1] <- paste0("Phi[", phi.ordering[i], "]")
      }
      for (j in seq_len(num.brand)) {
        temp[1,1+j] <- paste0("[", phi.ordering[j], "]",
                              round(result$phi[phi.ordering[j]], 2))
        temp[2,1+j] <- paste(rep("=", nchar(temp[1,1+j])+1), collapse="")
      }
      for (j in seq_len(num.brand)) {
        for (i in seq_len(num.brand)) {
          if (isTRUE(result$significance.table[i,j]) &&
              phi.ordering[i] < phi.ordering[j]) {
            print(paste("DEBUG Q DISPLAY: X at i=", i, "j=", j, "phi.ordering[i]=", phi.ordering[i], "phi.ordering[j]=", phi.ordering[j]))
            temp[2+phi.ordering[i],1+phi.ordering[j]] <- "X"
          }
        }
      }
      s <- paste0(s, matrix.string(temp, spacing=2))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Model Diagnostics\n")
      s <- paste0(s, "-----------------\n")
      if (result$model.is.fit) {
        s <- paste0(s, "Model fits the observed data.\n")
      } else {
        s <- paste0(s, "Model does not fit the observed data.\n")
      }
      s <- paste0(s, "\n")
      s <- paste0(s, "Standardized Residuals\n")
      s <- paste0(s, "----------------------\n")
      s <- paste0(s, matrix.string(round(result$std.residuals,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, paste0("Number of significant residuals = ",
                            result$num.sig.residuals))
      loyalty.show.result(s)
      loyalty.show.plot(result$bra, result$brl)
    } else if (input$loyaltyModelName == "Explore Model") {
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      # Perform the computation.
      result <- loyalty.explore.model.computation(contingency.table)
      s <- ""
      s <- paste0(s, "Explore Model Result\n")
      s <- paste0(s, "====================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Expected Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, "Same as observed data matrix.\n")
      s <- paste0(s, "\n")
      s <- paste0(s, paste0("TAU(Y|X)= ",
                            signif(result$tau.given.row, 4), "  TAU(X|Y)= ",
                            signif(result$tau.given.col, 4)))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Predator Table (1 is very dangerous, 0 is not)\n")
      s <- paste0(s, "----------------------------------------------\n")
      s <- paste0(s, matrix.string(round(result$predator.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Prey Table (1 is very weak, 0 is not)\n")
      s <- paste0(s, "-------------------------------------\n")
      s <- paste0(s, matrix.string(round(result$prey.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Measures\n")
      s <- paste0(s, "--------\n")
      s <- paste0(s, "TRL = ", signif(result$trl, 4), "   TAL = ",
                  signif(result$tal, 4), "\n")
      s <- paste0(s, "BRL: ", paste(round(result$brl, 2), collapse=" "), "\n")
      s <- paste0(s, "BRA: ", paste(round(result$bra, 2), collapse=" "))
      loyalty.show.result(s)
      loyalty.show.plot(result$bra, result$brl)
    } else if (input$loyaltyModelName == "Brand Sensitivity") {
      num.brand <- nrow(m)
      if (num.brand == 1) {
        showModal(modalDialog(title = "Oops!", "Brand sensitivity needs at least 2 brands.",
                              easyClose = TRUE))
        return()
      }
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      # Perform M and Q Model computation
      all.m.models <- list()
      all.q.models <- list()
      for (i in seq_len(num.brand)) {
        all.m.models[[i]] <- loyalty.m.model.computation(contingency.table[-i,-i], p.value.only=TRUE)
        all.q.models[[i]] <- loyalty.q.model.computation(contingency.table[-i,-i], p.value.only=TRUE)
      }
      s <- ""
      s <- paste0(s, "Loyalty Brand Sensitivity Result\n")
      s <- paste0(s, "================================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      output <- matrix("", 3 + num.brand, 5)
      output[1,1] <- "Omitted Brand"
      output[2,1] <- "-------------"
      output[3 + (1:num.brand)] <- 1:num.brand
      output[1,2] <- "M Model"
      output[2,2] <- "-------"
      output[3,2] <- "P-value"
      output[3,3] <- "Model fits?"
      output[1,4] <- "Q Model"
      output[2,4] <- "-------"
      output[3,4] <- "P-value"
      output[3,5] <- "Model fits?"
      for (i in seq_len(num.brand)) {
        output[3 + i, 2] <- signif(all.m.models[[i]]$p.value, 4)
        output[3 + i, 3] <- ifelse(all.m.models[[i]]$model.is.fit, "Yes", "No")
        output[3 + i, 4] <- signif(all.q.models[[i]]$p.value, 4)
        output[3 + i, 5] <- ifelse(all.q.models[[i]]$model.is.fit, "Yes", "No")
      }
      s <- paste0(s, matrix.string(output, spacing=4, left.justified=TRUE))
      loyalty.show.result(s)
    }
  }
})

observeEvent(input$loyaltyBrandOmissionDataFile, {
  rv$loyalty.brand.omission.data <- read.csv(
    input$loyaltyBrandOmissionDataFile$datapath, header = FALSE,
    strip.white = TRUE, stringsAsFactors = FALSE)
  shinyjs::enable("loyaltyBrandOmissionDownload")
})

observe({
  req(input$loyaltyBrandOmissionDataFileFormat)
  if (input$loyaltyBrandOmissionDataFileFormat == "Rows of subjects") {
    shinyjs::enable("loyaltyBrandOmissionDataFileFirst")
    shinyjs::enable("loyaltyBrandOmissionDataFileSecond")
  } else {
    shinyjs::disable("loyaltyBrandOmissionDataFileFirst")
    shinyjs::disable("loyaltyBrandOmissionDataFileSecond")
  }
})

output$loyaltyBrandOmissionDownload <- downloadHandler(
  filename = function() {
    b <- parse.group.string(input$loyaltyBrandOmissionWhich)
    if (length(b) != 1 || is.na(b)) {
      b <- 'none'
    } else {
      b <- b[[1]]
    }
    file.name <-
      tools::file_path_sans_ext(input$loyaltyBrandOmissionDataFile$name)
    file.extension <- tools::file_ext(input$loyaltyBrandOmissionDataFile$name)
    return(paste0(file.name, ' (omit ', paste(b, collapse=' '), ').',
                  file.extension))
  },
  content = function(file) {
    org.data <- rv$loyalty.brand.omission.data
    if (input$loyaltyBrandOmissionDataFileFormat == "Rows of subjects") {
      # Purchase columns?
      if (!string.is.positive.integer(input$loyaltyBrandOmissionDataFileFirst)) {
        showNotification("Oops! Invalid column for 1st purchase.", type = "message")
        return()
      }
      first <- as.numeric(input$loyaltyBrandOmissionDataFileFirst)
      if (first > nrow(org.data)) {
        showNotification("Oops! Invalid column for 1st purchase.", type = "message")
        return()
      }
      if (!string.is.positive.integer(input$loyaltyBrandOmissionDataFileSecond)) {
        showNotification("Oops! Invalid column for 2nd purchase", type = "message")
        return()
      }
      second <- as.numeric(input$loyaltyBrandOmissionDataFileSecond)
      if (second > nrow(org.data)) {
        showNotification("Oops! Invalid column for 2nd purchase.", type = "message")
        return()
      }
      if (first == second) {
        showNotification("Oops! Use different columns for 1st and 2nd purchases.",
                         type = "message")
        return()
      }
      first.col <- trimws(org.data[,first])
      second.col <- trimws(org.data[,second])
      # Omit brands?
      brand <- parse.group.string(input$loyaltyBrandOmissionWhich)
      if (length(brand) != 1 || is.na(brand)) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      brand <- brand[[1]]
      existing.brands <- unique(c(first.col, second.col))
      if (length(setdiff(brand, existing.brands)) > 0) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      is.omit <- (first.col %in% brand) | (second.col %in% brand)
      if (all(is.omit)) {
        showNotification("Oops! Cannot omit all brands", type = "message")
        return()
      }
      new.data <- org.data[!is.omit,]
    } else {
      # Omit brands?
      brand <- parse.group.string(input$loyaltyBrandOmissionWhich)
      if (length(brand) != 1 || is.na(brand)) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      brand <- brand[[1]]
      if (max(brand) > nrow(org.data)) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      if (length(brand) == nrow(org.data)) {
        showNotification("Oops! Cannot omit all brands.", type = "message")
        return()
      }
      new.data <- data.matrix(org.data)[-brand,-brand]
    }
    # Write to file
    write.table(new.data, file, quote = FALSE, sep = ",", row.names = FALSE,
                col.names = FALSE, qmethod = "double")
  }
)

observeEvent(input$loyaltyExplanatoryDataFile, {
  rv$loyalty.explanatory.data <- read.csv(
    input$loyaltyExplanatoryDataFile$datapath, header = FALSE,
    strip.white = TRUE, stringsAsFactors = FALSE)
})

observe({
  req(rv$loyalty.explanatory.data)
  shinyjs::enable("loyaltyExplanatoryDataDownload")
})

output$loyaltyExplanatoryDataDownload <- downloadHandler(
  filename = "loyalty_explanatory_data.csv",
  content = function(file) {
    org.data <- rv$loyalty.explanatory.data
    first.brand.column <- as.numeric(input$loyaltyExplanatoryVariableFirstBrandColumn)
    second.brand.column <- as.numeric(input$loyaltyExplanatoryVariableSecondBrandColumn)
    # check that brand columns are valid
    if (is.na(first.brand.column) | first.brand.column == 0 | first.brand.column > ncol(org.data)){
      showNotification("Oops! Invalid column for first brand.", type = "message")
      return()
    }
    if (is.na(second.brand.column) | second.brand.column == 0 | second.brand.column > ncol(org.data)){
      showNotification("Oops! Invalid column for second brand", type = "message")
      return()
    }
    
    
    first.brands <- parse.group.string(input$loyaltyExplanatoryTargetBrand1)
    if (length(first.brands) != 1 || is.na(first.brands)) {
      showNotification("Oops! Invalid input for 1st brand.", type = "message")
      return()
    }
    first.brands <- first.brands[[1]]
    if (max(first.brands) > max(org.data[,c(first.brand.column,second.brand.column)])) {
      showNotification("Oops! The 1st purchase brands exceed the number of brands in the file.", type = "message")
      return()
    }
    second.brands <- parse.group.string(input$loyaltyExplanatoryTargetBrand2)
    if (length(second.brands) != 1 || is.na(second.brands)) {
      showNotification("Oops! Invalid input for 2nd brand.", type = "message")
      return()
    }
    second.brands <- second.brands[[1]]
    if (max(second.brands) > max(org.data[,c(first.brand.column,second.brand.column)])) {
      showNotification("Oops! The 2nd purchase brands exceed the number of brands in the file.", type = "message")
      return()
    }
    # Create binary target variable
    binary.target.var <- rep(2, nrow(org.data))
    first.to.first.brands <- org.data[,first.brand.column] %in% first.brands
    second.to.second.brands <- org.data[,second.brand.column] %in% second.brands
    if (input$loyaltyExplanatoryTargetWith1 == "not with") {
      first.to.first.brands <- !first.to.first.brands
    }
    if (input$loyaltyExplanatoryTargetWith2 == "not with") {
      second.to.second.brands <- !second.to.second.brands
    }
    binary.target.var[first.to.first.brands & second.to.second.brands] <- 1
    new.data <- cbind(org.data, binary.target.var)
    # Write to file
    write.table(new.data, file, quote = FALSE, sep = ",", row.names = FALSE,
                col.names = FALSE, qmethod = "double")
  }
)

observeEvent(input$loyaltyExplanatorySort, {
  org.data <- rv$loyalty.explanatory.data
  first.brand.column <- as.numeric(input$loyaltyExplanatoryVariableFirstBrandColumn)
  second.brand.column <- as.numeric(input$loyaltyExplanatoryVariableSecondBrandColumn)
  # check that brand columns are valid
  if (is.na(first.brand.column) | first.brand.column == 0 | first.brand.column > ncol(org.data)){
    showNotification("Oops! Invalid column for first brand.", type = "message")
    return()
  }
  if (is.na(second.brand.column) | second.brand.column == 0 | second.brand.column > ncol(org.data)){
    showNotification("Oops! Invalid column for second brand", type = "message")
    return()
  }
  first.brands <- parse.group.string(input$loyaltyExplanatoryTargetBrand1)
  if (length(first.brands) != 1 || is.na(first.brands)) {
    showNotification("Oops! Invalid input for 1st brand.", type = "message")
    return()
  }
  first.brands <- first.brands[[1]]
  if (max(first.brands) > max(org.data[,c(first.brand.column,second.brand.column)])) {
    showNotification("Oops! The 1st purchase brands exceed the number of brands in the file.", type = "message")
    return()
  }
  second.brands <- parse.group.string(input$loyaltyExplanatoryTargetBrand2)
  if (length(second.brands) != 1 || is.na(second.brands)) {
    showNotification("Oops! Invalid input for 2nd brand.", type = "message")
    return()
  }
  second.brands <- second.brands[[1]]
  if (max(second.brands) > max(org.data[,c(first.brand.column,second.brand.column)])) {
    showNotification("Oops! The 2nd purchase brands exceed the number of brands in the file.", type = "message")
    return()
  }
  # Check that explanatory variable columns are valid
  explanatory.col <- parse.group.string(input$loyaltyExplanatoryVariableColumn)
  if (length(explanatory.col) == 0 || length(explanatory.col) == 1 && is.na(explanatory.col)) {
    showNotification("Oops! Invalid input for the explanatory column.", type = "message")
    return()
  }
  explanatory.col <- explanatory.col[[1]]
  if (max(explanatory.col) > ncol(org.data)) {
    showNotification("Oops! Explanatory column exceeds the number of columns in the data file.", type = "message")
    return()
  }
  if (any(explanatory.col %in% c(first.brand.column,second.brand.column))) {
    showNotification("Oops! Explanatory columns should not contain the brand columns 2 and 3.", type = "message")
    return()
  }
  # Create binary target variable
  binary.target.var <- rep(2, nrow(org.data))
  first.to.first.brands <- org.data[,first.brand.column] %in% first.brands
  second.to.second.brands <- org.data[,second.brand.column] %in% second.brands
  if (input$loyaltyExplanatoryTargetWith1 == "not with") {
    first.to.first.brands <- !first.to.first.brands
  }
  if (input$loyaltyExplanatoryTargetWith2 == "not with") {
    second.to.second.brands <- !second.to.second.brands
  }
  binary.target.var[first.to.first.brands & second.to.second.brands] <- 1
  if (length(unique(binary.target.var)) == 1) {
    showNotification("Oops! The target variable is not binary.", type = "message")
    return()
  }
  # Sort the explanatory variable columns
  new.data <- cbind(org.data, binary.target.var)
  explanatory.ordering <- threedimensional.sort.explanatory.variables(
    new.data, ncol(new.data), explanatory.col) # Target data is always the last column
  output$loyaltyExplanatoryDescriptiveness <- renderText({
    paste("Sorted by descriptiveness:", paste(explanatory.ordering, collapse=", "))})
})

# Ranking variable
rv$ranking.data <- NULL
rv$ranking.explanatory.data <- NULL
ranking.tab.counter <- 0

output$rankingData <- renderRHandsontable({
  req(rv$ranking.data)
  rhandsontable(rv$ranking.data, useTypes = FALSE) %>%
    hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
    hot_cols(colWidths = 80) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$rankingDataCreate, {
  updateTabsetPanel(session, "rankingTab", selected = "Data")
  if (!string.is.dimension(input$rankingDataDimension, 2)) {
    showModal(modalDialog(title = "Oops!", "Invalid data dimensions.",
                          easyClose = TRUE))
    return()
  }
  d <- parse.dimension.string(input$rankingDataDimension)
  if (d[1] < 2) {
    showModal(modalDialog(title = "Oops!", "There should be at least 2 brands.",
                          easyClose = TRUE))
    return()
  }
  if (d[2] < 2) {
    showModal(modalDialog(title = "Oops!", "There should be at least 2 satisfaction levels.",
                          easyClose = TRUE))
    return()
  }
  if (is.null(rv$ranking.data)) {
    row.name <- paste("Brand", 1:d[1])
    col.name <- paste("Level", 1:d[2])
    rv$ranking.data <- empty.data(d[1], d[2], row.name = row.name,
                                  col.name = col.name)
  } else if (!all(dim(rv$ranking.data) == d)) {
    showModal(modalDialog(title = "Confirmation", "Modify the data dimensions?",
                          footer = tagList(actionButton("rankingDataModifyOk", "OK"),
                                           actionButton("rankingDataModifyCancel", "Cancel"))))
  }
})

observeEvent(input$rankingDataModifyOk, {
  d <- parse.dimension.string(input$rankingDataDimension)
  row.name <- paste("Brand", 1:d[1])
  col.name <- paste("Level", 1:d[2])
  df <- empty.data(d[1], d[2], row.name = row.name, col.name = col.name)
  row <- min(nrow(rv$ranking.data), d[1])
  col <- min(ncol(rv$ranking.data), d[2])
  df[1:row,1:col] <- hot_to_r(input$rankingData)[1:row,1:col]
  rv$ranking.data <- df
  removeModal()
})

observeEvent(input$rankingDataModifyCancel, {
  updateTextInput(session, "rankingDataDimension",
                  value = paste(dim(rv$ranking.data), collapse = ","))
  removeModal()
})

observeEvent(input$rankingDataFile, {
  showModal(modalDialog(title = "File Information",
                        radioButtons("rankingDataFileFormat",
                                     label = "Data format",
                                     choices = list("Rows of subjects", "Contingency table")),
                        textInput("rankingDataFileRow", label = "Which column in file for brand?"),
                        textInput("rankingDataFileColumn", label = "Which column in file for satisfaction level?"),
                        selectInput("rankingDataFileSatisfactionConstraint", label = "Which satisfaction level cannot be collapsed over?", choices = c("None", "First", "Last")),
                        footer = tagList(actionButton("rankingDataFileOk", "OK"),
                                         modalButton("Cancel"))))
})

observe({
  req(input$rankingDataFileFormat)
  if (input$rankingDataFileFormat == "Rows of subjects") {
    shinyjs::enable("rankingDataFileRow")
    shinyjs::enable("rankingDataFileColumn")
  } else {
    shinyjs::disable("rankingDataFileRow")
    shinyjs::disable("rankingDataFileColumn")
  }
})

observeEvent(input$rankingDataFileOk, {
  if (input$rankingDataFileFormat == "Rows of subjects") {
    if (!string.is.positive.integer(input$rankingDataFileRow)) {
      showNotification("Oops! Invalid row number.", type = "message")
      return()
    }
    if (!string.is.positive.integer(input$rankingDataFileColumn)) {
      showNotification("Oops! Invalid column number.", type = "message")
      return()
    }
    row <- as.numeric(input$rankingDataFileRow)
    col <- as.numeric(input$rankingDataFileColumn)
    satisfaction.constraint = input$rankingDataFileSatisfactionConstraint
    
    if (row == col) {
      showNotification("Oops! Use different columns for brand and satisfaction levels.",
                       type = "message")
      return()
    }
    df <- read.csv(input$rankingDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (max(row, col) > ncol(df)) {
      showNotification(paste("Oops! Only", ncol(df), "columns in the file."),
                       type = "message")
      return()
    }
    m <- table.data(table(df[,c(row, col)]))
    if (nrow(m) < 2) {
      showNotification(paste("Oops! The data should be at least 2 brands."),
                       type = "message")
      return()
    }
    if (ncol(m) < 2) {
      showNotification(paste("Oops! The data should be at least 2 satisfaction levels."),
                       type = "message")
      return()
    }
    dimnames(m) <- list(paste("Brand", 1:nrow(m)), paste("Level", 1:ncol(m)))
    rv$ranking.data <- m
    removeModal()
  } else {
    df <- read.csv(input$rankingDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    rv$ranking.data <- matrix.data(data.matrix(df),
                                   row.name = paste("Brand", 1:nrow(df)),
                                   col.name = paste("Level", 1:ncol(df)))
    removeModal()
  }
  updateTabsetPanel(session, "rankingTab", selected = "Data")
})

observeEvent(input$rankingOmitBrand, {
  updateTabsetPanel(session, "rankingTab", selected = "Data")
  brand <- parse.group.string(input$rankingBrandOmission)
  if (length(brand) != 1 || is.na(brand)) {
    showModal(modalDialog(title = "Oops!", "Invalid brands.",
                          easyClose = TRUE))
    return()
  }
  brand <- brand[[1]]
  data <- hot_to_r(input$rankingData)
  if (max(brand) > nrow(data)) {
    showModal(modalDialog(title = "Oops!", "Invalid brands.",
                          easyClose = TRUE))
    return()
  }
  if (length(brand) == nrow(data)) {
    showModal(modalDialog(title = "Oops!", "Cannot omit all brands.",
                          easyClose = TRUE))
    return()
  }
  showModal(modalDialog(title = "Confirmation", "Omit the brands?",
                        footer = tagList(actionButton("rankingOmitBrandOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$rankingOmitBrandOk, {
  brand <- parse.group.string(input$rankingBrandOmission)[[1]]
  data <- data.numeric.matrix(hot_to_r(input$rankingData))[-brand,]
  if (is.vector(data)) {
    num.brand <- 1
    num.level <- length(data)
  } else {
    num.brand <- nrow(data)
    num.level <- ncol(data)
  }
  dim(data) <- c(num.brand, num.level)
  brand.name <- paste("Brand", seq_len(num.brand))
  level.name <- paste("Level", seq_len(num.level))
  rv$ranking.data <- matrix.data(data, brand.name, level.name)
  removeModal()
})

observeEvent(input$rankingDataCollapse, {
  updateTabsetPanel(session, "rankingTab", selected = "Data")
  cols <- parse.group.string(input$rankingDataCollapseColumn)
  if (length(cols) == 0) {
    return()
  }
  if (length(cols) == 1 && is.na(cols) ||
      length(cols) > 0 && max(unlist(cols)) > ncol(hot_to_r(input$rankingData))) {
    showModal(modalDialog(title = "Oops!", "Invalid satisfaction level groups.",
                          easyClose = TRUE))
    return()
  }
  if (!all(is.consecutive.group(cols))) {
    showModal(modalDialog(title = "Oops!", "Each satisfaction level group should be consecutive.",
                          easyClose = TRUE))
    return()
  }
  if (any(is.na(suppressWarnings(data.numeric.matrix(hot_to_r(input$rankingData)))))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  m <- collapse.data(data.numeric.matrix(hot_to_r(input$rankingData)), row.group = NULL, cols, TRUE)
  if (ncol(m) < 2) {
    showModal(modalDialog(title = "Oops!", "Collapsed data should have more than 1 satisfaction level.",
                          easyClose = TRUE))
    return()
  }
  showModal(modalDialog(title = "Confirmation", "Collapse the data?",
                        footer = tagList(actionButton("rankingDataCollapseOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$rankingDataCollapseOk, {
  cols <- parse.group.string(input$rankingDataCollapseColumn)
  m <- collapse.data(data.numeric.matrix(hot_to_r(input$rankingData)), row.group = NULL, cols, TRUE)
  dimnames(m) <- list(paste("Brand", 1:nrow(m)), paste("Level", 1:ncol(m)))
  rv$ranking.data <- m
  removeModal()
})

output$rankingDataDownload <- downloadHandler(
  filename = "ranking_data.csv",
  content = function(file) {
    write.table(hot_to_r(input$rankingData), file, quote = FALSE,
                sep = ",", row.names = FALSE, col.names = FALSE,
                qmethod = "double")
  }
)

observeEvent(input$rankingDataClear, {
  showModal(modalDialog(title = "Confirmation", "Clear the data?",
                        footer = tagList(actionButton("rankingDataClearOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$rankingDataClearOk, {
  updateTabsetPanel(session, "rankingTab", selected = "Data")
  rv$ranking.data <- NULL
  removeModal()
})

observe({
  if (is.null(rv$ranking.data)) {
    updateTextInput(session, "rankingDataDimension", value = "")
    updateActionButton(session, "rankingDataCreate", label = "Create")
    updateTextInput(session, "rankingDataCollapseColumn", value = "")
    shinyjs::disable("rankingOmitBrand")
    shinyjs::disable("rankingDataCollapse")
    shinyjs::disable("rankingDataDownload")
    shinyjs::disable("rankingDataClear")
  } else {
    updateTextInput(session, "rankingDataDimension",
                    value = paste(dim(rv$ranking.data), collapse = ","))
    updateActionButton(session, "rankingDataCreate", label = "Modify")
    shinyjs::enable("rankingOmitBrand")
    shinyjs::enable("rankingDataCollapse")
    shinyjs::enable("rankingDataDownload")
    shinyjs::enable("rankingDataClear")
  }
})

ranking.show.result <- function(s) {
  x <- ranking.tab.counter <<- ranking.tab.counter + 1
  text.id <- paste0("rankingResult", x)
  
  appendTab("rankingTab",
            tabPanel(paste("Result", x),
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(),
                     actionButton(paste0("rankingResultClose", x),
                                  label = "Close"),
                     br(), br()),
            select = TRUE)
  
  output[[text.id]] <- renderText({s})
  
  code <- gsub('%x', x,
               'observeEvent(input$rankingResultClose%x, {
        removeTab("rankingTab", "Result %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

ranking.show.plot1 <- function(avg.satisfaction, max.satisfaction.level) {
  x <- ranking.tab.counter <<- ranking.tab.counter + 1
  plot.id <- paste0("rankingPlot", x)
  
  appendTab("rankingTab",
            tabPanel(paste("Plot", x),
                     br(), plotOutput(plot.id),
                     actionButton(paste0("rankingPlotClose", x),
                                  label = "Close"),
                     br(), br()))
  
  # Plot average satisfaction levels.
  num.brand <- length(avg.satisfaction)
  rank.by.avg <- order(avg.satisfaction, decreasing=TRUE)
  temp.df <- data.frame(brand=seq_len(num.brand), score=avg.satisfaction)
  temp.df <- within(temp.df, brand <- factor(brand, levels=rank.by.avg))
  
  output[[plot.id]] <- renderPlot(
    ggplot(data=temp.df, aes(x=brand, y=score)) +
      xlab("Brand") +
      ylab("Average Satisfaction") +
      ggtitle("Ranking by Satisfaction") +
      geom_bar(stat="identity", fill="#0072B2") +
      ylim(0, max.satisfaction.level) +
      theme_grey(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5)))
  
  code <- gsub('%x', x,
               'observeEvent(input$rankingPlotClose%x, {
        removeTab("rankingTab", "Plot %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

ranking.show.plot2 <- function(brand.cluster, satisfaction.partition,
                               collapsed.pmf, stochastic.ordering) {
  
  x <- ranking.tab.counter <<- ranking.tab.counter + 1
  plot.id <- paste0("rankingPlot", x)
  
  appendTab("rankingTab",
            tabPanel(paste("Plot", x),
                     br(), plotOutput(plot.id),
                     actionButton(paste0("rankingPlotClose", x),
                                  label = "Close"),
                     br(), br()))
  
  num.brand.group <- nrow(collapsed.pmf)
  num.level <- ncol(collapsed.pmf)
  temp.df <- data.frame(
    brand.group=rep(seq_len(num.brand.group), each=num.level),
    satisfaction.level=rep(seq_len(num.level), num.brand.group),
    pmf=as.vector(t(collapsed.pmf)))
  if (is.null(stochastic.ordering)) {
    temp.df <- within(temp.df, brand.group <- factor(brand.group,
                                                     levels=1:num.brand.group))
  } else {
    temp.df <- within(temp.df, brand.group <- factor(brand.group,
                                                     levels=stochastic.ordering))
  }
  temp.df <- within(temp.df, satisfaction.level <- factor(satisfaction.level))
  # Brand group labels.
  for (i in seq_len(num.brand.group)) {
    levels(temp.df$brand.group)[levels(temp.df$brand.group)==i] <-
      paste("Brand", paste(which(brand.cluster==i), collapse=", "))
  }
  # Combined satisfaction level labels.
  satisfaction.label <- numeric(num.level)
  for (i in seq_len(num.level)) {
    subset.range <- range(which(satisfaction.partition==i))
    if(subset.range[1] == subset.range[2]) {
      satisfaction.label[i] <- as.character(subset.range[1])
    } else {
      satisfaction.label[i] <- paste0(subset.range[1], "-",
                                      subset.range[2])
    }
  }
  # Plot it.
  custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                      "#D55E00", "#CC79A7")
  gg.obj <- ggplot(data=temp.df, aes(satisfaction.level, pmf,
                                     fill=satisfaction.level)) +
    xlab("Combined Satisfaction Levels") +
    ylab("Probability") + 
    ggtitle("Ranking of Brand Groups") +
    scale_x_discrete(breaks=NULL) +
    expand_limits(y=0) +
    facet_wrap( ~ brand.group, ncol=num.brand.group) +
    geom_bar(stat="identity") +
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank()) +
    theme(panel.grid.minor.y=element_blank()) 
  if (length(satisfaction.label) <= length(custom_palette)) {
    gg.obj <- gg.obj + 
      scale_fill_manual(values=custom_palette,
                        name="Satisfaction\nLevels", labels=satisfaction.label)
  } else {
    gg.obj <- gg.obj + 
      scale_fill_discrete(name="Satisfaction\nLevels",
                          labels=satisfaction.label)
  }
  gg.obj <- gg.obj + theme_grey(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5))
  
  output[[plot.id]] <- renderPlot(gg.obj)
  
  code <- gsub('%x', x,
               'observeEvent(input$rankingPlotClose%x, {
        removeTab("rankingTab", "Plot %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

observeEvent(input$rankingGo, {
  if (input$rankingModelName == "Brand Omission") {
    showModal(modalDialog(title = "Ranking Analysis Brand Omission",
                          helpText('Step 1: Import Data'),
                          fileInput("rankingBrandOmissionDataFile", label = NULL,
                                    accept = c(".csv")),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 2: Data Format'),
                          radioButtons("rankingBrandOmissionDataFileFormat",
                                       label=NULL,
                                       choices=list("Rows of subjects", "Contingency table")),
                          textInput("rankingBrandOmissionDataFileBrand",
                                    label="Which column in file for brands?"),
                          hr(style="border-color: lightgray;"),
                          helpText('Step 3: Omit Brand(s)'),
                          textInput("rankingBrandOmissionWhich",
                                    label="Which brand(s) to be omitted?"),       
                          hr(style="border-color: lightgray;"),
                          helpText('Step 4: Export Data'),
                          shinyjs::disabled(downloadButton("rankingBrandOmissionDownload", label = "Download")),
                          footer = tagList(modalButton("Close"))))
  } else if (input$rankingModelName == "Explanatory Variable") {
    
    showModal(
      modalDialog(title = "Ranking Analysis Explanatory Variable",
                  helpText('Step 1: Import Data'),
                  fileInput("rankingExplanatoryDataFile", label = NULL,
                            accept = c(".csv")),
                  helpText('Step 2: Determine brand and satisfaction columns'),
                  hr(style="border-color: lightgray;"),
                  textInput('rankingExplanatoryVariableBrandColumn',
                            label = "Which column for brand?"),
                  textInput('rankingExplanatoryVariableSatisfactionColumn',
                            label = "Which column for satisfaction?"),
                  helpText('Step 3: Create Target Variable'),
                  textInput("rankingExplanatoryVariableBrand",
                            label = "Which brands?"),
                  textInput("rankingExplanatoryVariableSatisfaction",
                            label = "Which satisfaction levels?"),
                  hr(style="border-color: lightgray;"),
                  helpText('Step 4: Export Data With Target Variable'),
                  shinyjs::disabled(downloadButton("rankingExplanatoryDataDownload", label = "Download")),
                  hr(style="border-color: lightgray;"),
                  helpText('Step 4: Sort Explanatory Variables By Descriptiveness'),
                  textInput("rankingExplanatoryVariableColumn",
                            label = "Which explanatory columns in file?"),
                  actionButton("rankingExplanatorySort", label = "Sort!"),
                  br(),
                  br(),
                  verbatimTextOutput("rankingExplanatoryDescriptiveness"),
                  footer = tagList(modalButton("Close"))))
  } else {
    # Data is clean?
    if (is.null(rv$ranking.data)) {
      showModal(modalDialog(title = "Oops!", "There is no data.",
                            easyClose = TRUE))
      return()
    }
    m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$rankingData)))
    if (any(is.na(m))) {
      showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                            easyClose = TRUE))
      return()
    }
    if (any(m < 0)) {
      showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                            easyClose = TRUE))
      return()
    }
    if (input$rankingModelName %in% c("Exploratory", "Confirmatory")) {
      upper.polarity.idx <- parse.group.string(input$rankingPolarityIndex)
      if (length(upper.polarity.idx) == 0) {
        upper.polarity.idx <- NULL
      } else if (length(upper.polarity.idx) > 1 || is.na(upper.polarity.idx)) {
        showModal(modalDialog(title = "Oops!", "Invalid customized polarity index.",
                              easyClose = TRUE))
        return()
      } else {
        upper.polarity.idx <- unlist(upper.polarity.idx)
        min.upper.idx <- min(upper.polarity.idx)
        if (min.upper.idx == 1) {
          showModal(modalDialog(title = "Oops!", "Customized polarity index should not start at 1.",
                                easyClose = TRUE))
          return()
        }
        num.level <- ncol(m)
        if (max(upper.polarity.idx) != num.level) {
          showModal(modalDialog(title = "Oops!", "Customized polarity index should end at the highest level.",
                                easyClose = TRUE))
          return()
        }
        if (length(upper.polarity.idx) != num.level - min.upper.idx + 1) {
          showModal(modalDialog(title = "Oops!", "Customized polarity index should be consecutive.",
                                easyClose = TRUE))
          return()
        }
      }
    }
    if (input$rankingModelName == "Exploratory") {
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      satisfaction.constraint = input$rankingDataFileSatisfactionConstraint
      if (satisfaction.constraint == "None"){
        satisfaction.constraint = NULL
      }
      else if (satisfaction.constraint == "First"){
        satisfaction.constraint = 1
      }
      else if (satisfaction.constraint == "Last"){
        satisfaction.constraint = ncol(contingency.table)
      }
      # Perform the computation

      result <- ranking.exploratory.computation(contingency.table,
                                                upper.polarity.idx=upper.polarity.idx)
      
      if (!is.null(satisfaction.constraint)){
        satisfaction.constraint = unlist(satisfaction.constraint)
        omitted.contingency.table = contingency.table[,-satisfaction.constraint]
        result.omitted <- ranking.exploratory.computation(omitted.contingency.table,
                                                          upper.polarity.idx = upper.polarity.idx)
        omitted.contingency.table.collapsed.rows = result.omitted$collapsed.contingency.table 
        omitted.category.frequencies = contingency.table[,satisfaction.constraint]
        omitted.category.frequencies.collapsed = c()
        brand.clusters = result.omitted$brand.cluster
        for (i in unique(brand.clusters)){
          omitted.category.frequencies.collapsed[i] = sum(omitted.category.frequencies[which(brand.clusters == i)])
        }
        satisfaction.partition = result.omitted$satisfaction.partition
        m = matrix(nrow = length(unique(brand.clusters)),
                   ncol = length(unique(satisfaction.partition)))
        
        rowSums_with_one <- function(x){
          if (is.null(ncol(x))){
            return(x)
          }
          else{
            return(rowSums(x))
          }
        }
        for (i in unique(satisfaction.partition)){
          m[,i] = rowSums_with_one(omitted.contingency.table.collapsed.rows[,which(satisfaction.partition==i)])
        }
        
      }
      if (!is.null(satisfaction.constraint)){
      if (satisfaction.constraint == 1){
        m.with.constraint = matrix(cbind( omitted.category.frequencies.collapsed ,
                                          m ),
                   nrow  = (length(unique(brand.clusters)) ))
      }
      else {
        m.with.constraint = matrix(cbind(  m,
                                           omitted.category.frequencies.collapsed),
                                   nrow = (length(unique(brand.clusters)) ))
      }
      pmf.without.omitted.category = m/rowSums(m)
      pmf.with.omitted.category = m.with.constraint/rowSums(m.with.constraint)
      s <- ""
      s <- paste0(s, "Exploratory Model Result\n")
      s <- paste0(s, "========================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(result.omitted$observed,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "L-Square= ", round(result.omitted$full.l.sq.stat,
                                         2), "  D.O.F.= ", result.omitted$full.d.o.f, "  p-value= ",
                  signif(result.omitted$full.p.value, 4), "\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Final Ranking with Stochastic Ordering\n")
      s <- paste0(s, "--------------------------------------\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Ranking procedure with omission of satisfaction category: ", satisfaction.constraint, "\n")
      s <- paste0(s, "--------------------------------------\n")
      s <- paste0(s, "Critical value= ", round(result.omitted$critical.value, 2), "\n")
      s <- paste0(s, "\n")
      num.cluster <- nrow(result.omitted$collapsed.pmf)
      num.collapsed.level <- ncol(result.omitted$collapsed.pmf)
      temp <- matrix("", 2 + num.cluster, 1 + num.collapsed.level + 1)
      temp[1,1] <- "Brands"
      temp[2,1] <- "======"
      add.one = satisfaction.constraint == 1 
      for (i in seq_len(num.collapsed.level)) {
        subset.range <- range(which(result.omitted$satisfaction.partition==i))
        if (subset.range[1] == subset.range[2]) {
          temp[1,1+i] <- subset.range[1] + add.one
        } else {
          temp[1,1+i] <- paste0(subset.range[1] + add.one, "-", subset.range[2]+add.one)
        }
        temp[2,1+i] <- paste(rep("=", max(nchar(temp[1,1+i]),2)), collapse="")
      }
      temp[1,1+num.collapsed.level+1] <- "Polarity Index"
      temp[2,1+num.collapsed.level+1] <- "=============="
      for (i in seq(num.cluster)) {
        label <- result.omitted$stochastic.ordering[i]
        temp[2+i,1] <- paste(which(result.omitted$brand.cluster==label), collapse=",")
        for (j in seq_len(num.collapsed.level)) {
          temp[2+i,1+j] <- round(result.omitted$collapsed.pmf[label,j], 2)
        }
        temp[2+i,1+num.collapsed.level+1] <-
          round(result.omitted$polarity.index[label], 2)
      }
      s <- paste0(s, matrix.string(temp, spacing=4))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Information Loss\n")
      s <- paste0(s, "----------------\n")
      s <- paste0(s, paste0("L^2(N) - L^2(Mk) = ",
                            round(result.omitted$change.in.l.sq, 2)))
      s <- paste0(s, "\n")
      s <- paste0(s, paste0("Information loss D.O.F. = ",
                            result.omitted$change.in.d.o.f))
      s <- paste0(s, "\n")
      s <- paste0(s, paste0("Information p-value = ",
                            signif(result.omitted$information.p.value, 4)))
      s <- paste0(s, "\n")
      if (result.omitted$information.is.lost) {
        s <- paste0(s, "Collapsing leads to information loss.\n")
      } else {
        s <- paste0(s, "Collapsing does not lead to information loss.\n")
      }
      s <- paste0(s, "\n")
      
      s <- paste0(s, "Distributions with the omitted category added\nafter the stochastic ordering was obtained", "\n")
      temp <- matrix("", 2 + num.cluster, 2 + num.collapsed.level)
      temp[1,1] <- "Brands"
      temp[2,1] <- "======"
      add.one = satisfaction.constraint == 1 
      if (add.one){
        temp[1,2] = 1
        temp[2,2] = "=="
      }
      for (i in seq_len(num.collapsed.level)) {
        subset.range <- range(which(result.omitted$satisfaction.partition==i))
        if (subset.range[1] == subset.range[2]) {
          temp[1,1+i + add.one] <- subset.range[1] + add.one
        } else {
          temp[1,1+i + add.one] <- paste0(subset.range[1] + add.one, "-", subset.range[2]+add.one)
        }
        temp[2,1+i + add.one] <- paste(rep("=", max(nchar(temp[1,1+i]),2)), collapse="")
      }
      if (!add.one){
        temp[1,num.collapsed.level+2] = satisfaction.constraint
        temp[2,num.collapsed.level+2] = "=="
      }


      for (i in seq(num.cluster)) {
        label <- result.omitted$stochastic.ordering[i]
        temp[2+i,1] <- paste(which(result.omitted$brand.cluster==label), collapse=",")
        
        for (j in seq_len(num.collapsed.level + 1)) {
          temp[2+i,1+j] <- round(pmf.with.omitted.category[label,j], 2)
        }

      }
      s <- paste0(s, matrix.string(temp, spacing=4))
      
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Z-test for stochastic ordering \n")
      s <- paste0(s, "--------------------------------------\n")
      mid.category = ceiling(ncol(contingency.table)/2)
      satisfaction.partition = result.omitted$satisfaction.partition
      mid.category.partition = satisfaction.partition[mid.category]
      contingency.table.collapsed.rows = result.omitted$collapsed.contingency.table.rows
      rowSums_with_one <- function(x){
        if (is.null(ncol(x))){
          return(x)
        }
        else{
          return(rowSums(x))
        }
      }
      bottom.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition <= mid.category.partition)])
      top.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition > mid.category.partition)])

      ranking.matrix <- temp
      temp <- matrix("", 2 + num.cluster, 1 + num.cluster +1)
      temp[1,1] <- "Brands"
      for (j in 1:ncol(temp)){
        temp[2,j] <- "----"
      }
      for (i in 1:nrow(temp)){
        temp[i,2] <- "|"
      }
      for (i in 1:num.cluster){
        temp[i+2,1] <- ranking.matrix[i+2,1]
        temp[1, i+2]<- ranking.matrix[i+2,1]
        temp[i+2, i+2] <- "\\"
      }
      for (i in 1:length(bottom.half)){
        for (j in min((i+1), length(bottom.half)): length(bottom.half)){
          bottom_i = bottom.half[i]
          top_i = top.half[i]
          bottom_j = bottom.half[j]
          top_j = top.half[j]
          z_ij =  ( log(top_i/bottom_i) - log(top_j/bottom_j))/ (1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
          if (!is.na(z_ij)){
          if (z_ij > qnorm(0.95)){
            temp[i+2, j+2] = "X"
          }
          }
          
        }
      }
      s <- paste0(s, matrix.string(temp, spacing=4))
      
      ranking.show.result(s)
      
      ranking.show.plot1(result.omitted$avg.satisfaction, ncol(contingency.table))
      ranking.show.plot2(result.omitted$brand.cluster,
                         result.omitted$satisfaction.partition, result.omitted$collapsed.pmf,
                         result.omitted$stochastic.ordering)
      
      }
      if (is.null(satisfaction.constraint)){
      s <- ""
      s <- paste0(s, "Exploratory Model Result\n")
      s <- paste0(s, "========================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "L-Square= ", round(result$full.l.sq.stat,
                                         2), "  D.O.F.= ", result$full.d.o.f, "  p-value= ",
                  signif(result$full.p.value, 4), "\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Final Ranking with Stochastic Ordering\n")
      s <- paste0(s, "--------------------------------------\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Critical value= ", round(result$critical.value, 2), "\n")
      s <- paste0(s, "\n")
      num.cluster <- nrow(result$collapsed.pmf)
      num.collapsed.level <- ncol(result$collapsed.pmf)
      temp <- matrix("", 2 + num.cluster, 1 + num.collapsed.level + 1)
      temp[1,1] <- "Brands"
      temp[2,1] <- "======"
      for (i in seq_len(num.collapsed.level)) {
        subset.range <- range(which(result$satisfaction.partition==i))
        if (subset.range[1] == subset.range[2]) {
          temp[1,1+i] <- subset.range[1]
        } else {
          temp[1,1+i] <- paste0(subset.range[1], "-", subset.range[2])
        }
        temp[2,1+i] <- paste(rep("=", max(nchar(temp[1,1+i]),2)), collapse="")
      }
      temp[1,1+num.collapsed.level+1] <- "Polarity Index"
      temp[2,1+num.collapsed.level+1] <- "=============="
      for (i in seq(num.cluster)) {
        label <- result$stochastic.ordering[i]
        temp[2+i,1] <- paste(which(result$brand.cluster==label), collapse=",")
        for (j in seq_len(num.collapsed.level)) {
          temp[2+i,1+j] <- round(result$collapsed.pmf[label,j], 2)
        }
        temp[2+i,1+num.collapsed.level+1] <-
          round(result$polarity.index[label], 2)
      }
      s <- paste0(s, matrix.string(temp, spacing=4))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Information Loss\n")
      s <- paste0(s, "----------------\n")
      s <- paste0(s, paste0("L^2(N) - L^2(Mk) = ",
                            round(result$change.in.l.sq, 2)))
      s <- paste0(s, "\n")
      s <- paste0(s, paste0("Information loss D.O.F. = ",
                            result$change.in.d.o.f))
      s <- paste0(s, "\n")
      s <- paste0(s, paste0("Information p-value = ",
                            signif(result$information.p.value, 4)))
      s <- paste0(s, "\n")
      if (result$information.is.lost) {
        s <- paste0(s, "Collapsing leads to information loss.\n")
      } else {
        s <- paste0(s, "Collapsing does not lead to information loss.\n")
      }
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Z-test for stochastic ordering \n")
      s <- paste0(s, "--------------------------------------\n")
      mid.category = ceiling(ncol(contingency.table)/2)
      satisfaction.partition = result$satisfaction.partition
      mid.category.partition = satisfaction.partition[mid.category]
      contingency.table.collapsed.rows = result$collapsed.contingency.table.rows
      rowSums_with_one <- function(x){
        if (is.null(ncol(x))){
          return(x)
        }
        else{
          return(rowSums(x))
        }
      }
      bottom.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition <= mid.category.partition)])
      top.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition > mid.category.partition)])
      ranking.matrix <- temp
      temp <- matrix("", 2 + num.cluster, 1 + num.cluster +1)
      temp[1,1] <- "Brands"
      for (j in 1:ncol(temp)){
        temp[2,j] <- "----"
      }
      for (i in 1:nrow(temp)){
        temp[i,2] <- "|"
      }
      for (i in 1:num.cluster){
        temp[i+2,1] <- ranking.matrix[i+2,1]
        temp[1, i+2]<- ranking.matrix[i+2,1]
        temp[i+2, i+2] <- "\\"
      }
      for (i in 1:length(bottom.half)){
        for (j in min((i+1), length(bottom.half)): length(bottom.half)){
          bottom_i = bottom.half[i]
          top_i = top.half[i]
          bottom_j = bottom.half[j]
          top_j = top.half[j]
          z_ij =  ( log(top_i/bottom_i) - log(top_j/bottom_j))/ (1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
          if (z_ij > qnorm(0.95)){
            temp[i+2, j+2] = "X"
          }
          
        }
      }
      s <- paste0(s, matrix.string(temp, spacing=4))

      ranking.show.result(s)
      ranking.show.plot1(result$avg.satisfaction, ncol(contingency.table))
      ranking.show.plot2(result$brand.cluster,
                         result$satisfaction.partition, result$collapsed.pmf,
                         result$stochastic.ordering)}
    } else if (input$rankingModelName == "Confirmatory") {
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      satisfaction.constraint = input$rankingDataFileSatisfactionConstraint
      if (satisfaction.constraint == "None"){
        satisfaction.constraint = NULL
      }
      else if (satisfaction.constraint == "First"){
        satisfaction.constraint = 1
      }
      else if (satisfaction.constraint == "Last"){
        satisfaction.constraint = ncol(contingency.table)
      }
      
      if (!is.null(satisfaction.constraint)){
        satisfaction.constraint = unlist(satisfaction.constraint)
        omitted.contingency.table = contingency.table[,-satisfaction.constraint]
        result.omitted <- ranking.confirmatory.computation(omitted.contingency.table,
                                                          upper.polarity.idx = upper.polarity.idx)
        omitted.contingency.table.collapsed = result.omitted$collapsed.exp.spacing.result$observed
        omitted.category.frequencies = contingency.table[,satisfaction.constraint]
        omitted.category.frequencies.collapsed = c()
        brand.clusters = result.omitted$brand.cluster
        for (i in unique(brand.clusters)){
          omitted.category.frequencies.collapsed[i] = sum(omitted.category.frequencies[which(brand.clusters == i)])
        }

        if (satisfaction.constraint == 1){
          omitted.contingency.table.collapsed.with.constraint <-
            matrix(cbind(omitted.category.frequencies.collapsed,
                         omitted.contingency.table.collapsed),
                   nrow = length(unique(brand.clusters)))
        }
        
        else{
         omitted.contingency.table.collapsed.with.constraint <-
           matrix(cbind(omitted.contingency.table.collapsed,
                        omitted.category.frequencies.collapsed),
                  nrow = length(unique(brand.clusters)))
        }
        m <- omitted.contingency.table.collapsed.with.constraint
        pmf.with.constraint <- m/rowSums(m)
        s <- ""
        s <- paste0(s, "Omitted satisfaction level ", satisfaction.constraint, "\n")
        s <- paste0(s, "Confirmatory Model Result\n")
        s <- paste0(s, "=========================\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Observed Data Matrix\n")
        s <- paste0(s, "--------------------\n")
        s <- paste0(s, matrix.string(round(original.contingency.table[,-satisfaction.constraint],
                                           2), left.justified=FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Exponential Spacing Model Results on Original Data\n")
        s <- paste0(s, "==================================================\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Expected Data Matrix\n")
        s <- paste0(s, "--------------------\n")
        s <- paste0(s, matrix.string(round(
          result.omitted$org.exp.spacing.result$expected, 2),
          left.justified=FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "TAU(Y|X)= ", signif(result.omitted$org.exp.spacing.result$tau.given.row, 4),
                    "  TAU(X|Y)= ", signif(result.omitted$org.exp.spacing.result$tau.given.col, 4), "\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "X-Square= ", round(result.omitted$org.exp.spacing.result$chi.sq.stat, 2),
                    "  L-Square= ", round(result.omitted$org.exp.spacing.result$l.sq.stat, 2),
                    "  D.O.F.= ", result.omitted$org.exp.spacing.result$d.o.f,
                    "  p-value= ", signif(result.omitted$org.exp.spacing.result$p.value, 4), "\n")
        s <- paste0(s, "\n")
        s <- paste0(s, "Polynomial Degree for Rows: ", result.omitted$org.exp.spacing.result$poly.deg.row, "\n")
        s <- paste0(s, "Polynomial Degree for Columns: ", result.omitted$org.exp.spacing.result$poly.deg.col, "\n")
        s <- paste0(s, "\n")
        num.row <- length(result.omitted$org.exp.spacing.result$mu)
        num.col <- length(result.omitted$org.exp.spacing.result$nu)
        temp <- matrix("", 2 + num.row + 1 + num.col + 2, 3)
        temp[1,1] <- "Parameter"
        temp[2,1] <- "========="
        temp[1,2] <- "Estimate"
        temp[2,2] <- "========"
        temp[1,3] <- "E.A.S.D."
        temp[2,3] <- "========"
        for (i in seq_len(num.row)) {
          temp[2+i,1] <- paste0("Mu[", i, "]")
          temp[2+i,2] <- round(result.omitted$org.exp.spacing.result$mu[i], 2)
          if (is.na(result.omitted$org.exp.spacing.result$mu.easd[i])) {
            temp[2+i,3] <- "NA"
          } else {
            temp[2+i,3] <- round(result.omitted$org.exp.spacing.result$mu.easd[i], 2)
          }
        }
        add.one = satisfaction.constraint==1
        for (i in seq_len(num.col)) {
          temp[2+1+num.row+i,1] <- paste0("Nu[", i + add.one, "]")
          temp[2+1+num.row+i,2] <- round(result.omitted$org.exp.spacing.result$nu[i], 2)
          if (is.na(result.omitted$org.exp.spacing.result$nu.easd[i])) {
            temp[2+1+num.row+i,3] <- "NA"
          } else {
            temp[2+1+num.row+i,3] <- round(result.omitted$org.exp.spacing.result$nu.easd[i], 2)
          }
        }
        
        temp[2+1+num.row+1+num.col+1,1] <- "Phi"
        temp[2+1+num.row+1+num.col+1,2] <- round(result.omitted$org.exp.spacing.result$phi, 2)
        if (is.na(result.omitted$org.exp.spacing.result$phi.easd)) {
          temp[2+1+num.row+1+num.col+1,3] <- "NA"
        } else {
          temp[2+1+num.row+1+num.col+1,3] <- round(result.omitted$org.exp.spacing.result$phi.easd, 2)
        }
        s <- paste0(s, matrix.string(temp, spacing=2))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Model Diagnostics\n")
        s <- paste0(s, "-----------------\n")
        if (result.omitted$org.exp.spacing.result$model.is.fit) {
          s <- paste0(s, "Model fits the observed data.\n")
        } else {
          s <- paste0(s, "Model does not fit the observed data.\n")
        }
        s <- paste0(s, "\n")
        s <- paste0(s, "Standardized Residuals\n")
        s <- paste0(s, "----------------------\n")
        s <- paste0(s, matrix.string(round(result.omitted$org.exp.spacing.result$std.residuals,
                                           2), left.justified=FALSE))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Number of significant residuals = ",
                    result.omitted$org.exp.spacing.result$num.sig.residuals, "\n")
        s <- paste0(s, "\n")
        if (result.omitted$has.stochastic.ordering) {
          s <- paste0(s, "Final Ranking with Stochastic Ordering\n")
          s <- paste0(s, "--------------------------------------\n")
        } else {
          s <- paste0(s, "Final Ranking (No Stochastic Ordering)\n")
          s <- paste0(s, "--------------------------------------\n")
        }
        s <- paste0(s, "\n")
        num.cluster <- nrow(result.omitted$expected.collapsed.pmf)
        num.collapsed.level <- ncol(result.omitted$expected.collapsed.pmf)
        temp <- matrix("", 2 + num.cluster, 1 + num.collapsed.level + 1)
        temp[1,1] <- "Brands"
        temp[2,1] <- "======"
        for (i in seq_len(num.collapsed.level)) {
          subset.range <- range(which(result.omitted$satisfaction.partition==i))
          if (subset.range[1] == subset.range[2]) {
            temp[1,1+i] <- subset.range[1] + add.one
          } else {
            temp[1,1+i] <- paste0(subset.range[1] + add.one, "-", subset.range[2]+add.one)
          }
          temp[2,1+i] <- paste(rep("=", max(nchar(temp[1,1+i]),2)), collapse="")
        }
        temp[1,1+num.collapsed.level+1] <- "Polarity Index"
        temp[2,1+num.collapsed.level+1] <- "=============="
        for (i in seq(num.cluster)) {
          if (result.omitted$has.stochastic.ordering) {
            label <- result.omitted$stochastic.ordering[i]
          } else {
            label <- i
          }
          temp[2+i,1] <- paste(which(result.omitted$brand.cluster==label), collapse=",")
          for (j in seq_len(num.collapsed.level)) {
            temp[2+i,1+j] <- round(result.omitted$expected.collapsed.pmf[label,j],
                                   2)
          }
          temp[2+i,1+num.collapsed.level+1] <-
            round(result.omitted$polarity.index[label], 2)
        }
        s <- paste0(s, matrix.string(temp, spacing=4))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Distributions with the omitted category added\nafter the stochastic ordering was obtained", "\n")
        temp <- matrix("", 2 + num.cluster, 2 + num.collapsed.level)
        temp[1,1] <- "Brands"
        temp[2,1] <- "======"
        if (add.one){
          temp[1,2] = 1
          temp[2,2] = "=="
        }
        for (i in seq_len(num.collapsed.level)) {
          subset.range <- range(which(result.omitted$satisfaction.partition==i))
          if (subset.range[1] == subset.range[2]) {
            temp[1,1+i + add.one] <- subset.range[1] + add.one
          } else {
            temp[1,1+i + add.one] <- paste0(subset.range[1] + add.one, "-", subset.range[2]+add.one)
          }
          temp[2,1+i + add.one] <- paste(rep("=", max(nchar(temp[1,1+i]),2)), collapse="")
        }
        if (!add.one){
          temp[1,num.collapsed.level+2] = satisfaction.constraint
          temp[2,num.collapsed.level+2] = "=="
        }
        

        for (i in seq(num.cluster)) {
          label <- result.omitted$stochastic.ordering[i]
          temp[2+i,1] <- paste(which(result.omitted$brand.cluster==label), collapse=",")
          
          for (j in seq_len(num.collapsed.level + 1)) {
            temp[2+i,1+j] <- round(pmf.with.constraint[label,j], 2)
          }

        }
        s <- paste0(s, matrix.string(temp, spacing=4))
        s <- paste0(s, "\n\n")
        s <- paste0(s, "Z-test for stochastic ordering \n")
        s <- paste0(s, "--------------------------------------\n")
        mid.category = ceiling(num.col/2)
        satisfaction.partition = result.omitted$satisfaction.partition
        mid.category.partition = satisfaction.partition[mid.category]
        contingency.table.collapsed.rows = result.omitted$anoas.result$collapsed.original.contingency.table
        rowSums_with_one <- function(x){
          if (is.null(ncol(x))){
            return(x)
          }
          else{
            return(rowSums(x))
          }
        }
        bottom.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition <= mid.category.partition)])
        top.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition > mid.category.partition)])
        ranking.matrix <- temp
        temp <- matrix("", 2 + num.cluster, 1 + num.cluster +1)
        temp[1,1] <- "Brands"
        for (j in 1:ncol(temp)){
          temp[2,j] <- "----"
        }
        for (i in 1:nrow(temp)){
          temp[i,2] <- "|"
        }
        for (i in 1:num.cluster){
          temp[i+2,1] <- ranking.matrix[i+2,1]
          temp[1, i+2]<- ranking.matrix[i+2,1]
          temp[i+2, i+2] <- "\\"
        }
        for (i in 1:length(bottom.half)){
          for (j in min((i+1), length(bottom.half)): length(bottom.half)){
            bottom_i = bottom.half[i]
            top_i = top.half[i]
            bottom_j = bottom.half[j]
            top_j = top.half[j]
            z_ij =  ( log(top_i/bottom_i) - log(top_j/bottom_j))/ (1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
            if (z_ij > qnorm(0.95)){
              temp[i+2, j+2] = "X"
            }
            
          }
        }
        s <- paste0(s, matrix.string(temp, spacing=4))
        
        ranking.show.result(s)
        ranking.show.plot1(result.omitted$avg.satisfaction, ncol(contingency.table))
        ranking.show.plot2(result.omitted$brand.cluster,
                           result.omitted$satisfaction.partition, result.omitted$expected.collapsed.pmf,
                           result.omitted$stochastic.ordering)

      }
      
      else {
      result <- ranking.confirmatory.computation(contingency.table,
                                                 upper.polarity.idx=upper.polarity.idx)
      s <- ""
      s <- paste0(s, "Confirmatory Model Result\n")
      s <- paste0(s, "=========================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
        s <- paste0(s, "Exponential Spacing Model Results on Original Data\n")
      s <- paste0(s, "==================================================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Expected Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(
        result$org.exp.spacing.result$expected, 2),
        left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "TAU(Y|X)= ", signif(result$org.exp.spacing.result$tau.given.row, 4),
                  "  TAU(X|Y)= ", signif(result$org.exp.spacing.result$tau.given.col, 4), "\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "X-Square= ", round(result$org.exp.spacing.result$chi.sq.stat, 2),
                  "  L-Square= ", round(result$org.exp.spacing.result$l.sq.stat, 2),
                  "  D.O.F.= ", result$org.exp.spacing.result$d.o.f,
                  "  p-value= ", signif(result$org.exp.spacing.result$p.value, 4), "\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Polynomial Degree for Rows: ", result$org.exp.spacing.result$poly.deg.row, "\n")
      s <- paste0(s, "Polynomial Degree for Columns: ", result$org.exp.spacing.result$poly.deg.col, "\n")
      s <- paste0(s, "\n")
      num.row <- length(result$org.exp.spacing.result$mu)
      num.col <- length(result$org.exp.spacing.result$nu)
      temp <- matrix("", 2 + num.row + 1 + num.col + 2, 3)
      temp[1,1] <- "Parameter"
      temp[2,1] <- "========="
      temp[1,2] <- "Estimate"
      temp[2,2] <- "========"
      temp[1,3] <- "E.A.S.D."
      temp[2,3] <- "========"
      for (i in seq_len(num.row)) {
        temp[2+i,1] <- paste0("Mu[", i, "]")
        temp[2+i,2] <- round(result$org.exp.spacing.result$mu[i], 2)
        if (is.na(result$org.exp.spacing.result$mu.easd[i])) {
          temp[2+i,3] <- "NA"
        } else {
          temp[2+i,3] <- round(result$org.exp.spacing.result$mu.easd[i], 2)
        }
      }
      for (i in seq_len(num.col)) {
        temp[2+1+num.row+i,1] <- paste0("Nu[", i, "]")
        temp[2+1+num.row+i,2] <- round(result$org.exp.spacing.result$nu[i], 2)
        if (is.na(result$org.exp.spacing.result$nu.easd[i])) {
          temp[2+1+num.row+i,3] <- "NA"
        } else {
          temp[2+1+num.row+i,3] <- round(result$org.exp.spacing.result$nu.easd[i], 2)
        }
      }
      temp[2+1+num.row+1+num.col+1,1] <- "Phi"
      temp[2+1+num.row+1+num.col+1,2] <- round(result$org.exp.spacing.result$phi, 2)
      if (is.na(result$org.exp.spacing.result$phi.easd)) {
        temp[2+1+num.row+1+num.col+1,3] <- "NA"
      } else {
        temp[2+1+num.row+1+num.col+1,3] <- round(result$org.exp.spacing.result$phi.easd, 2)
      }
      s <- paste0(s, matrix.string(temp, spacing=2))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Model Diagnostics\n")
      s <- paste0(s, "-----------------\n")
      if (result$org.exp.spacing.result$model.is.fit) {
        s <- paste0(s, "Model fits the observed data.\n")
      } else {
        s <- paste0(s, "Model does not fit the observed data.\n")
      }
      s <- paste0(s, "\n")
      s <- paste0(s, "Standardized Residuals\n")
      s <- paste0(s, "----------------------\n")
      s <- paste0(s, matrix.string(round(result$org.exp.spacing.result$std.residuals,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Number of significant residuals = ",
                  result$org.exp.spacing.result$num.sig.residuals, "\n")
      s <- paste0(s, "\n")
      if (result$has.stochastic.ordering) {
        s <- paste0(s, "Final Ranking with Stochastic Ordering\n")
        s <- paste0(s, "--------------------------------------\n")
      } else {
        s <- paste0(s, "Final Ranking (No Stochastic Ordering)\n")
        s <- paste0(s, "--------------------------------------\n")
      }
      s <- paste0(s, "\n")
      num.cluster <- nrow(result$expected.collapsed.pmf)
      num.collapsed.level <- ncol(result$expected.collapsed.pmf)
      temp <- matrix("", 2 + num.cluster, 1 + num.collapsed.level + 1)
      temp[1,1] <- "Brands"
      temp[2,1] <- "======"
      for (i in seq_len(num.collapsed.level)) {
        subset.range <- range(which(result$satisfaction.partition==i))
        if (subset.range[1] == subset.range[2]) {
          temp[1,1+i] <- subset.range[1]
        } else {
          temp[1,1+i] <- paste0(subset.range[1], "-", subset.range[2])
        }
        temp[2,1+i] <- paste(rep("=", max(nchar(temp[1,1+i]),2)), collapse="")
      }
      temp[1,1+num.collapsed.level+1] <- "Polarity Index"
      temp[2,1+num.collapsed.level+1] <- "=============="
      for (i in seq(num.cluster)) {
        if (result$has.stochastic.ordering) {
          label <- result$stochastic.ordering[i]
        } else {
          label <- i
        }
        temp[2+i,1] <- paste(which(result$brand.cluster==label), collapse=",")
        for (j in seq_len(num.collapsed.level)) {
          temp[2+i,1+j] <- round(result$expected.collapsed.pmf[label,j],
                                 2)
        }
        temp[2+i,1+num.collapsed.level+1] <-
          round(result$polarity.index[label], 2)
      }
      s <- paste0(s, matrix.string(temp, spacing=4))
      s <- paste0(s, "\n\n")
      s <- paste0(s, "Z-test for stochastic ordering \n")
      s <- paste0(s, "--------------------------------------\n")
      
      rowSums_with_one <- function(x){
        if (is.null(ncol(x))){
          return(x)
        }
        else{
          return(rowSums(x))
        }
      }
      
      mid.category = ceiling(num.col/2)
      satisfaction.partition = result$satisfaction.partition
      mid.category.partition = satisfaction.partition[mid.category]
      contingency.table.collapsed.rows = result$anoas.result$collapsed.original.contingency.table
      bottom.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition <= mid.category.partition)])
      top.half = rowSums_with_one(contingency.table.collapsed.rows[,which(satisfaction.partition > mid.category.partition)])
      ranking.matrix <- temp
      temp <- matrix("", 2 + num.cluster, 1 + num.cluster +1)
      temp[1,1] <- "Brands"
      for (j in 1:ncol(temp)){
        temp[2,j] <- "----"
      }
      for (i in 1:nrow(temp)){
        temp[i,2] <- "|"
      }
      for (i in 1:num.cluster){
        temp[i+2,1] <- ranking.matrix[i+2,1]
        temp[1, i+2]<- ranking.matrix[i+2,1]
        temp[i+2, i+2] <- "\\"
      }
      for (i in 1:length(bottom.half)){
        for (j in min((i+1), length(bottom.half)): length(bottom.half)){
          bottom_i = bottom.half[i]
          top_i = top.half[i]
          bottom_j = bottom.half[j]
          top_j = top.half[j]
          z_ij =  ( log(top_i/bottom_i) - log(top_j/bottom_j))/ (1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
          if (z_ij > qnorm(0.95)){
            temp[i+2, j+2] = "X"
          }

          }
      }
      s <- paste0(s, matrix.string(temp, spacing=4))

      ranking.show.result(s)
      ranking.show.plot1(result$avg.satisfaction, ncol(contingency.table))
      ranking.show.plot2(result$brand.cluster,
                         result$satisfaction.partition, result$expected.collapsed.pmf,
                         result$stochastic.ordering)
      }
    } else if (input$rankingModelName == "Brand Sensitivity") {
      num.brand <- nrow(m)
      if (num.brand == 1) {
        showModal(modalDialog(title = "Oops!", "Brand sensitivity needs at least 2 brands.",
                              easyClose = TRUE))
        return()
      }
      original.contingency.table <- m
      contingency.table <- original.contingency.table
      contingency.table[contingency.table == 0] <- 0.5
      # Perform Confirmatory Model computation
      all.models <- list()
      for (i in seq_len(num.brand)) {
        all.models[[i]] <- ranking.confirmatory.computation(
          contingency.table[-i,], org.only=TRUE)
      }
      s <- ""
      s <- paste0(s, "Ranking Brand Sensitivity Result\n")
      s <- paste0(s, "================================\n")
      s <- paste0(s, "\n")
      s <- paste0(s, "Observed Data Matrix\n")
      s <- paste0(s, "--------------------\n")
      s <- paste0(s, matrix.string(round(original.contingency.table,
                                         2), left.justified=FALSE))
      s <- paste0(s, "\n\n")
      output <- matrix("", 3 + num.brand, 3)
      output[1,1] <- "Omitted Brand"
      output[2,1] <- "-------------"
      output[3 + (1:num.brand)] <- 1:num.brand
      output[1,2] <- "Confirmatory Model"
      output[2,2] <- "------------------"
      output[3,2] <- "P-value"
      output[3,3] <- "Model fits?"
      for (i in seq_len(num.brand)) {
        output[3 + i, 2] <- signif(all.models[[i]]$org.exp.spacing.result$p.value, 4)
        output[3 + i, 3] <- ifelse(all.models[[i]]$org.exp.spacing.result$model.is.fit, "Yes", "No")
      }
      s <- paste0(s, matrix.string(output, spacing=4, left.justified=TRUE))
      ranking.show.result(s)
    }
  }
})

observeEvent(input$rankingBrandOmissionDataFile, {
  rv$ranking.brand.omission.data <- read.csv(
    input$rankingBrandOmissionDataFile$datapath, header = FALSE,
    strip.white = TRUE, stringsAsFactors = FALSE)
  shinyjs::enable("rankingBrandOmissionDownload")
})

observe({
  req(input$rankingBrandOmissionDataFileFormat)
  if (input$rankingBrandOmissionDataFileFormat == "Rows of subjects") {
    shinyjs::enable("rankingBrandOmissionDataFileBrand")
  } else {
    shinyjs::disable("rankingBrandOmissionDataFileBrand")
  }
})

output$rankingBrandOmissionDownload <- downloadHandler(
  filename = function() {
    b <- parse.group.string(input$rankingBrandOmissionWhich)
    if (length(b) != 1 || is.na(b)) {
      b <- 'none'
    } else {
      b <- b[[1]]
    }
    file.name <-
      tools::file_path_sans_ext(input$rankingBrandOmissionDataFile$name)
    file.extension <- tools::file_ext(input$rankingBrandOmissionDataFile$name)
    return(paste0(file.name, ' (omit ', paste(b, collapse=' '), ').',
                  file.extension))
  },
  content = function(file) {
    org.data <- rv$ranking.brand.omission.data
    if (input$rankingBrandOmissionDataFileFormat == "Rows of subjects") {
      # Purchase columns?
      if (!string.is.positive.integer(input$rankingBrandOmissionDataFileBrand)) {
        showNotification("Oops! Invalid column for brand.", type = "message")
        return()
      }
      brand.idx <- as.numeric(input$rankingBrandOmissionDataFileBrand)
      if (brand.idx > nrow(org.data)) {
        showNotification("Oops! Invalid column for brand.", type = "message")
        return()
      }
      brand.col <- trimws(org.data[,brand.idx])
      # Omit brands?
      brand <- parse.group.string(input$rankingBrandOmissionWhich)
      if (length(brand) != 1 || is.na(brand)) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      brand <- brand[[1]]
      existing.brands <- unique(brand.col)
      if (length(setdiff(brand, existing.brands)) > 0) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      is.omit <- brand.col %in% brand
      if (all(is.omit)) {
        showNotification("Oops! Cannot omit all brands", type = "message")
        return()
      }
      new.data <- org.data[!is.omit,]
    } else {
      # Omit brands?
      brand <- parse.group.string(input$rankingBrandOmissionWhich)
      if (length(brand) != 1 || is.na(brand)) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      brand <- brand[[1]]
      if (max(brand) > nrow(org.data)) {
        showNotification("Oops! Invalid brands.", type = "message")
        return()
      }
      if (length(brand) == nrow(org.data)) {
        showNotification("Oops! Cannot omit all brands.", type = "message")
        return()
      }
      new.data <- data.matrix(org.data)[-brand,]
    }
    # Write to file
    write.table(new.data, file, quote = FALSE, sep = ",", row.names = FALSE,
                col.names = FALSE, qmethod = "double")
  }
)

observeEvent(input$rankingExplanatoryDataFile, {
  rv$ranking.explanatory.data <- read.csv(
    input$rankingExplanatoryDataFile$datapath, header = FALSE,
    strip.white = TRUE, stringsAsFactors = FALSE)
})

observe({
  req(rv$ranking.explanatory.data)
  shinyjs::enable("rankingExplanatoryDataDownload")
})

output$rankingExplanatoryDataDownload <- downloadHandler(
  filename = "ranking_explanatory_data.csv",
  content = function(file) {
    org.data <- rv$ranking.explanatory.data
    # Check that brands are valid
    brands <- parse.group.string(input$rankingExplanatoryVariableBrand)
    
    satisfaction.column <- as.numeric(input$rankingExplanatoryVariableSatisfactionColumn)
    brand.column <- as.numeric(input$rankingExplanatoryVariableBrandColumn)
    # check that brand and satisfactin columns are valid
    if (is.na(brand.column) | brand.column == 0 | brand.column > ncol(org.data)){
      showNotification("Oops! Invalid column for brands.", type = "message")
      return()
    }
    if (is.na(satisfaction.column) | brand.column == 0 | brand.column > ncol(org.data)){
      showNotification("Oops! Invalid column for satisfaction", type = "message")
      return()
    }
    if (length(brands) != 1 || is.na(brands)) {
      showNotification("Oops! Invalid input for brands.", type = "message")
      return()
    }
    brands <- brands[[1]]
    if (max(brands) > max(org.data[,brand.column])) {
      showNotification("Oops! The input brands exceed the number of brands in the file.", type = "message")
      return()
    }
    # Check that satisfaction levels are valid
    satisfy <- parse.group.string(input$rankingExplanatoryVariableSatisfaction)
    if (length(satisfy) != 1 || is.na(satisfy)) {
      showNotification("Oops! Invalid input for satisfaction levels.", type = "message")
      return()
    }
    satisfy <- satisfy[[1]]
    if (max(satisfy) > max(org.data[,satisfaction.column])) {
      showNotification("Oops! The input satisfaction levels exceed the number of satisfaction levels in the file.", type = "message")
      return()
    }
    # Create binary target variable
    binary.target.var <- rep(2, nrow(org.data))
    binary.target.var[org.data[,brand.column] %in% brands & org.data[,satisfaction.column] %in% satisfy] <- 1
    new.data <- cbind(org.data, binary.target.var)
    # Write to file
    write.table(new.data, file, quote = FALSE, sep = ",", row.names = FALSE,
                col.names = FALSE, qmethod = "double")
  }
)

observeEvent(input$rankingExplanatorySort, {
  org.data <- rv$ranking.explanatory.data
  satisfaction.column <- as.numeric(input$rankingExplanatoryVariableSatisfactionColumn)
  brand.column <- as.numeric(input$rankingExplanatoryVariableBrandColumn)
  # check that brand and satisfactin columns are valid
  if (is.na(brand.column) | brand.column == 0 | brand.column > ncol(org.data)){
    showNotification("Oops! Invalid column for brands.", type = "message")
    return()
  }
  if (is.na(satisfaction.column) | brand.column == 0 | brand.column > ncol(org.data)){
    showNotification("Oops! Invalid column for satisfaction", type = "message")
    return()
  }
  
  # Check that brands are valid
  brands <- parse.group.string(input$rankingExplanatoryVariableBrand)
  if (length(brands) != 1 || is.na(brands)) {
    showNotification("Oops! Invalid input for brands.", type = "message")
    return()
  }
  brands <- brands[[1]]
  if (max(brands) > max(org.data[,brand.column])) {
    showNotification("Oops! The input brands exceed the number of brands in the file.", type = "message")
    return()
  }
  # Check that satisfaction levels are valid
  satisfy <- parse.group.string(input$rankingExplanatoryVariableSatisfaction)
  if (length(satisfy) != 1 || is.na(satisfy)) {
    showNotification("Oops! Invalid input for satisfaction levels.", type = "message")
    return()
  }
  satisfy <- satisfy[[1]]
  if (max(satisfy) > max(org.data[,satisfaction.column])) {
    showNotification("Oops! The input satisfaction levels exceed the number of satisfaction levels in the file.", type = "message")
    return()
  }
  # Check that explanatory variable columns are valid
  explanatory.col <- parse.group.string(input$rankingExplanatoryVariableColumn)
  if (length(explanatory.col) == 0 || length(explanatory.col) == 1 && is.na(explanatory.col)) {
    showNotification("Oops! Invalid input for the explanatory column.", type = "message")
    return()
  }
  explanatory.col <- explanatory.col[[1]]
  if (max(explanatory.col) > ncol(org.data)) {
    showNotification("Oops! Explanatory column exceeds the number of columns in the data file.", type = "message")
    return()
  }
  if (any(explanatory.col %in% c(brand.column,satisfaction.column))) {
    showNotification("Oops! Explanatory columns should not contain the brand or satisfaction level.", type = "message")
    return()
  }
  # Create binary target variable
  binary.target.var <- rep(2, nrow(org.data))
  binary.target.var[org.data[,brand.column] %in% brands & org.data[,satisfaction.column] %in% satisfy] <- 1
  if (length(unique(binary.target.var)) == 1) {
    showNotification("Oops! The target variable is not binary.", type = "message")
    return()
  }
  new.data <- cbind(org.data, binary.target.var)
  # Sort the explanatory variable columns
  explanatory.ordering <- threedimensional.sort.explanatory.variables(
    new.data, ncol(new.data), explanatory.col)
  output$rankingExplanatoryDescriptiveness <- renderText({
    paste("Sorted by descriptiveness:", paste(explanatory.ordering, collapse=", "))})
})

# Spacing variable
rv$spacing.data <- NULL
spacing.tab.counter <- 0
spacing.computed <- list()

output$spacingData <- renderRHandsontable({
  req(rv$spacing.data)
  rhandsontable(rv$spacing.data, useTypes = FALSE) %>%
    hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
    hot_cols(colWidths = 80) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$spacingDataCreate, {
  updateTabsetPanel(session, "spacingTab", selected = "Data")
  if (!string.is.dimension(input$spacingDataDimension, 2)) {
    showModal(modalDialog(title = "Oops!", "Invalid data dimensions.",
                          easyClose = TRUE))
    return()
  }
  d <- parse.dimension.string(input$spacingDataDimension)
  if (d[1] < 1) {
    showModal(modalDialog(title = "Oops!", "Data should have at least 2 rows.",
                          easyClose = TRUE))
    return()
  }
  if (d[2] < 1) {
    showModal(modalDialog(title = "Oops!", "Data should have at least 2 columns.",
                          easyClose = TRUE))
    return()
  }
  if (is.null(rv$spacing.data)) {
    rv$spacing.data <- empty.data(d[1], d[2])
  } else if (!all(dim(rv$spacing.data) == d)) {
    showModal(modalDialog(title = "Confirmation", "Modify the data dimensions?",
                          footer = tagList(actionButton("spacingDataModifyOk", "OK"),
                                           actionButton("spacingDataModifyCancel", "Cancel"))))
  }
})

observeEvent(input$spacingDataModifyOk, {
  d <- parse.dimension.string(input$spacingDataDimension)
  row <- min(nrow(rv$spacing.data), d[1])
  col <- min(ncol(rv$spacing.data), d[2])
  df <- empty.data(d[1], d[2])
  df[1:row,1:col] <- hot_to_r(input$spacingData)[1:row,1:col]
  rv$spacing.data <- df
  removeModal()
})

observeEvent(input$spacingDataModifyCancel, {
  updateTextInput(session, "spacingDataDimension",
                  value = paste(dim(rv$spacing.data), collapse = ","))
  removeModal()
})

observeEvent(input$spacingDataFile, {
  showModal(modalDialog(title = "File Information",
                        radioButtons("spacingDataFileFormat",
                                     label = "Data format",
                                     choices = list("Rows of subjects", "Contingency table")),
                        textInput("spacingDataFileRow", label = "Which column in file for row?"),
                        textInput("spacingDataFileColumn", label = "Which column in file for column?"),
                        footer = tagList(actionButton("spacingDataFileOk", "OK"),
                                         modalButton("Cancel"))))
})

observe({
  req(input$spacingDataFileFormat)
  if (input$spacingDataFileFormat == "Rows of subjects") {
    shinyjs::enable("spacingDataFileRow")
    shinyjs::enable("spacingDataFileColumn")
  } else {
    shinyjs::disable("spacingDataFileRow")
    shinyjs::disable("spacingDataFileColumn")
  }
})

observeEvent(input$spacingDataFileOk, {
  if (input$spacingDataFileFormat == "Rows of subjects") {
    if (!string.is.positive.integer(input$spacingDataFileRow)) {
      showNotification("Oops! Invalid row number.", type = "message")
      return()
    }
    if (!string.is.positive.integer(input$spacingDataFileColumn)) {
      showNotification("Oops! Invalid column number.", type = "message")
      return()
    }
    row <- as.numeric(input$spacingDataFileRow)
    col <- as.numeric(input$spacingDataFileColumn)
    if (row == col) {
      showNotification("Oops! Use different row and column numbers.",
                       type = "message")
      return()
    }
    df <- read.csv(input$spacingDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    if (max(row, col) > ncol(df)) {
      showNotification(paste("Oops! Only", ncol(df), "columns in the file."),
                       type = "message")
      return()
    }
    m <- table.data(table(df[,c(row, col)]))
    if (nrow(m) < 1) {
      showNotification("Oops! Data should have at least 2 rows.", type = "message")
      return()
    }
    if (ncol(m) < 1) {
      showNotification("Oops! Data should have at least 2 columns.", type = "message")
      return()
    }
    rv$spacing.data <- m
    removeModal()
  } else {
    df <- read.csv(input$spacingDataFile$datapath, header = FALSE,
                   strip.white = TRUE, stringsAsFactors = FALSE)
    m <- matrix.data(data.matrix(df))
    if (nrow(m) < 1) {
      showNotification("Oops! Data should have at least 2 rows.", type = "message")
      return()
    }
    if (ncol(m) < 1) {
      showNotification("Oops! Data should have at least 2 columns.", type = "message")
      return()
    }
    rv$spacing.data <- m
    removeModal()
  }
  updateTabsetPanel(session, "spacingTab", selected = "Data")
})

observeEvent(input$spacingDataCollapse, {
  updateTabsetPanel(session, "spacingTab", selected = "Data")
  rows <- parse.group.string(input$spacingDataCollapseRow)
  if (length(rows) == 1 && is.na(rows) ||
      length(rows) > 0 && max(unlist(rows)) > nrow(hot_to_r(input$spacingData))) {
    showModal(modalDialog(title = "Oops!", "Invalid row groups.",
                          easyClose = TRUE))
    return()
  }
  cols <- parse.group.string(input$spacingDataCollapseColumn)
  if (length(cols) == 1 && is.na(cols) ||
      length(cols) > 0 && max(unlist(cols)) > ncol(hot_to_r(input$spacingData))) {
    showModal(modalDialog(title = "Oops!", "Invalid column groups.",
                          easyClose = TRUE))
    return()
  }
  if (any(is.na(suppressWarnings(data.numeric.matrix(hot_to_r(input$spacingData)))))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  showModal(modalDialog(title = "Confirmation", "Collapse the data?",
                        footer = tagList(actionButton("spacingDataCollapseOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$spacingDataCollapseOk, {
  rows <- parse.group.string(input$spacingDataCollapseRow)
  cols <- parse.group.string(input$spacingDataCollapseColumn)
  m <- as.data.frame(collapse.data(data.numeric.matrix(hot_to_r(input$spacingData)),
                                   rows, cols))
  rownames(m) <- 1:nrow(m)
  colnames(m) <- 1:ncol(m)
  
  if (nrow(m) < 1) {
    showNotification("Oops! Data should have at least 2 rows.", type = "message")
    return()
  }
  if (ncol(m) < 1) {
    showNotification("Oops! Data should have at least 2 columns.", type = "message")
    return()
  }
  rv$spacing.data <- m
  removeModal()
})

output$spacingDataDownload <- downloadHandler(
  filename = "spacing_data.csv",
  content = function(file) {
    write.table(hot_to_r(input$spacingData), file, quote = FALSE,
                sep = ",", row.names = FALSE, col.names = FALSE,
                qmethod = "double")
  }
)

observeEvent(input$spacingDataClear, {
  showModal(modalDialog(title = "Confirmation", "Clear the data?",
                        footer = tagList(actionButton("spacingDataClearOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$spacingDataClearOk, {
  updateTabsetPanel(session, "spacingTab", selected = "Data")
  rv$spacing.data <- NULL
  removeModal()
})

observe({
  if (is.null(rv$spacing.data)) {
    updateTextInput(session, "spacingDataDimension", value = "")
    updateActionButton(session, "spacingDataCreate", label = "Create")
    updateTextInput(session, "spacingDataCollapseRow", value = "")
    updateTextInput(session, "spacingDataCollapseColumn", value = "")
    shinyjs::disable("spacingDataCollapse")
    shinyjs::disable("spacingDataDownload")
    shinyjs::disable("spacingDataClear")
  } else {
    updateTextInput(session, "spacingDataDimension",
                    value = paste(dim(rv$spacing.data), collapse = ","))
    updateActionButton(session, "spacingDataCreate", label = "Modify")
    shinyjs::enable("spacingDataCollapse")
    shinyjs::enable("spacingDataDownload")
    shinyjs::enable("spacingDataClear")
  }
})

spacing.show.model.selection <- function(s) {
  x <- spacing.tab.counter <<- spacing.tab.counter + 1
  text.id <- paste0("spacingResult", x)
  
  appendTab("spacingTab",
            tabPanel(paste("Result", x),
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(),
                     actionButton(paste0("spacingResultClose", x),
                                  label = "Close"),
                     br(), br()),
            select = TRUE)
  
  output[[text.id]] <- renderText({s})
  
  code <- gsub('%x', x,
               'observeEvent(input$spacingResultClose%x, {
        removeTab("spacingTab", "Result %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

spacing.show.result <- function(s, phi, mu, nu) {
  x <- spacing.tab.counter <<- spacing.tab.counter + 1
  spacing.computed[[x]] <- list(phi=phi, mu=mu, nu=nu)
  
  text.id <- paste0("spacingResult", x)
  
  appendTab("spacingTab",
            tabPanel(paste("Result", x),
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(),
                     wellPanel(
                       tags$b('Odd Ratio Computation'),
                       fluidRow(
                         column(width = 3, offset = 5, align="center", helpText("Column 1")),
                         column(width = 3, align="center", helpText("Column 2"))
                       ),
                       fluidRow(
                         column(width = 3, offset = 5,
                                textInput(paste0("spacingOddRatioColumn1", x),
                                          label = NULL, value = "")
                         ),
                         column(width = 3,
                                textInput(paste0("spacingOddRatioColumn2", x),
                                          label = NULL, value = "")
                         )
                       ),
                       fluidRow(
                         column(width = 2, align="right", helpText("Row 1")),
                         column(width = 3, textInput(paste0("spacingOddRatioRow1", x), label = NULL, value = ""))
                       ),
                       fluidRow(
                         column(width = 2, align="right", helpText("Row 2")),
                         column(width = 3, textInput(paste0("spacingOddRatioRow2", x), label = NULL, value = ""))
                       ),
                       fluidRow(
                         column(width=3, offset=5,
                                actionButton(paste0("spacingComputeOddRatio", x), label = "Compute!"))
                       ),
                       br(),
                       fluidRow(
                         column(width=3, offset=2, align="right",
                                helpText('Odd Ratio')
                         ),
                         column(width=3,
                                textInput(paste0("spacingOddRatio", x), label = NULL, value = ""))
                       )
                     ),
                     actionButton(paste0("spacingResultClose", x), label = "Close"),
                     br(),
                     br()),
            select = TRUE)
  
  output[[text.id]] <- renderText({s})
  
  code <- gsub('%x', x,
               'observeEvent(input$spacingResultClose%x, {
        removeTab("spacingTab", "Result %x")
      })', fixed=TRUE)
  eval(parse(text=code))
  
  code <- gsub('%x', x,
               'observeEvent(input$spacingComputeOddRatio%x, {
      if (!string.is.positive.integer(input$spacingOddRatioRow1%x)) {
        showModal(modalDialog(title = "Oops!", "Row 1 is invalid.",
                            easyClose = TRUE))
        return()
      }
      r1 <- as.numeric(input$spacingOddRatioRow1%x)
      if (!string.is.positive.integer(input$spacingOddRatioRow2%x)) {
        showModal(modalDialog(title = "Oops!", "Row 2 is invalid.",
                            easyClose = TRUE))
        return()
      }
      r2 <- as.numeric(input$spacingOddRatioRow2%x)
      if (!string.is.positive.integer(input$spacingOddRatioColumn1%x)) {
        showModal(modalDialog(title = "Oops!", "Column 1 is invalid.",
                            easyClose = TRUE))
        return()
      }
      c1 <- as.numeric(input$spacingOddRatioColumn1%x)
      if (!string.is.positive.integer(input$spacingOddRatioColumn2%x)) {
        showModal(modalDialog(title = "Oops!", "Column 2 is invalid.",
                            easyClose = TRUE))
        return()
      }
      c2 <- as.numeric(input$spacingOddRatioColumn2%x)
      mu <- spacing.computed[[%x]]$mu
      nu <- spacing.computed[[%x]]$nu
      phi <- spacing.computed[[%x]]$phi
      num.row <- length(mu)
      num.col <- length(nu)
      if (r1 > num.row) {
        showModal(modalDialog(title = "Oops!", "Row 1 is invalid.",
                            easyClose = TRUE))
        return()
      }
      if (r2 > num.row) {
        showModal(modalDialog(title = "Oops!", "Row 2 is invalid.",
                            easyClose = TRUE))
        return()
      }
      if (c1 > num.col) {
        showModal(modalDialog(title = "Oops!", "Column 1 is invalid.",
                            easyClose = TRUE))
        return()
      }
      if (c2 > num.col) {
        showModal(modalDialog(title = "Oops!", "Column 2 is invalid.",
                            easyClose = TRUE))
        return()
      }
      if (r1 == r2) {
        showModal(modalDialog(title = "Oops!", "Row 1 and 2 should be different.",
                            easyClose = TRUE))
        return()
      }
      if (c1 == c2) {
        showModal(modalDialog(title = "Oops!", "Column 1 and 2 should be different.",
                            easyClose = TRUE))
        return()
      }
      odd.ratio <- round(exp(phi * (mu[r1] - mu[r2]) * (nu[c1] - nu[c2])), 4)
      updateTextInput(session, "spacingOddRatio%x",
                      label = NULL,
                      value = odd.ratio)
    })', fixed=TRUE)
  eval(parse(text=code))
}

observeEvent(input$spacingGo, {
  # Data is clean?
  if (is.null(rv$spacing.data)) {
    showModal(modalDialog(title = "Oops!", "There is no data.",
                          easyClose = TRUE))
    return()
  }
  m <- suppressWarnings(data.numeric.matrix(hot_to_r(input$spacingData)))
  if (any(is.na(m))) {
    showModal(modalDialog(title = "Oops!", "Data should be numeric.",
                          easyClose = TRUE))
    return()
  }
  if (any(m < 0)) {
    showModal(modalDialog(title = "Oops!", "Data should be non-negative.",
                          easyClose = TRUE))
    return()
  }
  original.contingency.table <- m
  contingency.table <- original.contingency.table
  contingency.table[contingency.table == 0] <- 0.5
  num.row <- nrow(contingency.table)
  num.col <- ncol(contingency.table)
  model.type <- input$spacingModelType
  # Model selection?
  if (model.type == "Model Selection") {
    s <- ""
    if (input$spacingModelName == "Exponential Spacing") {
      computation <- spacing.exponential.spacing.computation
      s <- paste0(s, "Exponential Spacing Model Selection\n")
      s <- paste0(s, "===================================\n")
    } else {
      computation <- spacing.canonical.correlation.computation
      s <- paste0(s, "Canonical Correlation Model Selection\n")
      s <- paste0(s, "=====================================\n")
    }
    s <- paste0(s, "\n")
    s <- paste0(s, "Observed Data Matrix\n")
    s <- paste0(s, "--------------------\n")
    s <- paste0(s, matrix.string(round(original.contingency.table, 2),
                                 left.justified=FALSE))
    s <- paste0(s, "\n\n")
    
    p.value.threshold <- 0.05
    indent <- space(4)
    s <- paste0(s, "Models\n")
    s <- paste0(s, "------\n")
    model.type <- "RC Model"
    poly.deg.row <- num.row - 1
    poly.deg.col <- num.col - 1
    rc.model <- computation(contingency.table, poly.deg.row, poly.deg.col)
    s <- paste0(s, "Model Type: ", model.type, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Rows: ", poly.deg.row, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Columns: ", poly.deg.col, "\n")
    s <- paste0(s, indent, paste0("X-Square= ",
                                  round(rc.model$chi.sq.stat, 2), "  L-Square= ",
                                  round(rc.model$l.sq.stat, 2), "  D.O.F.= ", rc.model$d.o.f,
                                  "  p-value= ", signif(rc.model$p.value, 4)))
    s <- paste0(s, "\n")
    if (rc.model$p.value > p.value.threshold) {
      s <- paste0(s, indent, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, indent, "Model does not fit the observed data.\n")
    }
    s <- paste0(s, "\n")
    
    model.type <- "R Model"
    poly.deg.row <- num.row - 1
    poly.deg.col <- 1
    r.model <- computation(contingency.table, poly.deg.row, poly.deg.col)
    s <- paste0(s, "Model Type: ", model.type, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Rows: ", poly.deg.row, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Columns: ", poly.deg.col, "\n")
    s <- paste0(s, indent, paste0("X-Square= ",
                                  round(r.model$chi.sq.stat, 2), "  L-Square= ",
                                  round(r.model$l.sq.stat, 2), "  D.O.F.= ", r.model$d.o.f,
                                  "  p-value= ", signif(r.model$p.value, 4)))
    s <- paste0(s, "\n")
    if (r.model$p.value > p.value.threshold) {
      s <- paste0(s, indent, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, indent, "Model does not fit the observed data.\n")
    }
    s <- paste0(s, "\n")
    
    model.type <- "C Model"
    poly.deg.row <- 1
    poly.deg.col <- num.col - 1
    c.model <- computation(contingency.table, poly.deg.row, poly.deg.col)
    s <- paste0(s, "Model Type: ", model.type, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Rows: ", poly.deg.row, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Columns: ", poly.deg.col, "\n")
    s <- paste0(s, indent, paste0("X-Square= ",
                                  round(c.model$chi.sq.stat, 2), "  L-Square= ",
                                  round(c.model$l.sq.stat, 2), "  D.O.F.= ", c.model$d.o.f,
                                  "  p-value= ", signif(c.model$p.value, 4)))
    s <- paste0(s, "\n")
    if (c.model$p.value > p.value.threshold) {
      s <- paste0(s, indent, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, indent, "Model does not fit the observed data.\n")
    }
    s <- paste0(s, "\n")
    
    model.type <- "U Model"
    poly.deg.row <- 1
    poly.deg.col <- 1
    u.model <- computation(contingency.table, poly.deg.row, poly.deg.col)
    s <- paste0(s, "Model Type: ", model.type, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Rows: ", poly.deg.row, "\n")
    s <- paste0(s, indent, "Polynomial Degree for Columns: ", poly.deg.col, "\n")
    s <- paste0(s, indent, paste0("X-Square= ",
                                  round(u.model$chi.sq.stat, 2), "  L-Square= ",
                                  round(u.model$l.sq.stat, 2), "  D.O.F.= ", u.model$d.o.f,
                                  "  p-value= ", signif(u.model$p.value, 4)))
    s <- paste0(s, "\n")
    if (u.model$p.value > p.value.threshold) {
      s <- paste0(s, indent, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, indent, "Model does not fit the observed data.\n")
    }
    
    models <- list(
      list(model.name="RC Model", l.sq.stat=rc.model$l.sq.stat, d.o.f=rc.model$d.o.f, tier=3),
      list(model.name="R Model", l.sq.stat=r.model$l.sq.stat, d.o.f=r.model$d.o.f, tier=2),
      list(model.name="C Model", l.sq.stat=c.model$l.sq.stat, d.o.f=c.model$d.o.f, tier=2),
      list(model.name="U Model", l.sq.stat=u.model$l.sq.stat, d.o.f=u.model$d.o.f, tier=1))
    best.name <- model.selection(models)
    s <- paste0(s, "\n")
    s <- paste0(s, "Optimal Model\n")
    s <- paste0(s, "-------------\n")
    if (is.null(best.name)) {
      s <- paste0(s, "No model fits the observed data.\n")
    } else {
      s <- paste0(s, best.name, " is the most parsimonious model that fits the observed data.\n")
    }
    spacing.show.model.selection(s)
    return()
  }
  # Specific model
  if (model.type == "RC Model") {
    poly.deg.row <- num.row - 1
    poly.deg.col <- num.col - 1
  } else if (model.type == "R Model") {
    poly.deg.row <- num.row - 1
    poly.deg.col <- 1
  } else if (model.type == "C Model") {
    poly.deg.row <- 1
    poly.deg.col <- num.col - 1
  } else if (model.type == "U Model") {
    poly.deg.row <- 1
    poly.deg.col <- 1
  }
  if (input$spacingModelName == "Exponential Spacing") {
    result <- spacing.exponential.spacing.computation(contingency.table,
                                                      poly.deg.row, poly.deg.col)
    s <- ""
    s <- paste0(s, "Exponential Spacing Model Result\n")
    s <- paste0(s, "================================\n")
    s <- paste0(s, "\n")
    s <- paste0(s, "Observed Data Matrix\n")
    s <- paste0(s, "--------------------\n")
    s <- paste0(s, matrix.string(round(original.contingency.table,
                                       2), left.justified=FALSE))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Expected Data Matrix\n")
    s <- paste0(s, "--------------------\n")
    s <- paste0(s, matrix.string(round(result$expected, 2),
                                 left.justified=FALSE))
    s <- paste0(s, "\n\n")
    s <- paste0(s, paste0("TAU(Y|X)= ",
                          signif(result$tau.given.row, 4), "  TAU(X|Y)= ",
                          signif(result$tau.given.col, 4)))
    s <- paste0(s, "\n\n")
    s <- paste0(s, paste0("X-Square= ",
                          round(result$chi.sq.stat, 2), "  L-Square= ",
                          round(result$l.sq.stat, 2), "  D.O.F.= ", result$d.o.f,
                          "  p-value= ", signif(result$p.value, 4)))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Model Type: ", model.type, "\n")
    s <- paste0(s, "Polynomial Degree for Rows: ", poly.deg.row, "\n")
    s <- paste0(s, "Polynomial Degree for Columns: ", poly.deg.col, "\n")
    s <- paste0(s, "\n")
    temp <- matrix("", 2 + num.row + 1 + num.col + 2, 3)
    temp[1,1] <- "Parameter"
    temp[2,1] <- "========="
    temp[1,2] <- "Estimate"
    temp[2,2] <- "========"
    temp[1,3] <- "E.A.S.D."
    temp[2,3] <- "========"
    for (i in seq_len(num.row)) {
      temp[2+i,1] <- paste0("Mu[", i, "]")
      temp[2+i,2] <- round(result$mu[i], 2)
      if (is.na(result$mu.easd[i])) {
        temp[2+i,3] <- "NA"
      } else {
        temp[2+i,3] <- round(result$mu.easd[i], 2)
      }
    }
    for (i in seq_len(num.col)) {
      temp[2+1+num.row+i,1] <- paste0("Nu[", i, "]")
      temp[2+1+num.row+i,2] <- round(result$nu[i], 2)
      if (is.na(result$nu.easd[i])) {
        temp[2+1+num.row+i,3] <- "NA"
      } else {
        temp[2+1+num.row+i,3] <- round(result$nu.easd[i], 2)
      }
    }
    temp[2+1+num.row+1+num.col+1,1] <- "Phi"
    temp[2+1+num.row+1+num.col+1,2] <- round(result$phi, 2)
    if (is.na(result$phi.easd)) {
      temp[2+1+num.row+1+num.col+1,3] <- "NA"
    } else {
      temp[2+1+num.row+1+num.col+1,3] <- round(result$phi.easd, 2)
    }
    s <- paste0(s, matrix.string(temp, spacing=2))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Maximum and Minimum Odd Ratios\n")
    s <- paste0(s, "------------------------------\n")
    temp.m <- matrix("", 3, 2)
    temp.m[1,1] <- paste(" Max OR:", round(result$max.odd.ratio,
                                           4))
    temp.m[2,1] <- paste("   Rows:", paste(sort(result$row.of.max.or), collapse=","))
    temp.m[3,1] <- paste("Columns:", paste(sort(result$col.of.max.or), collapse=","))
    temp.m[1,2] <- paste(" Any two adjusted rows/colume:", round(result$min.odd.ratio,
                                                                 4))
    temp.m[2,2] <- paste("   Rows:", paste(sort(result$row.of.min.or), collapse=","))
    temp.m[3,2] <- paste("Columns:", paste(sort(result$col.of.min.or), collapse=","))
    s <- paste0(s, matrix.string(temp.m, spacing=6))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Model Diagnostics\n")
    s <- paste0(s, "-----------------\n")
    if (result$model.is.fit) {
      s <- paste0(s, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, "Model does not fit the observed data.\n")
    }
    s <- paste0(s, "\n")
    s <- paste0(s, "Standardized Residuals\n")
    s <- paste0(s, "----------------------\n")
    s <- paste0(s, matrix.string(round(result$std.residuals, 2),
                                 left.justified=FALSE))
    s <- paste0(s, "\n\n")
    s <- paste0(s, paste0("Number of significant residuals = ",
                          result$num.sig.residuals))
    spacing.show.result(s, result$phi, result$mu, result$nu)
  } else {
    # Perform the computation.
    result <- spacing.canonical.correlation.computation(contingency.table,
                                                        poly.deg.row, poly.deg.col)
    s <- ""
    s <- paste0(s, "Canonical Correlation Model Result\n")
    s <- paste0(s, "==================================\n")
    s <- paste0(s, "\n")
    s <- paste0(s, "Observed Data Matrix\n")
    s <- paste0(s, "--------------------\n")
    s <- paste0(s, matrix.string(round(original.contingency.table,
                                       2), left.justified=FALSE))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Expected Data Matrix\n")
    s <- paste0(s, "--------------------\n")
    s <- paste0(s, matrix.string(round(result$expected, 2),
                                 left.justified=FALSE))
    s <- paste0(s, "\n\n")
    s <- paste0(s, paste0("TAU(Y|X)= ",
                          signif(result$tau.given.row, 4), "  TAU(X|Y)= ",
                          signif(result$tau.given.col, 4)))
    s <- paste0(s, "\n\n")
    s <- paste0(s, paste0("X-Square= ",
                          round(result$chi.sq.stat, 2), "  L-Square= ",
                          round(result$l.sq.stat, 2), "  D.O.F.= ", result$d.o.f,
                          "  p-value= ", signif(result$p.value, 4)))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Model Type: ", model.type, "\n")
    s <- paste0(s, "Polynomial Degree for Rows: ", poly.deg.row, "\n")
    s <- paste0(s, "Polynomial Degree for Columns: ", poly.deg.col, "\n")
    s <- paste0(s, "\n")
    temp <- matrix("", 2 + num.row + 1 + num.col + 2, 3)
    temp[1,1] <- "Parameter"
    temp[2,1] <- "========="
    temp[1,2] <- "Estimate"
    temp[2,2] <- "========"
    temp[1,3] <- "E.A.S.D."
    temp[2,3] <- "========"
    for (i in seq_len(num.row)) {
      temp[2+i,1] <- paste0("Mu[", i, "]")
      temp[2+i,2] <- round(result$mu[i], 2)
      if (is.na(result$mu.easd[i])) {
        temp[2+i,3] <- "NA"
      } else {
        temp[2+i,3] <- round(result$mu.easd[i], 2)
      }
    }
    for (i in seq_len(num.col)) {
      temp[2+1+num.row+i,1] <- paste0("Nu[", i, "]")
      temp[2+1+num.row+i,2] <- round(result$nu[i], 2)
      if (is.na(result$nu.easd[i])) {
        temp[2+1+num.row+i,3] <- "NA"
      } else {
        temp[2+1+num.row+i,3] <- round(result$nu.easd[i], 2)
      }
    }
    temp[2+1+num.row+1+num.col+1,1] <- "Phi"
    temp[2+1+num.row+1+num.col+1,2] <- round(result$phi, 2)
    if (is.na(result$phi.easd)) {
      temp[2+1+num.row+1+num.col+1,3] <- "NA"
    } else {
      temp[2+1+num.row+1+num.col+1,3] <- round(result$phi.easd, 2)
    }
    s <- paste0(s, matrix.string(temp, spacing=2))
    s <- paste0(s, "\n\n")
    s <- paste0(s, "Model Diagnostics\n")
    s <- paste0(s, "-----------------\n")
    if (result$model.is.fit) {
      s <- paste0(s, "Model fits the observed data.\n")
    } else {
      s <- paste0(s, "Model does not fit the observed data.\n")
    }
    s <- paste0(s, "\n")
    s <- paste0(s, "Standardized Residuals\n")
    s <- paste0(s, "----------------------\n")
    s <- paste0(s, matrix.string(round(result$std.residuals, 2),
                                 left.justified=FALSE))
    s <- paste0(s, "\n\n")
    s <- paste0(s, paste0("Number of significant residuals = ",
                          result$num.sig.residuals))
    spacing.show.result(s, result$phi, result$mu, result$nu)
  }
})

# Scale conversion variable
rv$scale.conversion.data <- NULL
scale.conversion.tab.counter <- 0

output$scaleData <- renderRHandsontable({
  req(rv$scale.conversion.data)
  rhandsontable(rv$scale.conversion.data, useTypes = FALSE) %>%
    hot_table(overflow = "hidden", rowHeaderWidth = 70) %>%
    hot_cols(colWidths = 80) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$scaleDataCreate, {
  updateTabsetPanel(session, "scaleTab", selected = "Data")
  if (!string.is.dimension(input$scaleDataDimension, 1)) {
    showModal(modalDialog(title = "Oops!", "Invalid data length.",
                          easyClose = TRUE))
    return()
  }
  d <- parse.dimension.string(input$scaleDataDimension)
  if (is.null(rv$scale.conversion.data)) {
    rv$scale.conversion.data <- empty.data(d, 2, row.name = 1:d,
                                           col.name = c("Row", "Column"))
  } else if (nrow(rv$scale.conversion.data) != d) {
    showModal(modalDialog(title = "Confirmation", "Modify the data length?",
                          footer = tagList(actionButton("scaleDataModifyOk", "OK"),
                                           actionButton("scaleDataModifyCancel", "Cancel"))))
  }
})

observeEvent(input$scaleDataModifyOk, {
  d <- parse.dimension.string(input$scaleDataDimension)
  row <- min(nrow(rv$scale.conversion.data), d)
  df <- empty.data(d, 2, row.name = 1:d, col.name = c("Row", "Column"))
  df[1:row,] <- hot_to_r(input$scaleData)[1:row,]
  rv$scale.conversion.data <- df
  removeModal()
})

observeEvent(input$scaleDataModifyCancel, {
  updateTextInput(session, "scaleDataDimension",
                  value = nrow(rv$scale.conversion.data))
  removeModal()
})

observeEvent(input$scaleDataFile, {
  showModal(modalDialog(title = "File Information",
                        radioButtons("scaleDataFileFormat",
                                     label = "Data format",
                                     choices = list("Columns of raw data", "Two columns of marginals")),
                        textInput("scaleDataFileRow", label = "Which column in file for row?"),
                        textInput("scaleDataFileColumn", label = "Which column in file for column?"),
                        footer = tagList(actionButton("scaleDataFileOk", "OK"),
                                         modalButton("Cancel"))))
})

observe({
  req(input$scaleDataFileFormat)
  if (input$scaleDataFileFormat == "Columns of raw data") {
    shinyjs::enable("scaleDataFileRow")
    shinyjs::enable("scaleDataFileColumn")
  } else {
    shinyjs::disable("scaleDataFileRow")
    shinyjs::disable("scaleDataFileColumn")
  }
})

observeEvent(input$scaleDataFileOk, {
  if (input$scaleDataFileFormat == "Columns of raw data") {
    if (!string.is.positive.integer(input$scaleDataFileRow)) {
      showNotification("Oops! Invalid row number.", type = "message")
      return()
    }
    if (!string.is.positive.integer(input$scaleDataFileColumn)) {
      showNotification("Oops! Invalid column number.", type = "message")
      return()
    }
    row.idx <- as.numeric(input$scaleDataFileRow)
    col.idx <- as.numeric(input$scaleDataFileColumn)
    if (row.idx == col.idx) {
      showNotification("Oops! Use different row and column numbers.",
                       type = "message")
      return()
    }
    # Read the data
    temp.data <- read.csv(input$scaleDataFile$datapath, header = FALSE,
                          strip.white = TRUE, stringsAsFactors = FALSE)
    num.col <- ncol(temp.data)
    if (max(row.idx, col.idx) > ncol(temp.data)) {
      showNotification(paste("Oops! Only", ncol(temp.data), "columns in the file."),
                       type = "message")
      return()
    }
    row.data <- temp.data[,row.idx]
    is.not.na <- !is.na(row.data)
    if (any(is.not.na)) {
      row.data <- as.numeric(table(row.data[is.not.na]))
    }
    col.data <- temp.data[,col.idx]
    is.not.na <- !is.na(col.data)
    if (any(is.not.na)) {
      col.data <- as.numeric(table(col.data[is.not.na]))
    }
    num.row <- length(row.data)
    num.col <- length(col.data)
    d <- max(num.row, num.col, 1)
    data <- empty.data(d, 2, row.name = 1:d,
                       col.name = c("Row", "Column"))
    if (num.row > 0) {
      data[1:num.row,1] <- row.data
    }
    if (num.col > 0) {
      data[1:num.col,2] <- col.data
    }
    rv$scale.conversion.data <- data
    removeModal()
  } else {
    data <- data.matrix(read.csv(input$scaleDataFile$datapath, header=FALSE))
    if (ncol(data) != 2) {
      showNotification("Oops! Data file should have 2 columns.",  type = "message")
      return()
    }
    row.data <- data[,1]
    temp.idx <- which(!is.na(row.data))
    if (length(temp.idx) > 0) {
      row.data <- row.data[1:max(temp.idx)]
    } else {
      row.data <- NULL
    }
    col.data <- data[,2]
    temp.idx <- which(!is.na(col.data))
    if (length(temp.idx) > 0) {
      col.data <- col.data[1:max(temp.idx)]
    } else {
      col.data <- NULL
    }
    num.row <- length(row.data)
    num.col <- length(col.data)
    d <- max(num.row, num.col, 1)
    data <- empty.data(d, 2, row.name = 1:d, col.name = c("Row", "Column"))
    if (num.row > 0) {
      data[1:num.row,1] <- row.data
    }
    if (num.col > 0) {
      data[1:num.col,2] <- col.data
    }
    rv$scale.conversion.data <- data
    removeModal()
  }
  updateTabsetPanel(session, "scaleTab", selected = "Data")
})

output$scaleDataDownload <- downloadHandler(
  filename = "scale_conversion_data.csv",
  content = function(file) {
    write.table(hot_to_r(input$scaleData), file, quote = FALSE,
                sep = ",", na="", row.names = FALSE, col.names = FALSE,
                qmethod = "double")
  }
)

observeEvent(input$scaleDataClear, {
  showModal(modalDialog(title = "Confirmation", "Clear the data?",
                        footer = tagList(actionButton("scaleDataClearOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$scaleDataClearOk, {
  updateTabsetPanel(session, "scaleTab", selected = "Data")
  rv$scale.conversion.data <- NULL
  removeModal()
})

observe({
  if (is.null(rv$scale.conversion.data)) {
    updateTextInput(session, "scaleDataDimension", value = "")
    updateActionButton(session, "scaleDataCreate", label = "Create")
    shinyjs::disable("scaleDataDownload")
    shinyjs::disable("scaleDataClear")
  } else {
    updateTextInput(session, "scaleDataDimension",
                    value = nrow(rv$scale.conversion.data))
    updateActionButton(session, "scaleDataCreate", label = "Modify")
    shinyjs::enable("scaleDataDownload")
    shinyjs::enable("scaleDataClear")
  }
})

scale.conversion.show.result <- function(s) {
  x <- scale.conversion.tab.counter <<- scale.conversion.tab.counter + 1
  text.id <- paste0("scaleResult", x)
  
  appendTab("scaleTab",
            tabPanel(paste("Result", x),
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(),
                     actionButton(paste0("scaleResultClose", x),
                                  label = "Close"),
                     br(), br()),
            select = TRUE)
  
  output[[text.id]] <- renderText({s})
  
  code <- gsub('%x', x,
               'observeEvent(input$scaleResultClose%x, {
        removeTab("scaleTab", "Result %x")
      })', fixed=TRUE)
  eval(parse(text=code))
}

observeEvent(input$scaleGo, {
  # Data is clean?
  if (is.null(rv$scale.conversion.data)) {
    showModal(modalDialog(title = "Oops!", "There is no data.",
                          easyClose = TRUE))
    return()
  }
  data <- suppressWarnings(data.numeric.matrix(hot_to_r(input$scaleData)))
  
  row.data <- data[,1]
  non.na.idx <- which(!is.na(row.data))
  if (length(non.na.idx) == 0) {
    showModal(modalDialog(title = "Oops!", "The \"Row\" data cannot be all missing.",
                          easyClose = TRUE))
    return()
  }
  non.na.row.data <- row.data[non.na.idx]
  if (!all(is.finite(non.na.row.data)) || any(non.na.row.data < 0)) {
    showModal(modalDialog(title = "Oops!", "All numbers in the data should be non-negative.",
                          easyClose = TRUE))
    return()
  }
  if (!any(non.na.row.data > 0)) {
    showModal(modalDialog(title = "Oops!", "At least one number of the \"Row\" data should be positive.",
                          easyClose = TRUE))
    return()
  }
  na.idx <- which(is.na(row.data))
  if (length(na.idx) > 0) {
    if (min(na.idx) < max(non.na.idx)) {
      showModal(modalDialog(title = "Oops!", "The \"Row\" data cannot have any non-numeric value before a number.",
                            easyClose = TRUE))
      return()
    }
  }
  col.data <- data[,2]
  non.na.idx <- which(!is.na(col.data))
  if (length(non.na.idx) == 0) {
    showModal(modalDialog(title = "Oops!", "The \"Column\" data cannot be all missing.",
                          easyClose = TRUE))
    return()
  }
  non.na.col.data <- col.data[non.na.idx]
  if (!all(is.finite(non.na.col.data)) || any(non.na.col.data < 0)) {
    showModal(modalDialog(title = "Oops!", "All numbers in the data should be non-negative.",
                          easyClose = TRUE))
    return()
  }
  if (!any(non.na.col.data > 0)) {
    showModal(modalDialog(title = "Oops!", "At least one number of the \"Column\" data should be positive.",
                          easyClose = TRUE))
    return()
  }
  na.idx <- which(is.na(col.data))
  if (length(na.idx) > 0) {
    if (min(na.idx) < max(non.na.idx)) {
      showModal(modalDialog(title = "Oops!", "The \"Column\" data cannot have any non-numeric value before a number.",
                            easyClose = TRUE))
      return()
    }
  }
  row.data <- data[,1]
  col.data <- data[,2]
  row.data <- row.data[1:max(which(!is.na(row.data)))]
  col.data <- col.data[1:max(which(!is.na(col.data)))]
  num.row <- length(row.data)
  num.col <- length(col.data)
  # Compute marginal probabilities.
  row.prob <- row.data / sum(row.data)
  col.prob <- col.data / sum(col.data)
  # Compute joint probability distribution.
  joint.prob <- scale.conversion.computation(row.prob,
                                             col.prob)
  row.wise.prop <- sweep(joint.prob, 1, rowSums(joint.prob), "/")
  row.wise.prop[is.nan(row.wise.prop)] <- NA
  col.wise.prop <- sweep(joint.prob, 2, colSums(joint.prob), "/")
  col.wise.prop[is.nan(col.wise.prop)] <- NA
  s <- ""
  s <- paste0(s, "Scale Conversion Model Result\n")
  s <- paste0(s, "=============================\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Observed Data\n")
  s <- paste0(s, "-------------\n")
  s <- paste0(s, "   Row: ", paste(round(row.data,
                                         4), collapse=", "), "\n")
  s <- paste0(s, "Column: ", paste(round(col.data,
                                         4), collapse=", "), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Marginal Probabilities\n")
  s <- paste0(s, "----------------------\n")
  s <- paste0(s, "   Row: ", paste(round(row.prob,
                                         4), collapse=", "), "\n")
  s <- paste0(s, "Column: ", paste(round(col.prob,
                                         4), collapse=", "), "\n")
  s <- paste0(s, "\n\n")
  s <- paste0(s, "(Optimal) Joint Probabilities\n")
  s <- paste0(s, "-----------------------------\n")
  s <- paste0(s, matrix.string(round(joint.prob,
                                     4), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Row-wise Proportions\n")
  s <- paste0(s, "--------------------\n")
  s <- paste0(s, matrix.string(round(row.wise.prop,
                                     4), left.justified=FALSE))
  s <- paste0(s, "\n\n")
  s <- paste0(s, "Column-wise Proportions\n")
  s <- paste0(s, "-----------------------\n")
  s <- paste0(s, matrix.string(round(col.wise.prop,
                                     4), left.justified=FALSE))
  scale.conversion.show.result(s)
})

# Regression variable
rv$regression.data <- NULL
regression.tab.counter <- 0

output$regressionData <- renderRHandsontable({
  req(rv$regression.data)
  rhandsontable(rv$regression.data, useTypes = FALSE, height=500) %>%
    hot_table(overflow = "hidden", rowHeaderWidth = 100) %>%
    hot_cols(colWidths = 100) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$regressionDataCreate, {
  updateTabsetPanel(session, "regressionTab", selected = "Data")
  if (!string.is.dimension(input$regressionDataDimension, 2)) {
    showModal(modalDialog(title = "Oops!", "Invalid data dimensions.",
                          easyClose = TRUE))
    return()
  }
  d <- parse.dimension.string(input$regressionDataDimension)
  if (is.null(rv$regression.data)) {
    rv$regression.data <- empty.data2(d[1], d[2])
  } else if (!all(dim(rv$regression.data) == d)) {
    showModal(modalDialog(title = "Confirmation", "Modify the data dimensions?",
                          footer = tagList(actionButton("regressionDataModifyOk", "OK"),
                                           actionButton("regressionDataModifyCancel", "Cancel"))))
  }
})

observeEvent(input$regressionDataModifyOk, {
  d <- parse.dimension.string(input$regressionDataDimension)
  row <- min(nrow(rv$regression.data), d[1])
  col <- min(ncol(rv$regression.data), d[2])
  df2 <- hot_to_r(input$regressionData)
  if (nrow(rv$regression.data) >= d[1]) {
    row.name <- rownames(df2)[1:d[1]]
  } else {
    row.name <- c(rownames(df2), paste("Row", (nrow(df2) + 1):d[1]))
  }
  if (ncol(rv$regression.data) >= d[2]) {
    col.name <- colnames(df2)[1:d[2]]
  } else {
    col.name <- c(colnames(df2), paste("Column", (ncol(df2) + 1):d[2]))
  }
  df <- empty.data2(d[1], d[2], row.name, col.name)
  df[1:row,1:col] <- df2[1:row,1:col]
  rv$regression.data <- df
  removeModal()
})

observeEvent(input$regressionDataModifyCancel, {
  updateTextInput(session, "regressionDataDimension",
                  value = paste(dim(rv$regression.data), collapse = ","))
  removeModal()
})

observeEvent(input$regressionDataFile, {
  showModal(modalDialog(title = "File Format",
                        checkboxInput("regressionDataFileHeader", label = "Header?", value = TRUE),
                        checkboxInput("regressionDataFileRowName", label = "Row names?"),
                        footer = tagList(actionButton("regressionDataFileOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$regressionDataFileOk, {
  has.header <- input$regressionDataFileHeader
  has.rowname <- input$regressionDataFileRowName
  output <- tryCatch({
    if (has.rowname) {
      df <- read.csv(input$regressionDataFile$datapath, header=has.header,
                     check.name=FALSE, row.names=1, na.strings=NULL,
                     colClasses="character", strip.white=TRUE)
    } else {
      df <- read.csv(input$regressionDataFile$datapath, header=has.header,
                     check.name=FALSE, na.strings=NULL,
                     colClasses="character", strip.white=TRUE)
    }
  }, error=function(e) {
    return(NULL)
  }, warning=function(w) {
    return(NULL)
  })
  if (is.null(output)) {
    showNotification("Oops! Invalid data.", type = "message")
  } else {
    df <- output
    if (!has.header) {
      colnames(df) <- paste("Column", 1:ncol(df))
    } else {
      col.name <- colnames(df)
      for (j in 1:ncol(df)) {
        if (util.character.is.empty(col.name[j])) {
          col.name[j] <- paste("Column", j)
        }
      }
      colnames(df) <- col.name
    }
    if (!has.rowname) {
      rownames(df) <- paste("Row", 1:nrow(df))
    } else {
      row.name <- rownames(df)
      for (i in 1:nrow(df)) {
        if (util.character.is.empty(row.name[i])) {
          row.name[i] <- paste("Row", i)
        }
      }
      rownames(df) <- row.name
    }
    rv$regression.data <- util.data.frame.to.character(df)
    removeModal()
  }
  updateTabsetPanel(session, "regressionTab", selected = "Data")
})

output$regressionDataDownload <- downloadHandler(
  filename = "data.csv",
  content = function(file) {
    df <- hot_to_r(input$regressionData)
    write.table(df, file, sep = ",",
                row.names = input$regressionDataDownloadRowName,
                col.names = input$regressionDataDownloadHeader,
                quote = which(sapply(df, function(col) any(grepl(",", col)))),
                qmethod = "double")
  }
)

observeEvent(input$regressionDataClear, {
  showModal(modalDialog(title = "Confirmation", "Clear the data?",
                        footer = tagList(actionButton("regressionDataClearOk", "OK"),
                                         modalButton("Cancel"))))
})

observeEvent(input$regressionDataClearOk, {
  updateTabsetPanel(session, "regressionTab", selected = "Data")
  rv$regression.data <- NULL
  removeModal()
})

observe({
  if (is.null(rv$regression.data)) {
    updateTextInput(session, "regressionDataDimension", value = "")
    updateActionButton(session, "regressionDataCreate", label = "Create")
    shinyjs::disable("regressionDataDownload")
    shinyjs::disable("regressionDataClear")
  } else {
    updateTextInput(session, "regressionDataDimension",
                    value = paste(dim(rv$regression.data), collapse = ","))
    updateActionButton(session, "regressionDataCreate", label = "Modify")
    shinyjs::enable("regressionDataDownload")
    shinyjs::enable("regressionDataClear")
  }
})

observeEvent(input$regressionGo, {
  current.model.name <- input$regressionModelName
  if (current.model.name == "Logistic Regression") {
    regression.go.logistic()
  }
})

regression.go.logistic <- function() {
  # Verify the data is not empty.
  if (is.null(rv$regression.data)) {
    updateTabsetPanel(session, "regressionTab", selected = "Data")
    showModal(modalDialog(title = "Oops!", "There is no data.",
                          easyClose = TRUE))
    return()
  }
  df <- hot_to_r(input$regressionData)
  is.full.column <- !sapply(df, util.has.missing.data)
  
  is.binary.column <- sapply(df, function(x) length(unique(x[trimws(x) != ""]))) == 2
  if (!any(is.full.column & is.binary.column)) {
    msg <- "Data must have at least one binary column without missing observation."
    updateTabsetPanel(session, "regressionTab", selected = "Data")
    showModal(modalDialog(title = "Oops!", msg, easyClose = TRUE))
    return()
  }
  if (sum(is.full.column) < 2) {
    msg <- "Data must have at least 2 columns without missing observation."
    updateTabsetPanel(session, "regressionTab", selected = "Data")
    showModal(modalDialog(title = "Oops!", msg, easyClose = TRUE))
    return()
  }
  full.column <- colnames(df)[is.full.column]
  binary.column <- colnames(df)[is.binary.column & is.full.column]
  is.numeric.column <- sapply(df, util.is.all.numeric)
  numeric.column <- colnames(df)[is.numeric.column & is.full.column]
  showModal(modalDialog(title = "Logistic Regression",
                        helpText('Step 1: Select response variable'),
                        selectInput("logisticRegressionResponse", label = "Response Variable",
                                    choices = c("(Select a variable)", binary.column)),
                        helpText('Step 2: Select explanatory variable(s)'),
                        fluidRow(
                          column(width = 6,
                                 checkboxGroupInput("logisticRegressionContinuous",
                                                    label="Continuous",
                                                    choices=numeric.column)
                          ),
                          column(width = 6,
                                 checkboxGroupInput("logisticRegressionDiscrete",
                                                    label="Discrete",
                                                    choices=full.column)
                          )
                        ),
                        footer = tagList(actionButton("logisticRegressionOk", "OK"),
                                         modalButton("Cancel"))))
}

observeEvent(input$logisticRegressionResponse, {
  x.c <- input$logisticRegressionContinuous
  is.y <- x.c == input$logisticRegressionResponse
  if (any(is.y)) {
    updateCheckboxGroupInput(session, "logisticRegressionContinuous",
                             selected = x.c[!is.y])
  }
  x.d <- input$logisticRegressionDiscrete
  is.y <- x.d == input$logisticRegressionResponse
  if (any(is.y)) {
    updateCheckboxGroupInput(session, "logisticRegressionDiscrete",
                             selected = x.d[!is.y])
  }
})

observeEvent(input$logisticRegressionContinuous, {
  x.c <- input$logisticRegressionContinuous
  if (input$logisticRegressionResponse %in% x.c) {
    updateSelectInput(session, "logisticRegressionResponse",
                      selected = "(Select a variable)")
  }
  x.d <- input$logisticRegressionDiscrete
  diff <- setdiff(x.d, x.c)
  if (!setequal(x.d, diff)) {
    updateCheckboxGroupInput(session, "logisticRegressionDiscrete",
                             selected = diff)
  }
})

observeEvent(input$logisticRegressionDiscrete, {
  x.d <- input$logisticRegressionDiscrete
  if (input$logisticRegressionResponse %in% x.d) {
    updateSelectInput(session, "logisticRegressionResponse",
                      selected = "(Select a variable)")
  }
  x.c <- input$logisticRegressionContinuous
  diff <- setdiff(x.c, x.d)
  if (!setequal(x.c, diff)) {
    updateCheckboxGroupInput(session, "logisticRegressionContinuous",
                             selected = diff)
  }
})

observeEvent(input$logisticRegressionOk, {
  if (input$logisticRegressionResponse == "(Select a variable)") {
    showNotification("Oops! Select a response variable.", type = "message")
    return()
  }
  if (length(input$logisticRegressionContinuous) +
      length(input$logisticRegressionDiscrete) == 0) {
    showNotification("Oops! Select at least one explanatory variable.", type = "message")
    return()
  }
  df <- hot_to_r(input$regressionData)
  y <- input$logisticRegressionResponse
  x.cont <- input$logisticRegressionContinuous
  x.factor <- input$logisticRegressionDiscrete
  removeModal()
  regression.do.logistic(df, y, x.cont, x.factor)
})

regression.show.result <- function(s, title, select=TRUE) {
  x <- regression.tab.counter <<- regression.tab.counter + 1
  text.id <- paste0("regressionResult", x)
  
  appendTab("regressionTab",
            tabPanel(paste(x, title),
                     tags$head(tags$style(gsub('%x', text.id, "#%x {white-space: pre; font-family: monospace;}"))),
                     br(), textOutput(text.id), br(), 
                     actionButton(paste0("regressionResultClose", x),
                                  label = "Close"),
                     br(), br()),
            select = select)
  
  output[[text.id]] <- renderText({s})
  
  code <- gsub('%title', title, gsub('%x', x,
                                     'observeEvent(input$regressionResultClose%x, {
        removeTab("regressionTab", "%x %title")
      })', fixed=TRUE), fixed=TRUE)
  eval(parse(text=code))
}

regression.show.plot <- function(obj, title, select=TRUE) {
  x <- regression.tab.counter <<- regression.tab.counter + 1
  plot.id <- paste0("regressionResult", x)
  
  appendTab("regressionTab",
            tabPanel(paste(x, title),
                     br(), plotOutput(plot.id, height = "600px"),
                     actionButton(paste0("regressionResultClose", x),
                                  label = "Close"),
                     br(), br()),
            select = select)
  
  output[[plot.id]] <- renderPlot(obj)
  
  code <- gsub('%title', title, gsub('%x', x,
                                     'observeEvent(input$regressionResultClose%x, {
        removeTab("regressionTab", "%x %title")
      })', fixed=TRUE), fixed=TRUE)
  eval(parse(text=code))
}

regression.do.logistic <- function(df, y, x.cont, x.factor) {
  actual.y <- df[[y]]
  df[[y]] <- as.factor(df[[y]])
  for (col in x.cont) {
    df[[col]] <- as.numeric(df[[col]])
  }
  for (col in x.factor) {
    df[[col]] <- as.factor(df[[col]])
  }
  # Fit model.
  f <- as.formula(paste(paste0("`", y, "`"), "~",
                        paste(sapply(c(x.cont, x.factor), function(col) paste0("`", col, "`")),
                              collapse=" + ")))
  output <- tryCatch({
    model <- glm(f, family=binomial, data=df)
  }, error=function(e) {
    return(NULL)
  }, warning=function(w) {
    return(NULL)
  })
  model <- output
  s <- ""
  s <- paste0(s, "Logistic Regression Result\n")
  s <- paste0(s, "==========================\n")
  s <- paste0(s, "\n")
  if (is.null(model)) {
    s <- paste0(s, "Error: The model is saturated.  The GLM algorithm did not converge.\n")
    regression.show.result(s, "Logistic Regression")
    return()
  }
  if (any(is.na(model$coefficients))) {
    s <- paste0(s, "Error: There is multicollinearity among the explanatory variables.\n")
    regression.show.result(s, "Logistic Regression")
    return()
  }
  s <- paste0(s, "\n")
  s <- paste0(s, "Parameter Estimates\n")
  s <- paste0(s, "-------------------\n")
  model.summary <- summary(model)
  model.coef <- model.summary$coefficients
  n <- nrow(df)
  p <- nrow(model.coef)
  m <- matrix("", 2 + p, 7)
  m[1,6] <- "95% C.I."
  m[2,2:7] <- c(colnames(model.coef), "Lower Bound", "Upper Bound")
  m[3:(2+p),1] <- rownames(model.coef)
  m[3:(2+p),2:4] <- round(model.coef[,1:3], 4)
  for (i in seq_len(p)) {
    if (model.coef[i,4] < 0.0001) {
      m[2+i,5] <- "<0.0001"
    } else {
      m[2+i,5] <- round(model.coef[i,4], 4)
    }
  }
  signif.level <- 0.05
  z.alpha <- qnorm(1 - signif.level / 2)
  m[3:(2+p),6] <- round(model.coef[,1] - z.alpha * model.coef[,2],
                        4)
  m[3:(2+p),7] <- round(model.coef[,1] + z.alpha * model.coef[,2],
                        4)
  s <- paste0(s, util.matrix.to.string(m, spacing=3), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Odds Estimates\n")
  s <- paste0(s, "--------------\n")
  m <- matrix("", 2 + (p - 1), 6)
  m[1,5] <- "95% C.I."
  m[2,2:6] <- c("Odds", "Wald Chi-Squared", "Pr(>w)", "Lower Bound",
                "Upper Bound")
  m[3:(1+p),1] <- rownames(model.coef)[2:p]
  odd <- exp(model.coef[2:p,1])
  m[3:(1+p),2] <- round(odd, 4)
  m[3:(1+p),3] <- round(model.coef[2:p,3] ^ 2, 4)
  p.value <- model.coef[2:p,4]
  for (i in seq(2, p)) {
    if (model.coef[i,4] < 0.0001) {
      m[1+i,4] <- "<0.0001"
    } else {
      m[1+i,4] <- round(model.coef[i,4], 4)
    }
  }
  lower.odd <- exp(model.coef[2:p,1] - z.alpha * model.coef[2:p,2])
  m[3:(1+p),5] <- round(lower.odd, 4)
  upper.odd <- exp(model.coef[2:p,1] + z.alpha * model.coef[2:p,2])
  m[3:(1+p),6] <- round(upper.odd, 4)
  s <- paste0(s, util.matrix.to.string(m, spacing=3), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Hosmer-Lemeshow Goodness of Fit Test\n")
  s <- paste0(s, "------------------------------------\n")
  m <- matrix("", 2, 2)
  m[1:2,1] <- c("-2 * Log-Likelihood", "AIC")
  m[1,2] <- round(model$deviance, 4)
  m[2,2] <- round(model$aic, 4)
  s <- paste0(s, util.matrix.to.string(m, spacing=3), "\n")
  s <- paste0(s, "\n")
  m <- matrix("", 2, 2)
  m[1:2,1] <- c("Chi-Squared Statistic", "p-value")
  numeric.y <- as.numeric(df[[y]]) - 1
  fitted.prob <- fitted(model)
  result <- regression.hoslem.test(numeric.y, fitted.prob)
  m[1,2] <- round(result$chi.sq, 4)
  if (result$p.value < 0.0001) {
    m[2,2] <- "<0.0001"
  } else {
    m[2,2] <- round(result$p.value, 4)
  }
  s <- paste0(s, util.matrix.to.string(m, spacing=3), "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "\n")
  s <- paste0(s, "Predicted Responses\n")
  s <- paste0(s, "-------------------\n")
  m <- matrix("", 1 + n, 3)
  m[1,] <- c("Obs", "Observed y",
             paste0("Predicted P(Y=", levels(df[[y]])[2], "|x)"))
  m[2:(1+n),1] <- 1:n
  m[2:(1+n),2] <- actual.y
  m[2:(1+n),3] <- round(fitted(model), 4)
  s <- paste0(s, util.matrix.to.string(m, spacing=3), "\n")
  regression.show.result(s, "Logistic Regression")
  
  # Create forest plot tab.
  plot.obj.log.reg.1 <- regression.create.forest.graph.obj(odd, lower.odd,
                                                           upper.odd, rownames(model.coef)[2:p], p.value, signif.level)
  regression.show.plot(plot.obj.log.reg.1, "Forest Plot", select=FALSE)
  
  # Compute true/false positive/negative.
  tau <- sort(unique(fitted.prob))
  num.tau <- 100
  if (length(tau) < num.tau) {
    tau <- seq(from=tau[1], to=tail(tau, 1), length.out=num.tau)
  }
  num.tau <- length(tau)
  num.tp <- numeric(num.tau)
  num.fp <- numeric(num.tau)
  num.fn <- numeric(num.tau)
  for (i in seq_len(num.tau)) {
    idx <- fitted.prob >= tau[i]
    num.tp[i] <- sum(numeric.y[idx])
    num.fp[i] <- sum(idx) - num.tp[i]
    num.fn[i] <- sum(numeric.y[!idx])
  }
  n <- length(numeric.y)
  num.tn <- n - num.tp - num.fp - num.fn
  # Compute true positive rate i.e. sensitivity.
  tpr <- num.tp / (num.tp + num.fn)
  tpr.margin <- z.alpha * sqrt(tpr * (1 - tpr) / (num.tp + num.fn))
  tpr.lower <- pmax(tpr - tpr.margin, 0)
  tpr.upper <- pmin(tpr + tpr.margin, 1)
  # Compute false positive rate i.e. 1 - specificity.
  fpr <- num.fp / (num.fp + num.tn)
  fpr.margin <- z.alpha * sqrt(fpr * (1 - fpr) / (num.fp + num.tn))
  fpr.lower <- pmax(fpr - fpr.margin, 0)
  fpr.upper <- pmin(fpr + fpr.margin, 1)
  # Compute accuracy.
  acc <- (num.tp + num.tn) / n
  is.best <- acc == max(acc)
  # Compute AUC.
  trapezoid.area <- c(0, rev(0.5 * (tpr[1:(num.tau-1)] + tpr[2:num.tau]) *
                               (fpr[1:(num.tau-1)] - fpr[2:num.tau])))
  auc <- cumsum(trapezoid.area)
  
  # Create ROC plot tab.
  plot.obj.log.reg.2 <- regression.create.roc.graph.obj(tpr, tpr.lower,
                                                        tpr.upper, fpr, fpr.lower, fpr.upper, tail(auc, 1), is.best)
  regression.show.plot(plot.obj.log.reg.2, "ROC Plot", select=FALSE)
  # Create accuracy plot tab.
  plot.obj.log.reg.3 <- regression.create.acc.graph.obj(tau, acc, is.best)
  regression.show.plot(plot.obj.log.reg.3, "Accuracy Plot", select=FALSE)
  # Create cumulative AUC plot tab
  plot.obj.log.reg.4 <- regression.create.auc.graph.obj(rev(tpr), auc, rev(is.best))
  regression.show.plot(plot.obj.log.reg.4, "Cumulative AUC Plot", select=FALSE)
}

session$onSessionEnded(stopApp)
}

app <- shinyApp(ui = ui, server = server)
print(app)