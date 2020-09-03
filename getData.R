packrat::init()

makeCSV = TRUE
makeHeatmap = FALSE
makeMonthGraph = FALSE

library(dotenv)

dotenv::load_dot_env(file = ".env")

library(dplyr)
library(purrr)
library(jsonlite)
library(data.table)
library(stringr)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

dataDirectory = Sys.getenv("DATA_DIRECTORY")

Files = list.files(
  path = dataDirectory,
  pattern = "^(heart_rate)+.*.json",
  full.names = TRUE
)

rawData = purrr::map_df(Files, function(x) {
  jsonlite::fromJSON(x)
})

rawData = as.data.table(rawData)

data.table::setDT(rawData)[, "date" := format(as.Date(data.table::tstrsplit(dateTime, " ")[[1]], format = "%m/%d/%y"), "%d/%m/%y")]
data.table::setDT(rawData)[, "time" := substr(data.table::tstrsplit(dateTime, " ")[[2]], 1, 5)]

rawData = subset(rawData, select = -c(value.confidence, dateTime))

pivotData = data.table::dcast(
  rawData,
  date ~ time,
  value.var = "value.bpm",
  drop = FALSE,
  fun.aggregate = function(x) as.integer(median(x))
)

pivotData = pivotData[order(as.Date(pivotData$date, format = "%d/%m/%y")),]

pivotData_Time = subset(pivotData, select = -c(date))



#######################################################
#     ------------------< Csv >------------------     #
#######################################################

if (isTRUE(makeCSV)) {

  write.table(pivotData,
    file = "./heartrate.csv",
    na = "",
    sep = ",",
    row.names = FALSE
  )

}


#######################################################
#     ----------------< Heatmap >----------------     #
#######################################################

if (isTRUE(makeHeatmap)) {

  heatmapFrame = data.frame()

  heatmapMinBpm = 1
  heatmapMaxBpm = 200

  columnNames = names(pivotData_Time)

  for (columnName in columnNames) {
    print(columnName)
    tempVector = c(rep(0, heatmapMaxBpm))
    for (i in heatmapMinBpm:heatmapMaxBpm) {
      for (row in 1:nrow(pivotData_Time)) {
        if (isTRUE(pivotData_Time[[row, columnName]] == i)) {
          tempVector[i] = tempVector[i] + 1
        }
      }
    }
    heatmapFrame[columnName] = tempVector
  }

  graphMinBpm = 50
  graphMaxBpm = 130

  heatmap = plotly::plot_ly(
    z = data.matrix(heatmapFrame %>% dplyr::slice(graphMinBpm:graphMaxBpm)),
    x = columnNames,
    y = c(graphMinBpm:graphMaxBpm),
    colors = colorRamp(c("black", "red")),
    type = "heatmap"
  )
  # heatmap

  heatmapApp = dash::Dash$new()
  heatmapApp$layout(
    dashHtmlComponents::htmlDiv(
      list(
        dashCoreComponents::dccGraph(figure = heatmap)
      )
    )
  )

  heatmapApp$run_server()

}


#######################################################
#     --------------< Month Graph >--------------     #
#######################################################

if (isTRUE(makeMonthGraph)) {

  graphMinBpm = 50
  graphMaxBpm = 130

  median.noText = function(x) {
    if (isTRUE(is.character(x[1]))) {
      return(x[1])
    }
    return(as.numeric(round(median(x, na.rm = TRUE))))
  }

  monthlyData = data.table::data.table(pivotData)

  data.table::setDT(monthlyData)[, "date" := stringr::str_c("01/", substr(date, 4, 8))]

  monthlyData = aggregate.data.frame(
    monthlyData,
    by = list(monthlyData$date),
    FUN = median.noText,
    drop = FALSE
  )

  monthlyData = subset(monthlyData, select = -c(Group.1))
  monthlyData = monthlyData[order(as.Date(monthlyData$date, format = "%d/%m/%y")),]
  data.table::setDT(monthlyData)[, "date" := substr(date, 4, 8)]

  monthGraph = plotly::plot_ly(
    monthlyData,
    x = names(monthlyData)[-1],
    height = 800
  )

  for (i in 1:nrow(monthlyData)) {
    rowName = monthlyData$date[[i]]
    monthGraph = monthGraph %>% plotly::add_trace(
      y = stats::filter(lapply(as.vector(t(monthlyData[i,]))[-1], function(x) as.integer(x)), rep(1/30, 30), side=2),
      name = rowName,
      type = 'scatter',
      mode = 'lines',
      line = list(shape = "spline")
    )
  }


  monthGraphApp = dash::Dash$new()
  monthGraphApp$layout(
    dashHtmlComponents::htmlDiv(
      list(
        dashCoreComponents::dccGraph(figure = monthGraph)
      )
    )
  )

  monthGraphApp$run_server()

}

