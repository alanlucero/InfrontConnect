library(jsonlite)
library(httr)
library(plyr)

#' @export
.feedmap <- read.csv(url("http://alanlucero.kissr.com/feeds.csv"))

#' @export
InfrontConnect <- function(username, password) {
    .username <<- username
    .password <<- password
    cat("Connected to Infront Desktop API for R \n")
    cat("\n")
    cat("**Disclaimer** \n")
    cat(" End-User agrees not to redistribute any such Information and to comply with any \n
    restrictions placed on such information by the providers thereof, hereunder but \n 
    not limited to acceptance of and compliance with Data Providers’ and/or other \n 
    third party license agreements. \n 
    Customer agrees to indemnify and keep indemnified Infront and its affiliates harmless \n 
    from and against any loss, damage, liability, cost, charges and expenses, including \n 
    reasonable legal fees, arising out of any breach on part of Customer with respect to \n 
    its obligations to obtain prior approvals from appropriate Data Providers and to \n 
    comply with any applicable, conditions, restrictions, or limitations imposed by such \n Data Providers.")

}
#' @export
FeedParser <- function(string) {
    feed_ticker <- as.list(strsplit(string, ":")[[1]])
    feed_id <- feed_ticker[[1]]

    feednu <- .feedmap$feednu[which(feed_id == .feedmap$feedcode)]
    return(feednu)
}
#' @export
TickerParser <- function(string) {
    feed_ticker <- as.list(strsplit(string, ":")[[1]])
    ticker_id <- feed_ticker[[2]]

    return(ticker_id)
}
#' @export
ListToJSON <- function(string) {
    instruments = c()
    for (inst in c(1:length(string))) {
        list = list(ticker = TickerParser(string[inst]), feed = FeedParser(string[inst]))

        instruments[length(instruments) + 1] <- list(list)
    }
    return(instruments)
}
#' @export
ListAppend <- function(existinglist, itemtoadd) {
    returnvalue <- c(existinglist, itemtoadd)
    return(returnvalue)
}

#' GetHistory Function
#'
#' Function to download historical time series with the Infront terminal
#' @param tickers Insert tickers in the format "FEED:TICKER" as a string
#' @param fields Data fields to export
#' @param start_date Starting date of the time series
#' @param end_date End date of the time series
#' @keywords GetHistory History Time Series
#' @export
#' @examples GetHistory(c("OSS:STL","OSS:YAR"), c("LAST"), "2017-01-13", "2017-01-18")
#' GetHistory()
#' @export
GetHistory <- function(tickers, fields, start_date, end_date) {

    if (missing(tickers)) {
        stop("You need to input a feed and market symbol as a list with items of class 'character'. \n E.g. \nFEED:TICKER\n, or c(\"LSE:AAL\",\"OSS:STL\"")
    }
    if (missing(fields)) {
        fields = c("LAST")
    }
    if (!is.character(tickers)) {
        stop("Symbols inputs must be of class and a list with items of class 'character': \n E.g.  c(\"NYSE:MSFT\",\"LSE:AAL\") ")
    }
    if (!missing(fields)) {
        if (!is.character(fields)) {
            stop("Fields inputs must be of a list with items of class 'character': \n E.g.  c(\"bid\",\"ask\") ")
        }
    }
    if (missing(start_date)) {
        stop("'start_date' input must be a string of class 'character' in the format 'YYYY-MM-DD' ")
    }
    if (missing(end_date)) {
        stop("'end_date' input must be a string of class 'character' in the format 'YYYY-MM-DD' ")
    }
    if (!is.character(start_date)) {
        stop("'start_date' input must be a string of class 'character' in the format 'YYYY-MM-DD'")
    }
    if (!is.character(end_date)) {
        stop("'start_date' input must be a string of class 'character' in the format 'YYYY-MM-DD'")
    }
    
    request_payload = list(
                           user = .username,
                           password = .password,
                           context = "user specific context",
                           historical_request = list(
                               fields = ListAppend(fields, "DATE"),
                               start_date = start_date,
                               end_date = end_date,
                               instruments = ListToJSON(tickers)
                                    )
                           )

    req_post <- POST("https://eod.infrontservices.com/historical/requests", body = request_payload,encode = "json")
    req_resp <- fromJSON(content(req_post, "text", encoding = "utf-8"))
    req_get <- read_json(req_resp$historical_response$full_response_url)

    while (req_get$error_code != 0) {
        if (req_get$error_code == 1) {
            req_get <- read_json(req_resp$historical_response$full_response_url)
        }
        else if (req_get$error_code > 1) {
            cat("SYNTAX ERROR: Please review your request.")
        }
        else {
            break
        }
    }
    hist_data = req_get$historical_data
    tickers_ <- tickers
    inst_list <- list()

    for (item in c(1:length(tickers_))) {
        instr = hist_data[[item]]
        ticker = instr$ticker
        add_item = ldply(instr$historical_trades, data.frame)
        rownames(add_item) <- add_item$date
        add_item$date <- NULL

        inst_list[ticker] <- list(add_item)
    }

return(inst_list)

cat("Data Succesfully imported!")

}
