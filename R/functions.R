# season_adj takes a dataframe, converts it to a tsibble, performs a decomposition of the time series,
# extracts the seasonally adjusted data from the components of the decomposition, then converts the output to a tibble.
season_adj <- function(df, with_ref_week = FALSE, model){
  tsble <-df%>%
    tsibble::as_tsibble(index = date)
  if(model=="stl"){
    dcmp <- tsble%>%
      fabletools::model(feasts::STL(value,
                                    robust = TRUE))
  }else if(model=="x11"){
    if(with_ref_week==TRUE){
      dcmp <- tsble%>%
        fabletools::model(feasts::X_13ARIMA_SEATS(value~ x11(),
                                                  xreg = refwk_ts,
                                                  regression.usertype = "holiday",
                                                  estimate.maxiter=10000))
    }else{
      dcmp <- tsble%>%
        fabletools::model(feasts::X_13ARIMA_SEATS(value~ x11()))
    }
  }else{
    stop("model must be stl, x11")
  }
  fabletools::components(dcmp)%>%
    select(date, season_adjust)%>%
    as_tibble()
}

#possibly_season_adj ensures that if X11 fails to converge it does not cause the script to stop. 
possibly_season_adj = purrr::possibly(.f = season_adj, otherwise = NULL)

#na_mean takes the mean of two values where one value is possibly NA. It returns the mean of the non-NA values or throws an error. 
na_mean <- function(val1, val2){
  vec <-c(val1, val2) 
  assertthat::assert_that(sum(!is.na(vec))!=0, msg="Both values can't be NA")
  sum(vec, na.rm = TRUE)/sum(!is.na(vec))
}
