## possible if statement - similar to Yann - not working
check_log <- function(data, log, variable, old_log_var, new_log_var, uuid_data, uuid_log) {
   
  # Check that the inputs are correct             
  #testthat::expect_that(log[[uuid_log]] %in% data[[uuid_data]],
  #                      "Things should be written here")
  
  #testthat::expect_that(log[[variable]] %in% names(data),
  #                      "Things should be written here")

  
  # Extract cleaning log
  extract_value <- function(data, 
                            log,
                            variable,
                            uuid_data,
                            uuid_log) {
    # Indexaction
    row.i<- which(data[[uuid_data]] == log[[uuid_log]])
    # Extraction
    value.c <- data[row.i, log[[variable]]]
    value.r <- data.frame(value.c, 
                          # unique id to join later
                          binding = paste0(log[[uuid_log]], log[[variable]]))
    names(value.r) <- c("value_clean", "binding")
    return(value.r)
    
  }
  
    log.check <- mapply(extract_value, 
                        log = split(log, row.names(log)), 
                        variable = variable, 
                        MoreArgs = list(
                        data = data,
                        uuid_data = uuid_data,
                        uuid_log = uuid_log
                        ),
                           SIMPLIFY = F) %>% do.call(rbind, .)
    
    #return(log.check)
    
    
    final <- log %>% 
      mutate(binding = paste0(log[[uuid_log]], log[[variable]])) %>%
      left_join(log.check) %>% 
      mutate(var_extracted = value_clean) %>%
      select(c(uuid_log, variable, old_log_var, new_log_var, var_extracted)) %>%
      mutate(check = log[[new_log_var]] == var_extracted)
    
    return(final)
    

}




















