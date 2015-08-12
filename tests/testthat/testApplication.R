test_that("empty", {
    app <- Application$new()
    
    expect_equal(app$empty(NULL), TRUE)
    expect_equal(app$empty(NA), TRUE)
    expect_equal(app$empty(""), TRUE)
    expect_equal(app$empty(0), TRUE)
    expect_equal(app$empty(c()), TRUE)
    expect_equal(app$empty(c(NA)), TRUE)
    expect_equal(app$empty(c("", "")), TRUE)
    expect_equal(app$empty(list()), TRUE)
    
    expect_equal(app$empty(c("", 1)), FALSE)
})

test_that("logging error channel", {
    app <- Application$new()
    app$setDebugMode(TRUE)    
    time <- format(Sys.time(), "%Y")

    # error channel
    con <- textConnection("messages", "w")
    sink(con, type = "message")
    app$logDebug("Test debug1 message")
    app$logInfo("Test info1 message")
    app$logError("Test error1 message")
    sink(NULL, type = "message")
    close(con)

    messages <- paste(messages, collapse = " ")    
    expect_match(messages, time)
    expect_that(messages, not(matches('info1')))
    expect_that(messages, not(matches('debug1')))
    expect_that(messages, matches('error1'))
})

test_that("logging info channel", {
    app <- Application$new()
    app$setDebugMode(TRUE)
    time <- format(Sys.time(), "%Y")
    con <- textConnection("messages", "w")
    sink(con, type = "output")
    app$logDebug("Test debug2 message")
    app$logInfo("Test info2 message")
    app$logError("Test error2 message")
    sink(NULL, type = "output")
    close(con)

    messages <- paste(messages, collapse = " ")
    expect_match(messages, time)
    expect_that(messages, matches('info2'))
    expect_that(messages, matches('debug2'))
    expect_that(messages, not(matches('error2')))
})

test_that("logging info channel without debug", {
    app <- Application$new()
    app$setDebugMode(FALSE)
    time <- format(Sys.time(), "%Y")
    con <- textConnection("messages", "w")
    sink(con, type = "output")
    app$logDebug("Test debug3 message")
    app$logInfo("Test info3 message")
    app$logError("Test error3 message")
    sink(NULL, type = "output")
    close(con)

    messages <- paste(messages, collapse = " ")    
    expect_match(messages, time)
    expect_that(messages, matches('iddnfo3'))
    expect_that(messages, not(matches('debug3')))
    expect_that(messages, not(matches('error3')))
})
