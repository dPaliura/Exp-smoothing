source("validation.R", echo=FALSE)


input.buffer.dir <- "inputBuff"


read.one.of.lines <- function(words , prompt="", prompt.rep=FALSE, ignore.case=FALSE, tolower=FALSE){
    cat(prompt)
    while (TRUE) {
        answer <- ifelse(prompt.rep, readline(prompt), readline())
        if (ifelse(test = ignore.case,
                   yes = tolower(answer) %in% tolower(words),
                   no = answer %in% words)){
            return(if (tolower) tolower(answer) else answer)
        }
    }
}


ask.for.yes.no <- function(prompt, yes="y", no="n", prompt.rep=FALSE){
    answer <- tolower(read.one.of.lines(c(yes, no), prompt=prompt,
                      prompt.rep = prompt.rep, ignore.case = TRUE))
    if (answer == yes) return(TRUE)
    else if(answer == no) return(FALSE)
}


ask.for.abs.path <- function(){
    answer <- tolower(read.one.of.lines(c("y", "n", "x"), 
                                        paste0("Do you want to use absolute path?\n",
                                              "y - yes\n",
                                              "n - no, use inputBuff folder\n",
                                              "x - eXit\n"),
                                        ignore.case = TRUE))
    if (answer == "y") return(TRUE)
    else if(answer == "n") return(FALSE)
    else if(answer == "x") return(NULL)
}


get.file.name <- function(path){
    while (TRUE){
        dir.path <- dir(path)
        files <- grep("\\.csv$", dir.path, value=TRUE)
        if (length(files)){
            files.df <- data.frame("available.files"=files)
            while (TRUE){
                print(files.df)
                answer <- read.one.of.lines(
                    words = c(files, 1:length(files), "b","B","x","X"),
                    prompt = paste0(
                            "\nWrite name or number of one from available files\n",
                            "(b - back to choose path, x - eXit)\n"))
                
                low.answer <- tolower(answer)
                if (low.answer == "x") return(NULL)
                if (low.answer == "b") return(NA)
                
                if(length(grep("^\\d+$", answer))){
                    return(files.df[answer,1])
                }
                else return(answer)
            }
        }
        else{
            cat("No available files in this folder.",
                "\nIf You want to use it, You have to upload your",
                ".csv file in this folder.",
                "\nDo you want to retry?\n")
            if (!ask.for.yes.no(paste0("y - yes, retry this path\n",
                                      "n - no, change path\n"))){
                return(NA)
            }
        }
    }
}


get.path <- function(){
    repeat {
        is.absolute.path <- ask.for.abs.path()
        if (is.null(is.absolute.path)) return(NULL)
        
        if(is.absolute.path){
            repeat {
                cat("Write path to folder with your .csv file",
                    "\n(b - back, x - eXit)\n")
                answer <- readline()
                low.answer <- tolower(answer)
                if (low.answer == "x") return(NULL)
                if (low.answer == "b"){
                    path <- NA
                    break()
                }
                
                repeat {
                    path.dir <- dir(answer)
                    if (length(path.dir)){
                        path <- answer
                        break()
                    }
                    else{
                        cat("Specified folder does not exist or empty. ",
                            "\nYou can create or fill it with file and try it again.\n")
                        if(!ask.for.yes.no(paste0("Do you want to retry your path?\n",
                                                  "y - yes, retry\n",
                                                  "n - no, try else one\n"))){
                            path <- NA
                            break()
                        }
                    }
                }
                
                if (!is.na(path)) break()
            }
        }
        else {
            path <- input.buffer.dir
        }
        
        if (!is.na(path)){
            fname <- get.file.name(path)
            if (is.null(fname)) return(NULL)
            if (!is.na(fname)) return(paste0(path, "/", fname))
        }
    }
}


get.data <- function(){
    options <- c("y", "n", "b", "x")
    repeat {
        path <- get.path()
        if (is.null(path)) return(NULL)
        
        repeat {
            answer <- read.one.of.lines( 
                words = options,
                prompt = paste0(
                    "Use default parameters to read file?\n",
                    "first row - feature names\n",
                    "separator - ','\n",
                    "decimal - '.'\n",
                    "y - yes\n",
                    "n - no, change parameters\n",
                    "b - back to change path\n",
                    "x - eXit\n"),
                ignore.case = TRUE,
                tolower = TRUE)
            
            if (answer == "x") return(NULL)
            if (answer == "b") break()
            if (answer == "y") return(read.csv(path))
            else {
                repeat {
                    answer <- read.one.of.lines(
                        words = options,
                        prompt = paste0(
                            "Use firs row as feature names?\n",
                            "y - yes\n",
                            "n - no\n",
                            "b - back\n",
                            "x - eXit\n"),
                        ignore.case=TRUE,
                        tolower = TRUE)
                    
                    if (answer == "x") return(NULL)
                    if (answer == "b"){
                        break()
                    }
                    header <- answer == "y"
                    
                    repeat {
                        answer <- read.one.of.lines(
                            words = options,
                            prompt = paste0(
                                "Use ',' (comma) as separator between numbers?\n",
                                "y - yes\n",
                                "n - no, use ';' (point-comma)\n",
                                "b - back\n",
                                "x - eXit\n"),
                            ignore.case=TRUE,
                            tolower = TRUE)
                        if (answer == "x") return(NULL)
                        if (answer == "b") break()
                        sep <- if (answer=="y") "," else ";"
                        
                        repeat {
                            answer <- read.one.of.lines(
                                words = options,
                                prompt = paste0(
                                    "Use '.' (point) as decimal separator?\n",
                                    "y - yes\n",
                                    "n - no, use ',' (comma)\n",
                                    "b - back\n",
                                    "x - eXit\n"),
                                ignore.case=TRUE,
                                tolower = TRUE)
                            if (answer == "x") return(NULL)
                            if (answer == "b") break()
                            dec <- if (answer=="y") "." else ","
                            return(read.csv(path, header = header,
                                            sep = sep, dec = dec))
                        }
                    }
                }
            }
        }
    }
}


get.time.series <- function(){
    repeat {
        df <- get.data()
        if (is.null(df)) return(NULL)
        
        if (nrow(df) > 5){
            valids <- 
            available.cols <- which(sapply(1:ncol(df),
                                            function(num){check.ts(df[,num])}))
            if(length(available.cols) > 1){
               df <- df[,available.cols]
               fields <- colnames(df)
               
               cat("Available fields\n")
               print(summary(df))
               answer <- read.one.of.lines(
                                words = c(fields, c("b","x","B","X")),
                                prompt = paste0("Type name of field to use it in model\n",
                                                "(b - back to choose path, x - eXit)\n"))
               low.answer <- tolower(answer)
               if (low.answer == "x") return(NULL)
               if (low.answer != "b"){
                   return(df[,answer])
               }
            }
            else if(length(available.cols)==1){
                return(df[,available.cols])
            }
            else {
                cat("No available fields in you file.",
                    "\nProbably there is no numeric fields or NA's in each field.",
                    "\nAlso parameters for file reading could be specified incorrectly.",
                    "\nGoing back to choose file path.\n\n")
            }
        }
        else {
            cat("Expected at least 5 observations in data set.",
                "\nGoing back to choose file path.\n\n")
        }
    }
}


get.model.param <- function(prompt, prints=TRUE, back="b", exit="x"){
    cat(prompt)
    repeat {
        answer <- readline()
        low.answer <- tolower(answer)
        if (low.answer == exit) return(NULL)
        if (low.answer == back) return(NA)
        
        if (check.param(answer, prints = prints)) {
            return (as.numeric(answer))
        }
    }
}


get.integer <- function(prompt, prints=TRUE, min.val=NULL, back="b", exit="x"){
    cat(prompt)
    repeat {
        answer <- readline()
        low.answer <- tolower(answer)
        if (low.answer == exit) return(NULL)
        if (low.answer == back) return(NA)
        
        if (check.integer(answer, prints = prints)) {
            num <- as.numeric(answer)
            if (is.null(min.val)){
                return(num)
            }
            else{
                if (num < min.val){
                    if (prints) cat("Value must be not less than",
                                    min.val,"\n")
                }
                else return(num)
            }
        }
    }
}


get.season.period <- function(ts.size){
    cat("Input season period",
        "\n(b - back to conditions, x - eXit)\n")
    repeat {
        period <- get.integer("", min.val = 2)
        if (period > ts.size/2){
            cat("To large period. Data must contain at least 2 periods\n")
        }
        else return(period)
    }
}


get.input <- function(){
    default.alpha <- 0.5
    default.beta  <- 0.5
    default.theta <- 0.5
    
    repeat {
        ts <- get.time.series()
        if (is.null(ts)) return(NULL)
        
        n <- length(ts)
        
        repeat {
            alpha <- default.alpha
            beta <- default.beta
            theta <- default.theta
            
            is.trended <- ask.for.yes.no(
                paste0("Has time series a trend?\n",
                       "y - yes\n",
                       "n - no\n"))
            is.seasonal <- ask.for.yes.no(
                paste0("Has time series a seasonal component?\n",
                       "y - yes\n",
                       "n - no\n"))
            default.params <- ask.for.yes.no(
                paste0("Use default parameters?\n",
                       "alpha - ", default.alpha, " (smooth parameter)\n",
                       ifelse(is.trended, 
                              paste0("beta - ", default.beta, " (trend parameter)\n"),
                              ""),
                       ifelse(is.seasonal,
                              paste0("theta - ", default.theta, " (seasonal parameter)\n"),
                              ""),
                       "y - yes\n",
                       "n - no\n"))
            if (!default.params) {
                alpha <- get.model.param(
                                prompt = paste0("Input alpha - smooth parameter\n",
                                                "(b - back to conditions, x - eXit)\n"))
                if (is.null(alpha)) return(NULL)
                if (!is.na(alpha)){
                    if (is.trended){
                        beta <- get.model.param(
                                prompt = paste0("Input beta - trend parameter\n",
                                                "(b - back to conditions, x - eXit)\n"))
                        if (is.null(beta)) return(NULL)
                        if (!is.na(beta)){
                            if (is.seasonal){
                                theta <- get.model.param(
                                            prompt = paste0("Input theta - seasonal parameter\n",
                                                            "(b - back to conditions, x - eXit)\n"))
                                if (is.null(theta)) return(NULL)
                            }
                        }
                    }
                }
            }
            
            if (all(!is.na(c(alpha, beta, theta)))){
                season.period <- if (is.seasonal) get.season.period(n)
                else 1
                if (is.null(season.period)) return(NULL)
                if (!is.na(season.period)){
                    forecast.lenght <- get.integer(
                                            prompt = paste0("Input size of forecast horizon\n",
                                                            "(b - back to conditions, x - eXit)\n"),
                                            prints = TRUE,
                                            min.val = 1)
                    if (is.null(forecast.lenght)) return(NULL)
                    if (!is.na(forecast.lenght)){
                        result <- list(
                            ts = ts,
                            is.trended = is.trended,
                            is.seasonal = is.seasonal,
                            alpha = alpha,
                            beta = ifelse(is.trended, beta, NA),
                            theta = ifelse(is.seasonal, theta, NA),
                            season.period = ifelse(is.seasonal, season.period, NA),
                            forecast.lenght = forecast.lenght)
                        return(result)
                    }
                }
            }
        }
    }
}
