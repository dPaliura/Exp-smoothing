source("input.R", echo = FALSE)
source("applier.R", echo = FALSE)
source("interpreter.R", echo = FALSE)


start <- function(){
    repeat {
        inp <- get.input()
        if (is.null(inp)) return()
        
        res <- apply(inp)
        interpret(res)
        
        if (!ask.for.yes.no(
                    prompt = paste0("Restart?\n",
                                    "y - yes\n",
                                    "x - eXit\n"),
                    no="x")) break()
    }
    
}
