



dump = function(params = NULL, verbose = TRUE){
  memory = mem_used()
  time = Sys.time()
  oldTime = if(!is.null(params)) params$time else time
  oldMemory = if(!is.null(params)) params$memory else memory
  if(verbose){
      cat(paste0("Mem used: ", memory / 1000000, " MB (delta = ", (memory - oldMemory) / 1000000, " MB)" ));
      cat(paste0("Current time: ", time))
      cat(paste0("Elapsed time: ", time - oldTime, " seconds"))
  }
  return(list("time" = time, "memory" = memory))
}
