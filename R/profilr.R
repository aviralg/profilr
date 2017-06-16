profilr.stack = list()

profilr.push <- function(time) {
  profilr.stack <<- c(time, profilr.stack)
}

profilr.pop <- function() {
  time <- profilr.stack[[1]]
  profilr.stack <<- profilr.stack[2 : length(profilr.stack)]
  time
}

profilr.matcher <- function(annotation) {
  annotation == as.symbol("profile")
}

profilr.transformer <- function(object, name, env, match) {
  body(object) <-
    substitute({
      profilr.push(Sys.time())
      result <- block
      print(Sys.time() - profilr.pop())
      result
    },
    list(block = body(object)))
  object
}

profilr.register <- function() {
  profilr.handler <- annotations.create.handler("profile", profilr.transformer)
  annotations.register("function", "header", profilr.matcher, profilr.handler)
}

profilr.deregister <- function() {
  # TODO
}

profilr.register()
