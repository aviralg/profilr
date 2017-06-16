profilr.stack = list()

profilr.push <- function(time) {
  profilr.stack <<- c(time, profilr.stack)
}

profilr.pop <- function() {
  time <- profilr.stack[[1]]
  profilr.stack <<- profilr.stack[2 : length(profilr.stack)]
  time
}

matcher <- function(annotation) {
  annotation == as.symbol("profile")
}

action <- function(object, name, env, match) {
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
  handler <- annotations.create.handler(name = "profile",
                                        action = action,
                                        mode = "once",
                                        remove = TRUE)
  annotations.register("function", "header", matcher, handler)
}

profilr.deregister <- function() {
  # TODO
}

profilr.register()
