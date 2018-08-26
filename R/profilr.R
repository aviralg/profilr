library(annotatr)

cache <- new.env(hash = TRUE, parent = emptyenv())

push <- function(time) {
  assign("stack",  c(time, cache$stack), cache)
}

pop <- function() {
  time <- cache$stack[[1]]
  assign("stack", cache$stack[2 : length(cache$stack)], cache)
  time
}

save <- function(name, time_diff) {
  assign(name , append(cache$times[[name]], list(time_diff)), cache$times)
}

namespace <- function() "profilr"

enable <- function() annotatr::enable_namespace(profilr_namespace())

disable <- function() annotatr::disable_namespace(profilr_namespace())

profile_matcher <- function(annotation) {
  print("here")
  annotation == as.symbol("profile")
}

profile_action <- function(name, fun, match) {
  body(fun) <-
    substitute({
      profilr:::push(Sys.time())
      print("here")
      result <- block
      print(result)
      time_diff <- Sys.time() - profilr:::pop()
      print(time_diff)
      profilr:::save(name, time_diff)
      print("here again")
      result
    },
    list(block = body(fun), name = name))
  fun
}

profile_handler <- create_handler("profile",
                                           profile_matcher, profile_action,
                                           "individual", TRUE)
environment_begin_hook <- function(env) {
  envname <<- environmentName(env)
  assign("stack", list(), cache)
  assign("times", new.env(hash = TRUE, parent = emptyenv()), cache)
}

environment_end_hook <- function(env) {
  reg.finalizer(.GlobalEnv,function(e){message("Bye Bye")},onexit=TRUE)
}

.onAttach <- function(libname, pkgname) {

## print(event_hook(namespace(), "environment_begin"))
print("executing this")
  register_event_hook(namespace(), "environment_begin", environment_begin_hook)
  print("and this")
register_annotation_handler(namespace(), "function_header", profile_handler)
print("executed this") 

}
