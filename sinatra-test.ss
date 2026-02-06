(import :std/test
        ./sinatra/route-test
        ./sinatra/context-test
        ./sinatra/helpers-test
        ./sinatra/cookies-test
        ./sinatra/session-test
        ./sinatra/template-test
        ./sinatra/middleware-test
        ./sinatra/handler-test)

(export sinatra-test)

(def sinatra-test
  (test-suite "gerbil-sinatra"
    route-test
    context-test
    helpers-test
    cookies-test
    session-test
    template-test
    middleware-test
    handler-test))
