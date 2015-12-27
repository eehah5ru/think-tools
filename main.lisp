(load #p"ideas.asd")

(ql:quickload :ideas)

(lucerne:start ideas:app :port 8000)
