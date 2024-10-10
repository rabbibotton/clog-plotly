
(asdf/parse-defsystem:defsystem #:clog-plotly
  :description
  "Plotly plugin for CLOG"
  :author
  "some@one.com"
  :license
  "BSD"
  :serial
  t
  :depends-on
  (#:clog)
  :entry-point
  "clog-plotly:start-test"
  :components
  ((:file "clog-plotly") (:file "test-plotly")))
(asdf/parse-defsystem:defsystem #:clog-plotly/tools
  :defsystem-depends-on
  (:clog)
  :depends-on
  (#:clog-plotly #:clog/tools)
  :components
  ((:file "clog-plotly-tools") (:clog-file "test-plotly")))
