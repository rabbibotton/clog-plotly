(in-package :clog-plotly)
(defclass test-panel (clog:clog-panel) ((clog-plotly-1 :reader clog-plotly-1)))
(defun create-test-panel
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"width: 400px; height: 300px; box-sizing: content-box; position: absolute; left: 100px; top: 25px;\" class=\"w3-border\" id=\"CLOGB3869928654\" data-clog-name=\"clog-plotly-1\"></div>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'test-panel)))
    (setf (slot-value panel 'clog-plotly-1)
            (attach-as-child clog-obj "CLOGB3869928654" :clog-type
             'clog-plotly:clog-plotly-element :new-id t))
    (let ((target (clog-plotly-1 panel)))
      (declare (ignorable target))
      (clog-plotly:attach-clog-plotly target)
      (clog-plotly:json-plotly target "[{x: [1, 2, 3, 4, 5],
           y: [1, 2, 4, 8, 16]}]"
                               "{ margin: { t: 0 } }"))
    panel))
