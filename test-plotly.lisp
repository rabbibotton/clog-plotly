(in-package :clog-plotly)
(defclass test-panel (clog:clog-panel)
          ((button-4 :reader button-4) (button-3 :reader button-3)
           (clog-plotly-1 :reader clog-plotly-1)))
(defun create-test-panel
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"width: 400px; height: 300px; box-sizing: content-box; position: absolute; left: 57px; top: 23px;\" class=\"w3-border\" id=\"CLOGB3870083496\" data-clog-name=\"clog-plotly-1\"></div><button style=\"box-sizing: content-box; position: absolute; left: 482px; top: 92px; width: 120px;\" id=\"CLOGB3870083497\" data-clog-name=\"button-3\">get png</button><button id=\"CLOGB38700837444\" style=\"box-sizing: content-box; position: absolute; left: 482px; top: 131px; width: 120px;\" data-clog-name=\"button-4\">download png</button>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'test-panel)))
    (setf (slot-value panel 'button-4)
            (attach-as-child clog-obj "CLOGB38700837444" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'button-3)
            (attach-as-child clog-obj "CLOGB3870083497" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'clog-plotly-1)
            (attach-as-child clog-obj "CLOGB3870083496" :clog-type
             'clog-plotly:clog-plotly-element :new-id t))
    (let ((target (clog-plotly-1 panel)))
      (declare (ignorable target))
      (clog-plotly:attach-clog-plotly target)
      (clog-plotly:new-plot-plotly target "[
        {
            type: 'scatter',
            x: [1, 2, 3],
            y: [3, 1, 6],
            marker: {
                color: 'rgb(16, 32, 77)'
            }
        },
        {
            type: 'bar',
            x: [1, 2, 3],
            y: [3, 1, 6],
            name: 'bar chart example'
        }
    ]"
                                   "{
        title: 'simple example',
        xaxis: {
            title: 'time'
        },
        annotations: [
            {
                text: 'simple annotation',
                x: 0,
                xref: 'paper',
                y: 0,
                yref: 'paper'
            }
        ]
    }"))
    (clog-plotly:set-on-plotly-to-image (clog-plotly-1 panel)
                                        (lambda (target data)
                                          (declare (ignorable target data))
                                          (create-img panel :url-src data)))
    (clog:set-on-click (button-3 panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (to-image-plotly (clog-plotly-1 panel)
                          "{format: 'png', width: 800, height: 600}")))
    (clog:set-on-click (button-4 panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (download-image-plotly (clog-plotly-1 panel)
                          "{format: 'png', width: 800, height: 600, filename: 'newplot'}")))
    panel))
