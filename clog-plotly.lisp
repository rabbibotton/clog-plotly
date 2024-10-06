(defpackage #:clog-plotly
  (:use #:cl #:clog)
  (:export clog-plotly-element
           create-clog-plotly-element
           create-clog-plotly-element-design
           init-clog-plotly
           attach-clog-plotly
           new-plot-plotly react-plotly
           restyle-plotly relayout-plotly
           update-plotly purge-plotly
           add-traces-plotly delete-traces-plotly
           move-traces-plotly extend-traces-plotly
           prepend-traces-plotly animate-plotly
           download-image-plotly to-image-plotly
           set-on-plotly-to-image set-on-plotly-afer-plot
           set-on-plotly-auto-size set-on-plotly-deselect
           set-on-plotly-redraw set-on-plotly-animated
           start-test))

(in-package :clog-plotly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-plotly-element (clog-element)()
  (:documentation "clog-plotly Element Object."))

(defgeneric create-clog-plotly-element (clog-obj &key hidden class
                                                      html-id auto-place)
  (:documentation "Create a new clog-plotly-element as child of CLOG-OBJ."))

(defmethod create-clog-plotly-element ((obj clog:clog-obj)
                                         &key
                                           (hidden nil)
                                           (class nil)
                                           (html-id nil)
                                           (auto-place t))
  "Create control - used at design time and in code"
  (let ((new-obj (create-div obj
                             :class class
                             :hidden hidden
                             :html-id html-id
                             :auto-place auto-place)))
    (set-geometry new-obj :width 400 :height 300)
    (change-class new-obj 'clog-plotly-element)
    (attach-clog-plotly new-obj)
    new-obj))

(defgeneric create-clog-plotly-element-design (clog-obj &key hidden class
                                                             html-id auto-place)
  (:documentation "Create a new clog-plotly-element as child of CLOG-OBJ."))

(defmethod create-clog-plotly-element-design ((obj clog:clog-obj)
                                                &key
                                                  (hidden nil)
                                                  (class nil)
                                                  (html-id nil)
                                                  (auto-place t))
  "Create control - used at design time and in code"
  (let ((new-obj (create-div obj
                             :class class
                             :hidden hidden
                             :html-id html-id
                             :auto-place auto-place)))
    (set-geometry new-obj :width 400 :height 300)
    (change-class new-obj 'clog-plotly-element)
    (attach-clog-plotly new-obj)
    (create-div new-obj :content "No preview")
    new-obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-plotly-to-image (clog-plotly-element handler)
  (:documentation "Set handler for receiving the image result of a call to
to-image-plotly"))

(defmethod set-on-plotly-to-image ((obj clog-plotly-element) handler)
  (set-on-event-with-data obj "clog-plotly-to-image" handler))

(defgeneric set-on-plotly-after-plot (clog-plotly-element handler)
  (:documentation "Set handler for after-plot plotly event"))

(defmethod set-on-plotly-after-plot ((obj clog-plotly-element) handler)
  (set-on-event obj "plotly_afterplot" handler))

(defgeneric set-on-plotly-auto-size (clog-plotly-element handler)
  (:documentation "Set handler for auto-size plotly event"))

(defmethod set-on-plotly-auto-size ((obj clog-plotly-element) handler)
  (set-on-event obj "plotly_autosize" handler))

(defgeneric set-on-plotly-deselect (clog-plotly-element handler)
  (:documentation "Set handler for deselect plotly event"))

(defmethod set-on-plotly-deselect ((obj clog-plotly-element) handler)
  (set-on-event obj "plotly_deselect" handler))

(defgeneric set-on-plotly-redraw (clog-plotly-element handler)
  (:documentation "Set handler for redraw plotly event"))

(defmethod set-on-plotly-redraw ((obj clog-plotly-element) handler)
  (set-on-event obj "plotly_redraw" handler))

(defgeneric set-on-plotly-animated (clog-plotly-element handler)
  (:documentation "Set handler for animated plotly event"))

(defmethod set-on-plotly-animated ((obj clog-plotly-element) handler)
  (set-on-event obj "plotly_animated" handler))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric new-plot-plotly (clog-plotly-element json-data json-layout)
  (:documentation "Plot with plotly json-data and json-layout"))

(defmethod new-plot-plotly ((obj clog-plotly-element) json-data json-layout)
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A)"
                          (script-id obj) json-data json-layout)))

(defgeneric react-plotly (clog-plotly-element json-data json-layout)
  (:documentation "Plot with plotly json-data and json-layout"))

(defmethod react-ploly ((obj clog-plotly-element) json-data json-layout)
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A)"
                          (script-id obj) json-data json-layout)))

(defgeneric restyle-plotly (clog-plotly-element json-update trace-indices)
  (:documentation "Restyle plotly with json-update and trace-indices."))

(defmethod restyle-plotly ((obj clog-plotly-element) json-update trace-indices)
  (js-execute obj (format nil "Plotly.restyle(~A,~A,~A)"
                               (script-id obj) json-update trace-indices)))

(defgeneric relayout-plotly (clog-plotly-element json-update)
  (:documentation "Relayout plotly with json-update"))

(defmethod relayout-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.relayout(~A,~A)"
                               (script-id obj) json-update)))

(defgeneric update-plotly (clog-plotly-element json-data
                                               json-plotly
                                               trace-indices)
  (:documentation "An efficient means of updating both the data array and layout
object in an existing plot."))

(defmethod update-plotly ((obj clog-plotly-element) json-data
                                                    json-layout
                                                    trace-indices)
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A, ~A)"
                          (script-id obj) json-data json-layout trace-indices)))

(defgeneric add-traces-plotly (clog-plotly-element json-update)
  (:documentation "Add traces plotly with json-update"))

(defmethod add-traces-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.addTraces(~A,~A)"
                               (script-id obj) json-update)))

(defgeneric delete-traces-plotly (clog-plotly-element json-update)
  (:documentation "Delete traces plotly with json-update"))

(defmethod delete-traces-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.deleteTraces(~A,~A)"
                               (script-id obj) json-update)))

(defgeneric move-traces-plotly (clog-plotly-element json-update)
  (:documentation "Move traces plotly with json-update"))

(defmethod move-traces-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.moveTraces(~A,~A)"
                               (script-id obj) json-update)))

(defgeneric extend-traces-plotly (clog-plotly-element json-update)
  (:documentation "Extend traces plotly with json-update"))

(defmethod extend-traces-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.extendTraces(~A,~A)"
                               (script-id obj) json-update)))

(defgeneric prepend-traces-plotly (clog-plotly-element json-update)
  (:documentation "Prepend traces plotly with json-update"))

(defmethod prepend-traces-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.prependTraces(~A,~A)"
                               (script-id obj) json-update)))

(defgeneric animate-plotly (clog-plotly-element json-frame json-attributes)
  (:documentation "Animate frames with attriutes"))

(defmethod animate-ploly ((obj clog-plotly-element) json-frame json-attributes)
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A)"
                          (script-id obj) json-frame json-attributes)))

(defgeneric to-image-plotly (clog-plotly-element json-properties)
  (:documentation "Create plot using json-properties and fire the event
on-plotlyto-image with string in url data form."))

(defmethod to-image-plotly ((obj clog-plotly-element) json-properties)
  (js-execute obj (format nil "Plotly.toImage(~A,~A).then(function(data) {~
                                 ~A.trigger('clog-plotly-to-image', data)})"
                               (script-id obj) json-properties
                               (jquery obj))))

(defgeneric download-image-plotly (clog-plotly-element json-properties)
  (:documentation "Download as image"))

(defmethod download-image-plotly ((obj clog-plotly-element) json-properties)
  (js-execute obj (format nil "Plotly.downloadImage(~A,~A)"
                               (script-id obj) json-properties)))

(defgeneric purge-plotly (clog-plotly-element)

  (:documentation "Remove plot"))

(defmethod purge-plotly ((obj clog-plotly-element))
  (js-execute obj (format nil "Plotly.relayout(~A)"
                               (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - js binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric init-clog-plotly (clog-plotly-element)
  (:documentation "Initialize libraries"))

(defmethod init-clog-plotly ((obj clog-plotly-element))
  (check-type obj clog:clog-obj)
  (load-script (html-document (connection-data-item obj "clog-body"))
    "https://cdn.plot.ly/plotly-2.35.2.min.js"))

(defgeneric attach-clog-plotly (clog-plotly-element)
  (:documentation "Initialize plugin"))

(defmethod attach-clog-plotly ((obj clog-plotly-element))
  (init-clog-plotly obj))

(defun on-test-clog-plotly (body)
  (clog:debug-mode body)
  ;; Use the panel-box-layout to center horizontally
  ;; and vertically our div on the screen.
  (let* ((layout (create-panel-box-layout body))
         (test   (create-clog-plotly-element (center-panel layout))))
    (center-children (center-panel layout))
    (clog-plotly::new-plot-plotly test
                              "[{x: [1, 2, 3, 4, 5],
                                 y: [1, 2, 4, 8, 16]}]"
                              "{ margin: { t: 0 } }")))

(defun start-test ()
  (initialize 'on-test-clog-plotly
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :clog-plotly)))
  (open-browser))
