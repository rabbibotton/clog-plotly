(defpackage #:clog-plotly
  (:use #:cl #:clog)
  (:export clog-plotly-element
           create-clog-plotly-element
           create-clog-plotly-element-design
           init-clog-plotly
           attach-clog-plotly
           json-plotly
           start-test))

(in-package :clog-plotly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-plotly-element (clog-element)()
  (:documentation "clog-plotly Element Object."))

(defgeneric create-clog-plotly-element (clog-obj &key hidden class html-id auto-place)
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

(defgeneric create-clog-plotly-element-design (clog-obj &key hidden class html-id auto-place)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-plotly-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric json-plotly (clog-plotly-element json-data json-plotly)
  (:documentation "Plot with plotly json-data and json-layout"))

(defmethod json-plotly ((obj clog-plotly-element) json-data json-layout)
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A)"
                          (script-id obj) json-data json-layout)))

(defgeneric json-react (clog-plotly-element json-data json-plotly)
  (:documentation "Plot with plotly json-data and json-layout"))

(defmethod json-react ((obj clog-plotly-element) json-data json-layout)
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A)"
                          (script-id obj) json-data json-layout)))

(defgeneric restyle-plotly (clog-plotly-element json-update trace-indices)
  (:documentation "Restyle plotly with json-update and trace-indices."))

(defmethod restyle-plotly ((obj clog-plotly-element) json-update trace-indices)
  (js-execute obj (format nil "Plotly.restyle(~A,~A,~A)"
                               (script-id obj) json-update trace-indices)))

(defgeneric relayout-plotly (clog-plotly-element json-update)
  (:documentation "Relayout plotly with json-update and trace-indices."))

(defmethod relayout-plotly ((obj clog-plotly-element) json-update)
  (js-execute obj (format nil "Plotly.relayout(~A,~A)"
                               (script-id obj) json-update)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - js binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric init-clog-plotly (clog-plotly-element)
  (:documentation "Initialize libraries"))

(defmethod init-clog-plotly ((obj clog-plotly-element))
  (check-type obj clog:clog-obj)
  (load-script (html-document (connection-data-item obj "clog-body"))
    "https://cdn.plot.ly/plotly-2.14.0.min.js"))

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
    (clog-plotly::json-plotly test
                              "[{x: [1, 2, 3, 4, 5],
                                 y: [1, 2, 4, 8, 16]}]"
                              "{ margin: { t: 0 } }")))

(defun start-test ()
  (initialize 'on-test-clog-plotly
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :clog-plotly)))
  (open-browser))
