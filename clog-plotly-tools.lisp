(in-package :clog-plotly)

(progn
  (clog-tools:add-supported-controls
   (list '(:name           "group"
           :description    "Plotly Controls"
           :create         nil
           :create-type    nil
           :events         nil
           :properties     nil)
          `(;; unique name to control used to identify it the .clog xml
           :name           "clog-plotly"
           ;; how control appears in builder control list
           :description    "clog-plotly"
           ;; the common lisp type of the control
           :clog-type      clog-plotly:clog-plotly-element
           ;; the create-function used to create the function
           ;; at _design time_ at run time only clog:attach-as-child is used
           ;; any initialization at _run time_ is done with :on-setup below.
           :create         clog-plotly:create-clog-plotly-element-design
           ;; clog has the following create-types
           ;;   :base         - create
           ;;   :element      - create create-content
           ;;   :form         - create create-param create-value
           ;;   :text-area    - create create-value
           ;;   :custom-query - create (ask user for string)
           ;;   :custom       - create create-content
           :create-type    :base
           ;; setup the control at _design time_ and custom attributes
           :setup          ,(lambda (control content control-record)
                              (declare (ignore content control-record))
                              ;; default custom attribute values and events at design time

                              ;; tell the builder this is a composite control, ie made of multiple
                              ;; controls
                              (setf (attribute control "data-on-create")
"(clog-plotly:new-plot-plotly target
  \"[{x: [1, 2, 3, 4, 5],
           y: [1, 2, 4, 8, 16]}]\"
        \"{ margin: { t: 0 } }\")")
                              (setf (css-class-name control) "w3-border")
                              (setf (attribute control "data-clog-composite-control") "t"))
           ;; code to run at _run time_ after all controls attached to panel
           :on-setup       ,(lambda (control control-record)
                              (declare (ignore control control-record))
                              ;; initialization at run time and apply custom attributes
                              (format nil "(clog-plotly:attach-clog-plotly target)"))
           ;; code to run at _design time_ on load from .clog file or paste
           ;; :on-load        ,(lambda (control control-record)
           ;;                 (declare (ignore control-record))
           ;;                 ;; code to attach functionality if your create for design time
           ;;                 )
           ;; events handled
           :events         ((:name        "on-plotly-to-image"
                             :package     "clog-plotly"
                             :parameters  "target data")
                            (:name        "on-plotly-after-plot"
                             :package     "clog-plotly"
                             :parameters  "target")
                            (:name        "on-plotly-auto-size"
                             :package     "clog-plotly"
                             :parameters  "target")
                            (:name        "on-plotly-deselect"
                             :package     "clog-plotly"
                             :parameters  "target")
                            (:name        "on-plotly-redraw"
                             :package     "clog-plotly"
                             :parameters  "target")
                            (:name        "on-plotly-animated"
                             :package     "clog-plotly"
                             :parameters  "target")
                            ,@clog-tools::*events-element*)
           ;; properties handled
           :properties     (,@clog-tools::*props-location*
                            ,@clog-tools::*props-width-height*
                            ,@clog-tools::*props-text*
                            ,@clog-tools::*props-css*
                            ,@clog-tools::*props-colors*
                            ,@clog-tools::*props-display*
                            ,@clog-tools::*props-flex-item*
                            ,@clog-tools::*props-nav*
                            ,@clog-tools::*props-contents*))))
  (format t "~%clog-plotly installed in CLOG Builder"))
