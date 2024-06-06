(in-package :paren-jquery)

(defpsmacro %h (&rest args)
  `(create ,@(loop :for (k v) :on args :by #'cddr
                   :collect (ps:symbol-to-js-string k)
                   :collect v)))

(defpsmacro %event-handler (name subject handler data)
  `($ ,subject (,name ,@(when data (list data))
                      ,handler)))

(defpsmacro $ (subject &rest chains)
  `(chain (j-query ,@(uiop:ensure-list subject)) ,@chains))

(defpsmacro $-> (&rest chains)
  `(chain j-query ,@chains))

(defpsmacro $add (subject selector)
  `($ ,subject (add ,selector)))

(defpsmacro $add-back (subject &optional selector)
  `($ ,subject (add-back ,@(when selector (list selector)))))

(defpsmacro $add-class (subject class)
  `($ ,subject (add-class ,class)))

(defpsmacro $after (subject content)
  `($ ,subject (after ,content)))

(defpsmacro $animate (subject properties &key duration easing complete)
  `($ ,subject (animate ,properties
                        ,@(when duration (list duration))
                        ,@(when easing (list easing))
                        ,@(when complete (list complete)))))

(defpsmacro $attr (subject attr-name)
  `($ ,subject (attr ,attr-name)))

(defpsmacro $append-to (subject target)
  `($ ,subject (append-to ,target)))

(defpsmacro $append (subject content)
  `($ ,subject (append ,content)))

(defpsmacro $before (subject content)
  `($ ,subject (before ,content)))

(defmacro $bind (subject event-type handler &optional event-data)
  `($ ,subject (bind ,event-type ,@(when event-data (list event-data))
                     ,handler)))

(defpsmacro $blur (subject handler &optional event-data)
  `(%event-handler blur ,subject ,handler ,event-data))

(defpsmacro $change (subject handler &optional event-data)
  `(%event-handler change ,subject ,handler ,event-data))

(defpsmacro $children (subject &optional selector)
  `($ ,subject (children ,@(when selector (list selector)))))


(defpsmacro $click (subject handler &optional event-data)
  `(%event-handler click ,subject ,handler ,event-data))

(defpsmacro $closest (subject selector &optional context)
  `($ ,subject (closest ,selector ,@(when context (list context)))))

(defpsmacro $contents (subject)
  `($ ,subject (contents)))

(defpsmacro $contextmenu (subject handler)
  `($ ,subject (contextmenu ,handler)))

(defpsmacro $css (subject prop1 &rest props)
  `($ ,subject (css ,(when props
                       (append (list prop1) props)
                       prop1))))

(defpsmacro $data (subject key &optional (val nil valp))
  `($ ,subject (data ,key ,@(when valp (list val)))))

(defpsmacro $dblclick (subject handler &optional event-data)
  `(%event-handler dblclick ,subject ,handler ,event-data))

(defpsmacro $detach (subject &optional selector)
  `($ ,subject (detach ,@(when selector (list selector)))))

(defpsmacro $each (subject handler)
  `($ ,subject (each ,handler)))

(defpsmacro $empty (subject)
  `($ ,subject (empty)))

(defpsmacro $eq (subject index)
  `($ ,subject (eq ,index)))

(defpsmacro $error (subject handler)
  `($ ,subject (error ,handler)))

(defpsmacro $even (subject)
  `($ ,subject (even)))

(defpsmacro $fade-in (subject &key duration complete)
  `($ ,subject (fade-in ,@(when duration (list duration))
                        ,@(when complete (list complete)))))

(defpsmacro $fade-out (subject &key duration complete)
  `($ ,subject (fade-out ,@(when duration (list duration))
                         ,@(when complete (list complete)))))

(defpsmacro $fade-toggle (subject &key duration easing complete)
  `($ ,subject (fade-out ,@(when duration (list duration))
                         ,@(when easing (list easing))
                         ,@(when complete (list complete)))))

(defpsmacro $filter (subject filter)
  `($ ,subject (filter ,filter)))

(defpsmacro $find (subject selector)
  `($ ,subject (find ,selector)))

(defpsmacro $finish (subject &optional queue)
  `($ ,subject (finish ,@(when queue (list queue)))))

(defpsmacro $first (subject)
  `($ ,subject (first)))

(defpsmacro $focus (subject handler &optional event-data)
  `(%event-handler focus ,subject ,handler ,event-data))

(defpsmacro $focus-in (subject handler &optional event-data)
  `(%event-handler focus-in ,subject ,handler ,event-data))

(defpsmacro $focus-out (subject handler &optional event-data)
  `(%event-handler focus-out ,subject ,handler ,event-data))

(defpsmacro $get (subject index)
  `($ ,subject (get ,index)))

(defpsmacro $has (subject selector)
  `($ ,subject (has ,selector)))

(defpsmacro $has-class (subject class)
  `($ ,subject (has-class ,class)))

(defpsmacro $height (subject &optional (val nil valp))
  `($ ,subject (height ,@(when valp (list val)))))

(defpsmacro $hover (subject handler-in &optional handler-out)
  `($ ,subject (hover ,handler-in ,@(when handler-out (list handler-out)))))

(defpsmacro $html (subject &optional (val nil valp))
  `($ ,subject (html ,@(when valp (list val)))))

(defpsmacro $index (subject selector)
  `($ ,subject (index ,selector)))

(defpsmacro $inner-height (subject &optional (val nil valp))
  `($ ,subject (inner-height ,@(when valp (list val)))))

(defpsmacro $inner-width (subject &optional (val nil valp))
  `($ ,subject (inner-width ,@(when valp (list val)))))

(defpsmacro $inseft-after (subject target)
  `($ ,subject (inseft-after ,target)))

(defpsmacro $inseft-before (subject target)
  `($ ,subject (inseft-before ,target)))

(defpsmacro $is (subject selector)
  `($ ,subject (is ,selector)))

(defpsmacro $keydown (subject handler &optional event-data)
  `(%event-handler keydown ,subject ,handler ,event-data))

(defpsmacro $keypress (subject handler &optional event-data)
  `(%event-handler keypress ,subject ,handler ,event-data))

(defpsmacro $keyup (subject handler &optional event-data)
  `(%event-handler keyup ,subject ,handler ,event-data))

(defpsmacro $last (subject)
  `($ ,subject (last)))

(defpsmacro $load (subject url &key data complete)
  `($ ,subject (load ,url
                     ,@(when data (list data))
                     ,@(when complete (list complete)))))

(defpsmacro $mousedown (subject handler &optional event-data)
  `(%event-handler mousedown ,subject ,handler ,event-data))

(defpsmacro $mouseenter (subject handler &optional event-data)
  `(%event-handler mouseenter ,subject ,handler ,event-data))

(defpsmacro $mouseleave (subject handler &optional event-data)
  `(%event-handler mouseleave ,subject ,handler ,event-data))

(defpsmacro $mousemove (subject handler &optional event-data)
  `(%event-handler mousemove ,subject ,handler ,event-data))

(defpsmacro $mouseout (subject handler &optional event-data)
  `(%event-handler mouseout ,subject ,handler ,event-data))

(defpsmacro $mouseover (subject handler &optional event-data)
  `(%event-handler mouseover ,subject ,handler ,event-data))

(defpsmacro $mouseup (subject handler &optional event-data)
  `(%event-handler mouseup ,subject ,handler ,event-data))


(defpsmacro $next (subject &optional selector)
  `($ ,subject (next ,@(when selector (list selector)))))

(defpsmacro $next-all (subject &optional selector)
  `($ ,subject (next-all ,@(when selector (list selector)))))

(defpsmacro $next-until (subject &optional selector filter)
  `($ ,subject (next-until ,@(when selector (list selector))
                           ,@(when filter (list filter)))))

(defpsmacro $not (subject selector)
  `($ ,subject (not ,selector)))

(defpsmacro $odd (subject)
  `($ ,subject (odd)))

(defpsmacro $on (subject event handler &optional event-data)
  `($ ,subject (on ,event ,@(when event-data (list event-data)) ,handler)))

(defpsmacro $one (subject event handler &optional event-data)
  `($ ,subject (one ,event ,@(when event-data (list event-data))
                    ,handler)))

(defpsmacro $off (subject event)
  `($ ,subject (off ,event)))


(defpsmacro $offset (subject &optional coords)
  `($ ,subject (offset ,@(when coords (list coords)))))

(defpsmacro $offset-parent (subject)
  `($ ,subject (offset-parent)))

(defpsmacro $parent (subject &optional selector)
  `($ ,subject (parent ,@(when selector (list selector)))))

(defpsmacro $parents (subject &optional selector)
  `($ ,subject (parents ,@(when selector (list selector)))))

(defpsmacro $parents-until (subject &optional selector filter)
  `($ ,subject (parents-until ,@(when selector (list selector))
                              ,@(when filter (list filter)))))

(defpsmacro $position (subject)
  `($ ,subject (position)))

(defpsmacro $prepend (subject content)
  `($ ,subject (prepend ,content)))

(defpsmacro $prepend-to (subject target)
  `($ ,subject (prepend-to ,target)))

(defpsmacro $prev (subject &optional selector)
  `($ ,subject (prev ,@(when selector (list selector)))))

(defpsmacro $prev-all (subject &optional selector)
  `($ ,subject (prev-all ,@(when selector (list selector)))))

(defpsmacro $prev-until (subject &optional selector filter)
  `($ ,subject (prev-until ,@(when selector (list selector))
                           ,@(when filter (list filter)))))

(defpsmacro $promise (subject &optional type target)
  `($ ,subject (promise ,@(when type (list type))
                        ,@(when target (list target)))))

(defpsmacro $ready (subject handler)
  `($ ,subject (ready ,handler)))

(defpsmacro $remove (subject &optional selector)
  `($ ,subject (remove ,@(when selector (list selector)))))

(defpsmacro $remove-attr (subject attr)
  `($ ,subject (remove-attr ,attr)))

(defpsmacro $remove-class (subject class)
  `($ ,subject (remove-class ,class)))

(defpsmacro $remove-data (subject &optional name)
  `($ ,subject (remove-data ,@(when name (list name)))))

(defpsmacro $remove-prop (subject prop)
  `($ ,subject (remove-prop ,prop)))

(defpsmacro $replace-all (subject target)
  `($ ,subject (replace-all ,target)))

(defpsmacro $replace-with (subject content)
  `($ ,subject (replace-with ,content)))

(defpsmacro $scroll (subject handler &optional event-data)
  `(%event-handler scroll ,subject ,handler ,event-data))

(defpsmacro $scroll-left (subject &optional val)
  `($ ,subject (scroll-left ,@(when val (list val)))))

(defpsmacro $scroll-top (subject &optional val)
  `($ ,subject (scroll-top ,@(when val (list val)))))

(defpsmacro $select (subject handler &optional event-data)
  `(%event-handler select ,subject ,handler ,event-data))

(defpsmacro $serialize (subject)
  `($ ,subject (serialize)))

(defpsmacro $serialize-array (subject)
  `($ ,subject (serialize-array)))

(defpsmacro $show (subject &key duration complete)
  `($ ,subject (show ,@(when duration (list duration))
                     ,@(when complete (list complete)))))

(defpsmacro $siblings (subject &optional selector)
  `($ ,subject (siblings ,@(when selector (list selector)))))

(defpsmacro $size (subject)
  `($ ,subject (size)))

(defpsmacro $slice (subject start &optional end)
  `($ ,subject (slice ,start ,@(when end (list end)))))

(defpsmacro $slide-down (subject &key duration complete)
  `($ ,subject (slide-down ,@(when duration (list duration))
                           ,@(when complete (list complete)))))

(defpsmacro $slide-toggle (subject &key duration complete)
  `($ ,subject (slide-toggle ,@(when duration (list duration))
                             ,@(when complete (list complete)))))

(defpsmacro $slide-up (subject &key duration complete)
  `($ ,subject (slide-up ,@(when duration (list duration))
                         ,@(when complete (list complete)))))

(defpsmacro $submit (subject &optional handler event-data)
  `($ ,subject (submit ,@(when event-data (list event-data))
                       ,@(when handler (list handler)))))

(defpsmacro $text (subject &optional (val nil valp))
  `($ ,subject (text ,@(when valp (list val)))))

(defpsmacro $to-array (subject)
  `($ ,subject (to-array)))

(defpsmacro $toggle-class (subject class)
  `($ ,subject (toggle-class ,class)))


(defpsmacro $trigger (subject event-type &optional params)
  `($ ,subject (trigger ,event-type ,@(when params (list params)))))

(defpsmacro $trigger-handler (subject event-type &optional params)
  `($ ,subject (trigger-handler ,event-type ,@(when params (list params)))))

(defpsmacro $unbind (subject event &optional handler)
  `($ ,subject (unbind ,event ,@(when handler (list handler)))))

(defpsmacro $uniq-sort (subject)
  `($ ,subject (uniq-sort)))

(defpsmacro $unwrap (subject &optional selector)
  `($ ,subject (unwrap ,@(when selector (list selector)))))

(defpsmacro $val (subject &optional (val nil valp))
  `($ ,subject (val ,@(when valp (list val)))))

(defpsmacro $width (subject &optional (val nil valp))
  `($ ,subject (width ,@(when valp (list val)))))

(defpsmacro $wrap (subject wrap)
  `($ ,subject (wrap ,wrap)))

(defpsmacro $wrap-all (subject wrap)
  `($ ,subject (wrap-all ,wrap)))

(defpsmacro $wrap-inner (subject wrap)
  `($ ,subject (wrap-inner ,wrap)))



;; FUNCTIONS
(defpsmacro $-ajax (url &rest settings)
  `($-> (ajax ,url (%h ,@settings))))

(defpsmacro $-contains (container contained)
  `($-> (contains ,container ,contained)))

(defpsmacro $-extend (target object1 &rest objects)
  `($-> (extend ,target ,object1 ,@objects)))

(defpsmacro $-post (uri data handler)
  `($-> (post ,uri ,data ,handler)))

(defpsmacro $-get (uri data handler)
  `($-> (get ,uri ,data ,handler)))

(defpsmacro $-get-json (uri data handler)
  `($-> (get-j-s-o-n ,uri ,data ,handler)))

(defpsmacro $-get-script (url &optional handler)
  `($-> (get-script ,url ,@(when handler (list handler)))))

(defpsmacro $-grep (lst handler)
  `($-> (grep ,lst ,handler)))

(defpsmacro $-map (lst handler)
  `($-> (map ,lst ,handler)))

(defpsmacro $-now ()
  `($-> (now)))

(defpsmacro $-parse-html (data &key context keep-scripts)
  `($-> (parse-h-t-m-l ,data
                       ,@(when context (list context))
                       ,@(when keep-scripts (list keep-scripts)))))

(defpsmacro $-parse-xml (data)
  `($-> (parse-x-m-l ,data)))

;; ADDITIONAL

(defpsmacro $existsp (subject)
  `(> (@ ($ ,subject) length) 0))

(defpsmacro $doc-ready (&body body)
  `($ document (ready (lambda () ,@body))))


(defpsmacro $append-html (target &rest html)
  `($ ,target (append (who-ps-html ,@html))))

(defpsmacro $prepend-html (target &rest html)
  `($ ,target (prepend (who-ps-html ,@html))))

(defpsmacro $replace-html (target &rest html)
  `($ ,target (replace-with (who-ps-html ,@html))))

(defpsmacro do-$each ((index element selector) &body body)
  `(-> $ (each ,selector (lambda (,index ,element)
                           ,@body))))

(defpsmacro $deferred ()
  `(-> $ (-deferred)))

(defpsmacro $deferred-resolve (defer &rest args)
  `(-> ,defer (resolve ,@args)))

(defpsmacro $deferred-reject (defer &rest args)
  `(-> ,defer (reject ,@args)))

(defpsmacro with-$deferred ((defer) &body body)
  `(let ((,defer ($-> (-deferred))))
     ,@body
     ,defer))
