;;; grid-table-charts.el --- Chart and data visualization plugin for grid-table -*- lexical-binding: t -*-、
;; This is a developing, experiment plugin for grid-table.el.

(require 'grid-table)
(require 'grid-table-plugins)

;;;----------------------------------------------------------------------
;;; Data Bar Renderer
;;;----------------------------------------------------------------------

(defun grid-table-charts-data-bar-renderer (value &optional cell-props)
  "Render numeric values as data bars with color coding.
VALUE should be a numeric string.
CELL-PROPS can contain:
- :max-value - Maximum value for scaling (default: 100)
- :color - Color of the bar (default: blue)
- :show-percentage - Whether to show percentage (default: t)"
  (if (and value (stringp value))
      (let* ((num (string-to-number value))
             (max-value (or (plist-get cell-props :max-value) 100))
             (show-percentage (if (plist-get cell-props :show-percentage) 
                                 (plist-get cell-props :show-percentage) 
                               t))
             (bar-width 15)
             (filled-width (max 0 (min bar-width 
                                     (if (> max-value 0) 
                                         (round (* num bar-width (/ 1.0 max-value))) 
                                       0))))
             (percentage (if (> max-value 0) 
                            (round (* num 100 (/ 1.0 max-value))) 
                          0))
             (bar (make-string bar-width ?\s)))
        (when (and (numberp num) (>= num 0))
          ;; Fill the bar
          (dotimes (i filled-width)
            (aset bar i ?█))
          ;; Apply color based on value
          (let ((colored-bar (propertize bar 'face 
                                       (cond
                                        ((>= percentage 80) '(:foreground "green"))
                                        ((>= percentage 60) '(:foreground "yellow"))
                                        ((>= percentage 40) '(:foreground "orange"))
                                        (t '(:foreground "red"))))))
            (if show-percentage
                (format "%s %d%%" colored-bar percentage)
              (format "%s" colored-bar))))
    (format "%s" (or value "")))))

;;;----------------------------------------------------------------------
;;; Text-based Sparkline Renderer
;;;----------------------------------------------------------------------

(defun grid-table-charts-text-sparkline-renderer (value &optional cell-props)
  "Render simple sparkline charts using Unicode block characters.
VALUE should be a comma-separated string of numbers.
CELL-PROPS can contain:
- :width - Width of the sparkline in characters (default: 10)
- :height - Height in characters (default: 1)"
  (if (and value (stringp value))
      (let* ((data-points (split-string value ","))
             (numbers (mapcar #'string-to-number data-points))
             (width (or (plist-get cell-props :width) 10))
             (height (or (plist-get cell-props :height) 1))
             (max-val (if numbers (apply #'max numbers) 1))
             (min-val (if numbers (apply #'min numbers) 0))
             (range (if (= max-val min-val) 1 (- max-val min-val))))
        (if (and numbers (> (length numbers) 1))
            (let ((sparkline ""))
              (dotimes (i width)
                (let* ((index (floor (* i (/ (float (length numbers)) width))))
                       (index (min index (1- (length numbers))))
                       (val (nth index numbers))
                       (normalized (/ (- val min-val) (float range)))
                       ;; Use different Unicode characters for different heights
                       (char (cond
                              ((> normalized 0.8) ?█)
                              ((> normalized 0.6) ?▆)
                              ((> normalized 0.4) ?▄)
                              ((> normalized 0.2) ?▂)
                              (t ?\s))))
                  (setq sparkline (concat sparkline (char-to-string char)))))
              (format "%s (min:%.1f max:%.1f)" sparkline min-val max-val))
          (format "%s" value)))
    (format "%s" (or value ""))))

;;;----------------------------------------------------------------------
;;; SVG-based Sparkline Renderer
;;;----------------------------------------------------------------------

(defun grid-table-charts-svg-to-image (svg-object &optional target-px-w target-px-h)
  "Convert SVG object to regular image for slicing.
SVG-OBJECT is the SVG object created by svg-create.
TARGET-PX-W is the target width in pixels.
TARGET-PX-H is the target height in pixels.
Returns an image suitable for insert-sliced-image."
  (svg-image svg-object :ascent 'center :width target-px-w :height target-px-h))

(defun grid-table-charts-svg-sparkline-renderer (value &optional cell-props)
  "Render SVG-based sparkline charts.
VALUE should be a comma-separated string of numbers.
CELL-PROPS can contain:
- :width - Width of the SVG in pixels (default: 150)
- :height - Height of the SVG in pixels (default: 40)
- :color - Line color (default: blue)
- :fill - Whether to fill the area under the line (default: nil)"
  (if (and value (stringp value))
      (let* ((data-points (split-string value ","))
             (numbers (mapcar #'string-to-number data-points))
             (width (or (plist-get cell-props :width) 150))
             (height (or (plist-get cell-props :height) 40))
             (color (or (plist-get cell-props :color) "blue"))
             (fill (plist-get cell-props :fill))
             (max-val (if numbers (apply #'max numbers) 1))
             (min-val (if numbers (apply #'min numbers) 0))
             (range (if (= max-val min-val) 1 (- max-val min-val))))
        (if (and numbers (> (length numbers) 1))
            ;; Check if Emacs has native SVG support
            (if (fboundp 'svg-create)
                ;; Use native SVG support with image slicing
                (let* ((svg (svg-create width height))
                       (n (length numbers))
                       (points '())
                       ;; 为绘图保留更大空间，缩小文本区域
                       (graph-height (- height 10)) ; 给底部留出10像素高度用于文本
                       (graph-y-offset 2)) ; 给顶部留出2像素的空间
                  
                  ;; 设置SVG背景
                  (svg-rectangle svg 0 0 width height :fill-color "#f8f8f8" :fill-opacity 0.1)
                  
                  ;; Generate points for the line - 使用调整后的高度
                  (dotimes (i n)
                    (let* ((x (* (/ (float i) (1- n)) width))
                           (y (+ graph-y-offset (- graph-height (* (/ (- (nth i numbers) min-val) (float range)) graph-height)))))
                      (push (cons (round x) (round y)) points)))
                  (setq points (nreverse points))
                  
                  ;; 添加网格线
                  (let ((grid-step (/ graph-height 4)))
                    (dotimes (i 4)
                      (let ((y-pos (+ graph-y-offset (* i grid-step))))
                        (svg-line svg 0 y-pos width y-pos :stroke-color "#dddddd" :stroke-width 0.5))))
                  
                  ;; Add filled area if requested
                  (when fill
                    (let ((fill-points (copy-sequence points)))
                      ;; Add bottom right and bottom left corners to close the polygon
                      (push (cons width (+ graph-y-offset graph-height)) fill-points)
                      (push (cons 0 (+ graph-y-offset graph-height)) fill-points)
                      (svg-polygon svg fill-points :fill-color color :fill-opacity 0.2)))
                  
                  ;; Add the line with improved styling
                  (svg-polyline svg points :stroke-color color :stroke-width 1.5)
                  
                  ;; 添加数据点标记
                  (dolist (point points)
                    (let ((x (car point))
                          (y (cdr point)))
                      (svg-circle svg x y 1.0 :fill-color color)))
                  
                  ;; 只添加最小值和最大值，格式更紧凑
                  (svg-text svg (format "%.1f" min-val) 
                            :x 1 :y (- height 1) 
                            :font-size 8 :fill "#555555")
                  (svg-text svg (format "%.1f" max-val) 
                            :x (- width 1) :y (- height 1) 
                            :font-size 8 :fill "#555555"
                            :text-anchor "end")
                  
                  ;; 使用图像切片技术而不是直接返回SVG
                  (with-temp-buffer
                    (let* ((target-px-w width)
                           (target-px-h height)
                           (image (grid-table-charts-svg-to-image svg target-px-w target-px-h)))
                      (insert-sliced-image image " " nil grid-table-image-target-char-height 
                                          (round (/ width (frame-char-width))))
                      (buffer-string))))
              ;; Fallback to text-based sparkline if no SVG support
              (grid-table-charts-text-sparkline-renderer value cell-props))
          (format "%s" value)))
    (format "%s" (or value ""))))

;;;----------------------------------------------------------------------
;;; Icon Set Renderer
;;;----------------------------------------------------------------------

(defun grid-table-charts-icon-set-renderer (value &optional cell-props)
  "Render values as icon sets based on their value.
VALUE should be a numeric string.
CELL-PROPS can contain:
- :type - Type of icon set ('traffic-light, 'arrows, 'shapes)
- :thresholds - List of threshold values (default: (80 60 40))"
  (if (and value (stringp value))
      (let* ((num (string-to-number value))
             (icon-type (or (plist-get cell-props :type) 'traffic-light))
             (thresholds (or (plist-get cell-props :thresholds) '(80 60 40)))
             (t1 (nth 0 thresholds))
             (t2 (nth 1 thresholds))
             (t3 (nth 2 thresholds))
             (icon (pcase icon-type
                     ('traffic-light
                      (cond
                       ((>= num t1) "🟢")
                       ((>= num t2) "🟡")
                       ((>= num t3) "🟠")
                       (t "🔴")))
                     ('arrows
                      (cond
                       ((>= num t1) "⬆️")
                       ((>= num t2) "↗️")
                       ((>= num t3) "➡️")
                       (t "↘️")))
                     ('shapes
                      (cond
                       ((>= num t1) "◆")
                       ((>= num t2) "◇")
                       ((>= num t3) "◈")
                       (t "◇")))
                     (_ "⚪"))))
        (format "%s %s" icon value))
    (format "%s" (or value ""))))

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun grid-table-charts-plugin-init ()
  "Initialize the charts plugin."
  ;; 确保使用全局变量
  (message "Before registration: hash table size: %d" (hash-table-count grid-table-cell-renderers))
  
  ;; 确保哈希表已初始化
  (unless (hash-table-p grid-table-cell-renderers)
    (setq grid-table-cell-renderers (make-hash-table)))
  
  ;; Register cell renderers with explicit global references
  (puthash 'data-bar #'grid-table-charts-data-bar-renderer grid-table-cell-renderers)
  (puthash 'text-sparkline #'grid-table-charts-text-sparkline-renderer grid-table-cell-renderers)
  (puthash 'svg-sparkline #'grid-table-charts-svg-sparkline-renderer grid-table-cell-renderers)
  (puthash 'icon-set #'grid-table-charts-icon-set-renderer grid-table-cell-renderers)
  
  ;; 验证渲染器是否已注册
  (message "After registration: hash table size: %d" (hash-table-count grid-table-cell-renderers))
  (message "data-bar renderer: %s" (gethash 'data-bar grid-table-cell-renderers))
  (message "text-sparkline renderer: %s" (gethash 'text-sparkline grid-table-cell-renderers))
  (message "svg-sparkline renderer: %s" (gethash 'svg-sparkline grid-table-cell-renderers))
  (message "icon-set renderer: %s" (gethash 'icon-set grid-table-cell-renderers))
  
  ;; Register plugin
  (add-to-list 'grid-table-plugins 'charts)
  
  (message "Charts plugin initialized"))



(provide 'grid-table-charts)
