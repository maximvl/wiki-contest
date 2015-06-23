;;;; wiki-contest.lisp

(in-package #:wiki-contest)

;;; "wiki-contest" goes here. Hacks and glory await!

(declaim (optimize speed))

(defun a-tag-callback (tag)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (cons tag))
  (let ((a (car tag)))
    (declare (type cons a))
    (let ((pos (position :href a)))
      (declare (type (or fixnum null) pos))
      (when pos
        (let ((href (nth (1+ pos) a)))
          (declare (simple-string href))
          (when (and (alexandria:starts-with-subseq "/wiki/" href)
                     (not (find #\: href))
                     (not (find #\# href)))
            href))))))

(defun escape-unicode (url)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (simple-string url))
  (babel:octets-to-string (babel:string-to-octets url :encoding :utf-8) :encoding :latin-1))

(defun find-links (url)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (simple-string url))
  (let* (result
         (url (format nil "https://ru.wikipedia.org~a" (escape-unicode url)))
         (stream (drakma:http-request
                  url
                  :want-stream t
                  :external-format-out :utf8)))
    (cl-html-parse:parse-html stream
     :callback-only t
     :callbacks `((:a . ,#'(lambda (tag)
                             (let ((r (a-tag-callback tag)))
                               (when r (push r result)))))))
    (close stream)
    (nreverse result)))

(defun print-parent (url dict)
  (loop for curr = url
     then (gethash curr dict) 
     until (eq curr t)
     do (format t "~&~a" (percent-encoding:decode curr))))

(defun search-path (from to)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (simple-string from)
           (simple-string to))
  (let ((hash (make-hash-table :test #'equalp))
        (q (make-instance 'cl-containers:basic-queue)))
    (setf (gethash from hash) t)
    (block external
      (loop for curr = from
         then (cl-containers:dequeue q)
         do (loop for a in (find-links curr)
               unless (gethash a hash)
               do (progn
                    (setf (gethash a hash) curr)
                    (when (string-equal a to)
                      (return-from external))
                    (cl-containers:enqueue q a)))))
    (print-parent to hash)))

(defun profile-contest (f)
  (sb-profile:reset)
  (sb-profile:profile a-tag-callback find-links print-parent search-path)
  (funcall f)
  (sb-profile:report))
