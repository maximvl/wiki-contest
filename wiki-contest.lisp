;;;; wiki-contest.lisp

(in-package #:wiki-contest)

;;; "wiki-contest" goes here. Hacks and glory await!

(defun a-tag-callback (tag)
  (let* ((a (car tag))
         (pos (position :href a)))
    (when pos
      (let ((href (nth (1+ pos) a)))
        (when (and (alexandria:starts-with-subseq "/wiki/" href)
                   (not (find #\: href))
                   (not (find #\# href)))
          href)))))

(defun find-links (url)
  (check-type url string)
  (let* (result
         (url (format nil "https://ru.wikipedia.org~a" url))
         (stream (drakma:http-request
                  url
                  :want-stream t
                  :external-format-out :utf8)))
    (cl-html-parse:parse-html stream
     :callback-only t
     :callbacks `((:a . ,#'(lambda (tag)
                             (let ((r (a-tag-callback tag)))
                               (when r (push r result)))))))
    (nreverse result)))

(defun print-parent (url dict)
  (loop for curr = url
     then (gethash curr dict)
     until (eq curr t)
     do (format t "~&~a" curr)))

(defun search-path (from to)
  (check-type from string)
  (check-type to string)
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
                    (when (equalp a to)
                      (return-from external))
                    (cl-containers:enqueue q a)))))
    (print-parent to hash)))

