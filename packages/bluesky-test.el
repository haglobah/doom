;;; bluesky-test.el --- Tests for bluesky.el -*- lexical-binding: t; -*-
;;; Run via: emacs --batch -L /home/beat/.config/emacs/.local/straight/build-30.2/dash -l /home/beat/.config/doom/packages/bluesky-test.el -f ert-run-tests-batch-and-exit 2>&1

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'url)

;;; Code:
(defvar bluesky-test--dir (file-name-directory (or load-file-name buffer-file-name)))

;; Stub Doom macros so we can load outside Doom
(unless (fboundp 'load!)
  (defmacro load! (file &rest _)
    `(load (expand-file-name ,file bluesky-test--dir) nil t)))

(unless (fboundp 'map!)
  (defmacro map! (&rest _) nil))

;; Ensure dash is available (needed for ->>)
(require 'dash nil t)
(unless (fboundp '->>)
  (error "dash.el is required to run these tests"))

(load (expand-file-name "bluesky/mdx-to-richtext.el" bluesky-test--dir))
(load (expand-file-name "bluesky.el" bluesky-test--dir))

;;; bsky-append-posse-backlink

(ert-deftest bsky-append-posse-backlink-appends-text ()
  (let ((result (bsky-append-posse-backlink "Hello world")))
    (should (string-prefix-p "Hello world" result))
    (should (string-match-p "digital garden" result))))

(ert-deftest bsky-append-posse-backlink-preserves-original ()
  (let ((result (bsky-append-posse-backlink "")))
    (should (string-match-p "Syndicated from my" result))))

;;; bsky--handle-response

(ert-deftest bsky--handle-response-reports-error ()
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (bsky--handle-response '(:error (error http 500)))
      (should (string-match-p "failed" msg)))))

(ert-deftest bsky--handle-response-reports-success ()
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (with-temp-buffer
        (insert "HTTP/1.1 200 OK\n\n")
        (insert (json-encode '((uri . "at://did:plc:abc/post/123"))))
        (goto-char (point-min))
        (search-forward "\n\n")
        (bsky--handle-response '())
        (should (string-match-p "Posted to Bluesky" msg))))))

;;; bsky--post-json

(ert-deftest bsky--post-json-sends-correct-request ()
  (let (captured-url captured-method captured-data captured-headers)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (setq captured-url url)
                 (setq captured-method url-request-method)
                 (setq captured-data url-request-data)
                 (setq captured-headers url-request-extra-headers))))
      (bsky--post-json "/post" '((text . "hello")))
      (should (equal captured-url "http://localhost:3000/post"))
      (should (equal captured-method "POST"))
      (should (assoc "Content-Type" captured-headers))
      (let ((decoded (json-read-from-string (decode-coding-string captured-data 'utf-8))))
        (should (equal (alist-get 'text decoded) "hello"))))))

(ert-deftest bsky--post-json-encodes-utf8 ()
  (let (captured-data)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (setq captured-data url-request-data))))
      (bsky--post-json "/post" '((text . "Sch\u00f6ne Gr\u00fc\u00dfe")))
      (let ((decoded (json-read-from-string (decode-coding-string captured-data 'utf-8))))
        (should (equal (alist-get 'text decoded) "Sch\u00f6ne Gr\u00fc\u00dfe"))))))

;;; bsky-post

(ert-deftest bsky-post-sends-title-text-facets ()
  (let (captured-payload)
    (cl-letf (((symbol-function 'bsky--post-json)
               (lambda (endpoint payload)
                 (setq captured-payload payload))))
      (let ((ret (make-ret :title "My Title" :text "body" :facets '() :tags "#tag")))
        (bsky-post ret)
        (should (equal (alist-get 'title captured-payload) "My Title"))
        (should (equal (alist-get 'text captured-payload) "body"))
        (should (equal (alist-get 'facets captured-payload) '()))))))

;;; bsky-post-as-image

(ert-deftest bsky-post-as-image-sends-correct-payload ()
  (let (captured-endpoint captured-payload)
    (cl-letf (((symbol-function 'bsky--post-json)
               (lambda (endpoint payload)
                 (setq captured-endpoint endpoint)
                 (setq captured-payload payload))))
      (bsky-post-as-image "my text" "alt" "stream#00001")
      (should (equal captured-endpoint "/post/as-image"))
      (should (equal (alist-get 'text captured-payload) "my text"))
      (should (equal (alist-get 'alttext captured-payload) "alt"))
      (should (equal (alist-get 'link captured-payload) "stream#00001")))))

;;; bsky--post-text

(ert-deftest bsky--post-text-appends-backlink-and-posts ()
  (let (posted-ret)
    (cl-letf (((symbol-function 'bsky-post)
               (lambda (ret) (setq posted-ret ret))))
      (bsky--post-text "Hello")
      (should posted-ret)
      (should (string-match-p "digital garden" (ret-text posted-ret)))
      (should (string-match-p "Hello" (ret-text posted-ret))))))

;;; bsky--image-post-text

(ert-deftest bsky--image-post-text-returns-default-when-nil ()
  (let ((ret (make-ret :title "Title" :text "body" :facets '() :tags "#tag")))
    (should (equal (bsky--image-post-text ret nil) "Title\n\n#tag"))))

(ert-deftest bsky--image-post-text-prompts-when-type-title ()
  (let ((ret (make-ret :title "Title" :text "body" :facets '() :tags "#tag")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt initial) initial)))
      (should (equal (bsky--image-post-text ret t) "Title\n\n#tag")))))

;;; bsky--buffer-link

(ert-deftest bsky--buffer-link-returns-stream-link ()
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda () "/home/beat/beathagenlocher.com/src/content/stream/00042.mdx")))
    (should (equal (bsky--buffer-link) "stream#00042"))))

(ert-deftest bsky--buffer-link-returns-page-link ()
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda () "/home/beat/beathagenlocher.com/src/content/my-post.mdx")))
    (should (equal (bsky--buffer-link) "my-post"))))

;;; Integration: full pipeline without IO

(ert-deftest bsky--post-text-full-pipeline ()
  "Verify the full append -> parse -> post pipeline with markdown links."
  (let (posted-ret)
    (cl-letf (((symbol-function 'bsky-post)
               (lambda (ret) (setq posted-ret ret))))
      (bsky--post-text "Check out [this](https://example.com)!")
      (should posted-ret)
      (should (string-match-p "Check out this!" (ret-text posted-ret)))
      (should (> (length (ret-facets posted-ret)) 0))
      ;; The backlink should also produce a facet
      (should (>= (length (ret-facets posted-ret)) 2)))))

(provide 'bluesky-test)
;;; bluesky-test.el ends here
