(require 'cl)
(require 'dash)

(defun xml+--node-matches (node matcher)
  "Return non-nil if xml/html node NODE matches query selector
element MATCHER, an attribute-value plist optionally beginning
with a tag name or list of tag names, e.g. ((p div) :class
\"content\")

Begin MATCHER with ~ to negate the match

If MATCHER is a list of lists, it matches node if node matches
any of its members.
"
  (if (eq (type-of node) 'string)
      nil ;; TODO text matching
    (let ((tags nil)
          (text nil)
          (negative nil))
      (setq matcher (-clone matcher))
      (if (-every? 'listp matcher)
          (-any? `(lambda (m) (xml+--node-matches (quote ,node) m)) matcher)
        (when (eq (car matcher) '~)
          (setq negative t)
          (setq matcher (cdr matcher)))

        (when (and matcher
                   (not (keywordp (first matcher))))
          (setq tags (-flatten (list (pop matcher)))))

        (not (equal ;; xor
							negative
							(and
               ;; Match tags
							 (or (not tags) (memq (xml-node-name node) tags))
               ;; Match attributes
							 (let ((matches t))
								 (while (and matcher matches)
									 (when (not (keywordp (car matcher)))
										 (error (format "Expected keyword in matcher, got %s"
																		(symbol-name (type-of (car matcher))))))
									 (setq matches
												 (equal (cdr (assoc (intern (substring (symbol-name (car matcher)) 1))
																						(xml-node-attributes node)))
																(cadr matcher)))
									 (setq matcher (cddr matcher)))
								 matches))))))))

(defun xml+-query--generic (just-one nodes query &optional result must-be-root)
	"Recursive depth first search to collect one or all nodes in
NODES matching QUERY."
	;; Make cons cell to store a persistent reference to the result
  (unless result (setq result (cons nil nil)))
  (if (equal (car query) '>)
      (xml+-select nodes (cdr query) result t)
    (when (eq (type-of (car nodes)) 'symbol)
      (setq nodes (list nodes)))

    (let ((nodes-remaining (-filter (lambda (n) (and n (listp n))) nodes))
          (node nil))

      (while nodes-remaining
        (setq node (pop nodes-remaining))
        (if (xml+--node-matches node (car query))
            (if (cdr query)
                (xml+-query--generic just-one (xml-node-children node) (cdr query) result)
              (setcdr result (append (cdr result) (list node)))
              (when just-one (setq nodes-remaining nil)))
          (unless must-be-root
            (xml+-query--generic just-one (xml-node-children node) query result)))))
    (cdr (reverse result))))

(defun xml+-query-all (nodes query &optional must-be-root)
	"Search NODES for matches.
NODES is a single xml or html node or a list of them.

QUERY is a list of node matchers optionally preceded by the
symbol `>'. Node matchers are usually descendant matchers. When
preceded by '> they are direct decendant matchers.

For example, the analog to the Javascript call
document.querySelectorAll(\"body div#post > p.content\") is:

	(xml+-query-all document '((body)
														 (div :id \"post\") >
														 (p :class \"content\")
														 (a)))

See also `xml-node-matches'"
	(xml+-query--generic nil nodes query must-be-root))

(defun xml+-query-first (nodes query &optional must-be-root)
	"Like `xml+-query-all', but only return the first match."
  (xml+-query--generic t nodes query must-be-root))

(defun xml+-node-text (node)
  "Get the text of NODE"
  (if (stringp node)
      node
    (-reduce (lambda (&optional n1 n2)
               (if n1
                   (concat (xml-node-text n1) (xml-node-text n2))
                 ""))
             (xml-node-children node))))

(provide 'xml+)
