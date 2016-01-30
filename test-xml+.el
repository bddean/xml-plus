(defun test-xml+-parse-string (str)
  (with-temp-buffer
    (insert str)
    (xml-parse-region (point-min) (point-max))))

(defvar test-xml+-sample1 (test-xml+-parse-string
                           "<html>
    <body>
        <div class=\"container\"> 
            <p class=\"content\">
                Nullam rutrum.  <em>Donec neque quam,</em> dignissim in, mollis nec,
                sagittis eu, wisi.  
            </p>
            <p class=\"content\">
                Sed id ligula quis est <a href=\"http://test1.html\"> convallis</a>
                tempor.  Praesent fermentum tempor tellus.  
            </p>
            <p class=\"content\">
                Sed diam.  Mauris mollis <em>tincidunt</em> felis.  
            </p>
            <p class=\"content\">
                Mauris ac felis vel <a href=\"http://test2.html\"> velit tristique</a>
                imperdiet.  Cum sociis natoque penatibus et magnis dis parturient
                montes, nascetur ridiculus mus.  
            </p>
        </div>
    </body>
</html>"))

(test-xml+-parse-string test-xml+-html)

(ert-deftest test-queryall ()
  (equal
   (xml+-query-all test-xml+-sample1
                   '((div :class "container") > (p)  (a))
                   )
   '((a
      ((href . "http://test2.html"))
      " velit tristique")
     (a
      ((href . "http://test1.html"))
      " convallis"))))

(ert-deftest test-xml+-node-string ()
  (equal (xml+-node-text (car test-xml+-sample1))
         "Nullam rutrum. Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi. Sed id ligula quis est convallis tempor. Praesent fermentum tempor tellus. Sed diam. Mauris mollis tincidunt felis. Mauris ac felis vel velit tristique imperdiet. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus."))

(ert-run-tests-interactively t)
