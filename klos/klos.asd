(defsystem :klos
  :description "Kiwi Lightweight Object System"
  :author "TANAKA Shinichi <shinichi.tanaka45@gmail.com>"
  :version "0.0.1"
  :license "?"
  :components ((:file "class")
               (:file "package" :depends-on ("class"))))
