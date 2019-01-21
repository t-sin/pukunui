(defsystem :pukunui
  :class :package-inferred-system
  :description "synth?"
  :author "TANAKA Shinichi"
  :depends-on ("cl-portaudio"
               "pukunui/main")
  :in-order-to ((test-op (test-op "pukunui-test"))))
