(defsystem :pukunui
  :class :package-inferred-system
  :description "synth?"
  :author "TANAKA Shinichi"
  :depends-on ("bit-smasher"
               "bit-ops"

               "cl-portaudio"
               "cl-wav"

               "pukunui/main")
  :in-order-to ((test-op (test-op "pukunui-test"))))
