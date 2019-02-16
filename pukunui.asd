(defsystem :pukunui
  :class :package-inferred-system
  :description "synth?"
  :author "TANAKA Shinichi"
  :depends-on ("bit-smasher"
               "bit-ops"

               "queues"
               "queues.simple-cqueue"

               "cl-portaudio"
               "cl-wav"

               "bordeaux-threads"

               "pukunui/main")
  :in-order-to ((test-op (test-op "pukunui-test"))))
