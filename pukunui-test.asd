(defsystem :pukunui-test
  :class :package-inferred-system
  :description "Tests for Pukunui"
  :depends-on ("rove"
               "pukunui"
               "pukunui-test/tests/signal"
               "pukunui-test/tests/time")
  :perform (test-op (o c) (uiop:symbol-call :rove ':run c)))
