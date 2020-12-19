(ns quippy.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [quippy.core-test]))

(doo-tests 'quippy.core-test)

