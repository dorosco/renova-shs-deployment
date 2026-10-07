(ns shs.core
  (:require [shs.sim :as shs]))

(defn run_scenario!
  [{:keys [namecase]}]
  (println (str "Corrida de Escenario >>> " namecase))
  (def rows (shs/simulate shs/base-params))
  (shs/print-table (shs/monthly-summary rows (:total-households shs/base-params)))
)

