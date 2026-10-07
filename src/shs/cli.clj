(ns shs.cli
  (:require [datomic.client.api :as d]
	    [shs.core :as shs]))

(defn -main
  [{:keys [case_name]
    :or {case_name "Corrida de prueba"}}]
  (shs/run_scenario! case_name))

