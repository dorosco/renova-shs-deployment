(ns shs.sim
  "Deployment simulation for a Solar Home System (SHS) rollout.
   Part 1: pure model (params -> rows of crews/installations per subcontractor-month).
   Part 2: Datomic persistence + queries so scenarios can be stored and compared."
  (:require [datomic.client.api :as d])
  (:import (java.time YearMonth)))

;; ---------------------------------------------------------------------------
;; 1. Parameters
;; ---------------------------------------------------------------------------

(def base-params
  {:scenario/name       "base"
   :total-households    200000
   :horizon-months      24
   :working-days        22      ; per month
   :houses-per-crew-day 0.9
   :efficiency          1.0     ; 0-1, losses from weather, logistics, rework, travel
   :peak-crews          550     ; total crews when ALL subcontractors are at full capacity
   :capacity-ratio      1.5    ; type A capacity / type B capacity (1.5 .. 2.0)
   :ramp-months         3       ; months for a subcontractor to reach full capacity
   :start-month         "2027-09" ; calendar month of simulation month 1 (yyyy-MM)
   :rainy-months        #{11 12 1 2} ; calendar months (1-12) with rain, Nov-Feb
   :rain-reduction      0.40    ; fraction of crew performance lost in rainy months
   :subcontractors
   [{:id "A1" :type :A :start 1}
    {:id "A2" :type :A :start 1}
    {:id "B1" :type :B :start 1}
    {:id "A3" :type :A :start 3}
    {:id "B2" :type :B :start 1}
    {:id "A4" :type :A :start 3}
    {:id "B3" :type :B :start 5}
    {:id "A5" :type :A :start 5}
    {:id "B4" :type :B :start 5}
    {:id "B5" :type :B :start 5}]})

;; ---------------------------------------------------------------------------
;; 2. Pure model
;; ---------------------------------------------------------------------------

(defn crews-by-type
  "Full-capacity crews of one subcontractor of each type, such that the sum of all
   subcontractors at full capacity equals :peak-crews and A = ratio * B."
  [{:keys [subcontractors peak-crews capacity-ratio]}]
  (let [n-a (count (filter #(= :A (:type %)) subcontractors))
        n-b (count (filter #(= :B (:type %)) subcontractors))
        b   (/ peak-crews (+ (* n-a capacity-ratio) n-b))]
    {:A (* capacity-ratio b) :B b}))

(defn ramp-factor
  "Fraction of full capacity in `month` for a subcontractor that starts in `start`.
   Linear mobilization over `ramp` months."
  [month start ramp]
  (if (< month start)
    0.0
    (min 1.0 (/ (inc (- month start)) (double ramp)))))

(defn calendar-month
  "YearMonth for simulation month `m` (1-based), given start-month \"yyyy-MM\"."
  ^YearMonth [start-month m]
  (.plusMonths (YearMonth/parse start-month) (long (dec m))))

(defn weather-factor
  "Performance multiplier for simulation month `m`: (1 - rain-reduction) in
   rainy calendar months, 1.0 otherwise."
  [{:keys [start-month rainy-months rain-reduction]} m]
  (if (contains? rainy-months (.getMonthValue (calendar-month start-month m)))
    (- 1.0 rain-reduction)
    1.0))

(defn simulate
  "Returns a vector of maps {:month :sub :crews :installed}.
   Stops when the total of households is reached (last month is trimmed,
   proportionally across subcontractors) or the horizon ends."
  [{:keys [subcontractors total-households horizon-months working-days
           houses-per-crew-day efficiency ramp-months start-month] :as params}]
  (let [full (crews-by-type params)]
    (loop [m 1, remaining (double total-households), rows []]
      (if (or (> m horizon-months) (<= remaining 0.0))
        rows
        (let [weather  (weather-factor params m)
              cal      (str (calendar-month start-month m))
              per-crew (* working-days houses-per-crew-day efficiency weather)
              demand (for [{:keys [id type start]} subcontractors
                           :let [crews (* (full type) (ramp-factor m start ramp-months))]
                           :when (pos? crews)]
                       {:month m :cal cal :weather weather :sub id :crews crews
                        :potential (* crews per-crew)})
              pot    (reduce + 0.0 (map :potential demand))
              scale  (if (> pot remaining) (/ remaining pot) 1.0)
              rs     (mapv #(-> % (assoc :installed (* scale (:potential %)))
                                (dissoc :potential))
                           demand)]
          (recur (inc m)
                 (- remaining (reduce + 0.0 (map :installed rs)))
                 (into rows rs)))))))

(defn monthly-summary
  "Aggregate rows by month: active subcontractors, crews, weather factor,
   installed, cumulative, % of target."
  [rows total]
  (->> (group-by :month rows)
       (sort-by key)
       (reduce (fn [{:keys [acc cum]} [m rs]]
                 (let [inst (reduce + (map :installed rs))
                       cum' (+ cum inst)]
                   {:acc (conj acc {:month m
                                    :cal (:cal (first rs))
                                    :weather (:weather (first rs))
                                    :subs (count (distinct (map :sub rs)))
                                    :crews (reduce + (map :crews rs))
                                    :installed inst
                                    :cumulative cum'
                                    :pct (* 100.0 (/ cum' total))})
                    :cum cum'}))
               {:acc [] :cum 0.0})
       :acc))

(defn total-installed [params]
  (reduce + 0.0 (map :installed (simulate params))))

(defn min-peak-crews
  "Smallest :peak-crews (bisection) that still completes :total-households within
   the horizon. Useful to check whether 500 crews is oversized for a 24-month plan."
  [params]
  (loop [lo 1.0, hi 5000.0, i 0]
    (let [mid (/ (+ lo hi) 2)]
      (cond
        (> i 60) hi
        (>= (total-installed (assoc params :peak-crews mid))
            (- (:total-households params) 0.5)) (recur lo mid (inc i))
        :else (recur mid hi (inc i))))))

(defn print-table [summary]
  (println "Month  Calendar  Subs  Crews  Weather  Installed  Cumulative      %")
  (doseq [{:keys [month cal subs crews weather installed cumulative pct]} summary]
    (println (format "%5d  %-8s %5d %6.0f %8.2f %10.0f %11.0f %6.1f"
                    month cal subs crews weather installed cumulative pct))))

;; ---------------------------------------------------------------------------
;; 3. Datomic schema
;; ---------------------------------------------------------------------------

(def schema
  [{:db/ident :scenario/name   :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :scenario/params :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/doc "EDN of the input parameters, for reproducibility"}
   {:db/ident :sub/uid         :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity
    :db/doc "<scenario>/<sub-id>"}
   {:db/ident :sub/id          :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :sub/scenario    :db/valueType :db.type/ref    :db/cardinality :db.cardinality/one}
   {:db/ident :sub/type        :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/ident :sub/start-month :db/valueType :db.type/long   :db/cardinality :db.cardinality/one}
   {:db/ident :deploy/uid       :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity
    :db/doc "<scenario>/<sub-id>/<month>"}
   {:db/ident :deploy/sub       :db/valueType :db.type/ref    :db/cardinality :db.cardinality/one}
   {:db/ident :deploy/month     :db/valueType :db.type/long   :db/cardinality :db.cardinality/one}
   {:db/ident :deploy/calendar-month :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/doc "yyyy-MM"}
   {:db/ident :deploy/weather-factor :db/valueType :db.type/double :db/cardinality :db.cardinality/one
    :db/doc "1.0 = dry season, 1 - rain-reduction = rainy season"}
   {:db/ident :deploy/crews     :db/valueType :db.type/double :db/cardinality :db.cardinality/one}
   {:db/ident :deploy/installed :db/valueType :db.type/double :db/cardinality :db.cardinality/one}])

;; ---------------------------------------------------------------------------
;; 4. Datomic persistence & queries
;; ---------------------------------------------------------------------------

(defn connect!
  "In-memory Datomic Local. For disk storage use :storage-dir \"/path\"."
  []
  (let [client (d/client {:server-type :datomic-local :system "renova-simulation"})]
    (d/create-database client {:db-name "simulations"})
    (let [conn (d/connect client {:db-name "simulations"})]
      (d/transact conn {:tx-data schema})
      conn)))

(defn save-scenario!
  "Runs the simulation and stores the scenario, its subcontractors and monthly rows."
  [conn {:keys [subcontractors] :as params}]
  (let [scn  (:scenario/name params)
        rows (simulate params)]
    (d/transact conn {:tx-data [{:scenario/name scn
                                 :scenario/params (pr-str (dissoc params :scenario/name))}]})
    (d/transact conn {:tx-data (for [{:keys [id type start]} subcontractors]
                                 {:sub/uid (str scn "/" id) :sub/id id
                                  :sub/scenario [:scenario/name scn]
                                  :sub/type type :sub/start-month start})})
    (d/transact conn {:tx-data (for [{:keys [month cal weather sub crews installed]} rows]
                                 {:deploy/uid (str scn "/" sub "/" month)
                                  :deploy/sub [:sub/uid (str scn "/" sub)]
                                  :deploy/month month
                                  :deploy/calendar-month cal
                                  :deploy/weather-factor weather
                                  :deploy/crews crews
                                  :deploy/installed installed})})
    (count rows)))

;; NOTE: :with ?dp keeps duplicate values (e.g. identical monthly figures) from
;; being collapsed before aggregation. Without it, sums would be wrong.

(defn installed-by-month [db scn]
  (->> (d/q '[:find ?month (sum ?inst) (sum ?crews)
              :with ?dp
              :in $ ?scn
              :where [?e :scenario/name ?scn]
                     [?s :sub/scenario ?e]
                     [?dp :deploy/sub ?s]
                     [?dp :deploy/month ?month]
                     [?dp :deploy/installed ?inst]
                     [?dp :deploy/crews ?crews]]
            db scn)
       (sort-by first)
       (map (fn [[m inst crews]] {:month m :installed inst :crews crews}))))

(defn active-subs-by-month [db scn]
  (->> (d/q '[:find ?month (count-distinct ?s)
              :in $ ?scn
              :where [?e :scenario/name ?scn]
                     [?s :sub/scenario ?e]
                     [?dp :deploy/sub ?s]
                     [?dp :deploy/month ?month]]
            db scn)
       (sort-by first)))

(defn installed-by-type [db scn]
  (->> (d/q '[:find ?type (sum ?inst)
              :with ?dp
              :in $ ?scn
              :where [?e :scenario/name ?scn]
                     [?s :sub/scenario ?e]
                     [?s :sub/type ?type]
                     [?dp :deploy/sub ?s]
                     [?dp :deploy/installed ?inst]]
            db scn)
       (into {})))

(defn installed-by-sub [db scn]
  (->> (d/q '[:find ?id ?month ?inst
              :in $ ?scn
              :where [?e :scenario/name ?scn]
                     [?s :sub/scenario ?e]
                     [?s :sub/id ?id]
                     [?dp :deploy/sub ?s]
                     [?dp :deploy/month ?month]
                     [?dp :deploy/installed ?inst]]
            db scn)
       (sort-by (juxt first second))))

;; ---------------------------------------------------------------------------
;; 5. REPL usage
;; ---------------------------------------------------------------------------

(comment
  ;; Pure model
  (def rows (simulate base-params))
  (print-table (monthly-summary rows (:total-households base-params)))
  (min-peak-crews base-params)        ; crews needed to finish exactly in 24 months

  ;; Weather variants (same engine, just parameters)
  (print-table (monthly-summary
                (simulate (assoc base-params :rain-reduction 0.0))   ; no weather effect
                200000))
  (print-table (monthly-summary
                (simulate (assoc base-params :rain-reduction 0.5 :rainy-months #{10 11 12 1 2 3}))
                200000))

  ;; Datomic
  (def conn (connect!))
  (save-scenario! conn base-params)
  (def db (d/db conn))
  (installed-by-month db "base")
  (installed-by-type db "base")
  (installed-by-sub db "base")

  ;; Variant: ratio 2.0, slower ramp, 85% efficiency, 24-month fit
  (let [p (assoc base-params :scenario/name "ratio2-eff85"
                 :capacity-ratio 2.0 :ramp-months 4 :efficiency 0.85)
        p (assoc p :peak-crews (min-peak-crews p))]
    (save-scenario! conn p)
    (installed-by-month (d/db conn) "ratio2-eff85")))
