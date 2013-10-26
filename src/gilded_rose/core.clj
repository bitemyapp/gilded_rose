(ns gilded-rose.core)

(defn update-quality [items]
  (map
   (fn [item] (cond

               (and (< (:sell-in item) 0) (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))
               (merge item {:quality 0})

               (or (= (:name item) "Aged Brie") (= (:name item) "Backstage passes to a TAFKAL80ETC concert"))
               (if (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 5) (< (:sell-in item) 10))
                 (merge item {:quality (inc (inc (:quality item)))})
                 (if (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 0) (< (:sell-in item) 5))
                   (merge item {:quality (inc (inc (inc (:quality item))))})
                   (if (< (:quality item) 50)
                     (merge item {:quality (inc (:quality item))})
                     item)))

               (< (:sell-in item) 0)
               (if (= "Backstage passes to a TAFKAL80ETC concert" (:name item))
                 (merge item {:quality 0})
                 (if (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
                   (merge item {:quality (- (:quality item) 2)})
                   item))

               (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
               (merge item {:quality (dec (:quality item))})

               :else item))

   (map (fn [item]
          (if (not= "Sulfuras, Hand of Ragnaros" (:name item))
            (merge item {:sell-in (dec (:sell-in item))})
            item)) items)))

(defn item [item-name sell-in quality]
  {:name item-name :sell-in sell-in :quality quality})

(def inventory
  [(item "+5 Dexterity Vest" 10 20)
   (item "Aged Brie" 2 0)
   (item "Elixir of the Mongoose" 5 7)
   (item "Sulfuras, Hand of Ragnaros" 0 80)
   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)])

(def constraints
  {:is-epic              #(not= (:name %) "Sulfuras, Hand of Ragnaros")
   :backstage            #(= "Backstage passes to a TAFKAL80ETC concert" (:name %))
   :brie                 #(= (:name %) "Aged Brie")
   :sell-in-not-zero     #(and (< (:sell-in %) 0))
   :sell-in-between-5-10 #(and (>= (:sell-in %) 5) (< (:sell-in %) 10))
   :sell-in-between-0-5  #(and (>= (:sell-in %) 0) (< (:sell-in %) 5))
   :quality-below-50     #(< (:quality %) 50)
   :sell-in-below-0      #(< (:sell-in %) 0)
   :dex-vest             #(= "+5 Dexterity Vest" (:name %))
   :mongoose             #(= "Elixir of the Mongoose" (:name %))
   :dex-or-mongoose      #(or (= "+5 Dexterity Vest" (:name %)) (= "Elixir of the Mongoose" (:name %)))})


(def rules
  [
   [[:is-epic true] #(update-in % [:sell-in] dec)]

   [[:backstage true :sell-in-not-zero true] #(merge % {:quality 0})]

   [[:backstage true :brie true :sell-in-between-5-10 true]
    #(merge % {:quality (inc (inc (:quality %)))})]

   [[:backstage true :brie true :sell-in-between-0-5 false :quality-not-more-50 true]
    #(merge % {:quality (inc (:quality %))})]

   [[:sell-in-below-0 true :backstage true]
    #(merge % {:quality 0})]

   [[:sell-in-below-0 true :backstage false :dex-or-mongoose true]
    #(merge % {:quality (- (:quality %) 2)})]

   [[:dex-or-mongoose true]
    #(merge % {:quality (dec (:quality %))})]

   ])

(use 'clojure.tools.trace)

(deftrace apply-rule [item rule-pair]
  (let [[[rule qualifier] update-fn] rule-pair
        rule-fn (constraints rule)]
    (= (rule-fn item) qualifier)))

(deftrace apply-rules [item rule-vec]
  (mapv (partial apply-rule item) (partition 2 rule-vec)))

(deftrace process-item [rules-vec item]
  (mapv (partial apply-rules item) rules-vec))

(deftrace progress-one-day [items]
  (mapv (partial process-item rules) items))

(defn update-current-inventory []
  (update-quality inventory))
