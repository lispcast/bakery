(ns bakery.core
  (:require [clojure.string :as string]))

(def bowl-state (ref {}))
(def bowl-ingredients (ref {}))
(def pan-state (ref {}))
(def hand-state (ref nil))
(def cooling-rack (ref {}))
(def location (ref :prep-area))
(def loaded (ref {}))
(def prep-stock (ref {:egg 3
                      :flour 3
                      :sugar 2
                      :butter 1
                      :milk 1}))

(def _ingredients #{:egg :flour :milk :sugar :butter :cocoa})
(def _fridge-ingredients #{:egg :milk :butter})
(def _pantry-ingredients #{:flour :sugar :cocoa})
(def _grabbables #{:egg :cup :butter})
(def _scoopables #{:flour :milk :sugar :cocoa :ganache})
(def _squeezables #{:egg})
(def _locations #{:prep-area :fridge :pantry})


(defn- from-cup [x]
  (and
   x
   (= (seq "cup-of-") (take 7 (name x)))
   (keyword (.substring (name x) 7))))

(defn- from-squeezed [x]
  (and
   x
   (= (seq "squeezed-") (take (count "squeezed-") (name x)))
   (keyword (.substring (name x) (count "squeezed-")))))


;; just to get a clean state again

(defn start-over
  "Clean up the bakery, empty hands, clean out the bowl, get a new pan. Good when you mess up and need a fresh start."
  []
  (dosync
   (ref-set bowl-state {})
   (ref-set bowl-ingredients {})
   (ref-set pan-state {})
   (ref-set hand-state nil)
   (ref-set cooling-rack {})
   (ref-set location :prep-area)
   (ref-set loaded {})
   (ref-set prep-stock {}))

  :ok)

(defn- error [& args]
  (apply println args)
  :error)

(defn grab
  "Grab an item from the bakery (:egg, :butter, or :cup)."
  [ingredient]
  (cond
   (not= @location :prep-area)
   (error "I can only grab things in the prep area. I am at the" @location)

   (not (_grabbables ingredient))
   (error "I cannot grab" ingredient)

   (not (or (= :cup ingredient)
            (pos? (@prep-stock ingredient 0))))
   (error ingredient "is not available at the prep area")

   @hand-state
   (error "I already have" (name @hand-state) "in my hand.")

   :else
   (dosync
    (ref-set hand-state ingredient)
    (when (not= :cup ingredient)
      (alter prep-stock update-in [ingredient] dec))
    :ok)))

(defn scoop
  "If you are holding the cup, scoop up something into your measuring cup
   (milk, flour, or sugar)."
  [ingredient]
  (cond
   (not= @location :prep-area)
   (error "I can only scoop things in the prep area. I am at the" @location)
   (not= @hand-state :cup)
   (error "I need to have the cup in my hand to scoop.")

   (not (_scoopables ingredient))
   (error ingredient "is not scoopable.")

   (not (pos? (@prep-stock ingredient 0)))
   (error ingredient "is not available at the prep area")

   :everything-is-ok
   (dosync
    (ref-set hand-state (keyword (str "cup-of-" (name ingredient))))
    (alter prep-stock update-in [ingredient] dec)
    :ok)))

(defn squeeze
  "Squeeze the egg you are holding to crack it."
  []
  (cond
   (nil? @hand-state)
   (error "I am not holding anything.")

   (not (_squeezables @hand-state))
   (error "I am holding" @hand-state ", which is not squeezable.")

   :its-squeezable
   (dosync
    (alter hand-state #(keyword (str "squeezed-" (name %))))
    :ok)))

(defn release
  "Release whatever you are holding in your hand."
  []
  (cond
   (nil? @hand-state)
   (error "I am not holding anything.")

   (not= @location :prep-area)
   (error "Please only release things in the prep area.")

   :else
   (dosync
    (cond
     (= @hand-state :cup)
     :ok

     (from-cup @hand-state)
     (alter prep-stock update-in [(from-cup @hand-state)] (fnil inc 0))

     (from-squeezed @hand-state)
     (alter prep-stock update-in [(from-squeezed @hand-state)] (fnil inc 0))

     :else
     (alter prep-stock update-in [@hand-state] (fnil inc 0)))
    (ref-set hand-state nil)

    :ok)))

(defn add-to-bowl
  "Add what is in your hand to the bowl."
  []
  (cond
   (not= @location :prep-area)
   (error "I can only add things to the bowl in the prep area. I am at the" @location)

   (nil? @hand-state)
   (error "I am not holding anything.")

   (= :butter @hand-state)
   (dosync
    (alter bowl-ingredients update-in [:butter] (fnil inc 0))
    (ref-set hand-state nil)
    (alter bowl-state assoc :mixed false)
    :ok)

   (= :egg @hand-state)
   (error "Shouldn't I break the egg I am holding first?")

   (= :cup @hand-state)
   (error "My cup is empty.")

   (from-cup @hand-state)
   (dosync
    (alter bowl-ingredients update-in [(from-cup @hand-state)] (fnil inc 0))
    (ref-set hand-state :cup)
    (alter bowl-state assoc :mixed false)
    :ok)

   (from-squeezed @hand-state)
   (dosync
    (alter bowl-ingredients update-in [(from-squeezed @hand-state)] (fnil inc 0))
    (ref-set hand-state nil)
    (alter bowl-state assoc :mixed false)
    :ok)

   :otherwise
   (error "I'm lost.")))

(defn mix
  "Mix the contents of the bowl."
  []
  (cond
   (not= @location :prep-area)
   (error "I can only mix the bowl when I'm in the prep area. I'm at the" @location)

   (empty? @bowl-state)
   (error "The bowl is empty."))

  :else
  (dosync
   (when (= {:butter 2
             :cocoa 2
             :sugar 1} @bowl-ingredients)
     (ref-set bowl-ingredients {:ganache 4}))
   (alter bowl-state assoc :mixed true)
   :ok))

(defn add-contents [a b]
  (if (or (nil? a) (map? a))
    (merge-with + a b)
    :randomness))

(defn pour-into-pan
  "Pour the contents of the bowl into the baking pan."
  []
  (cond
   (not (:mixed @bowl-state))
   (error "The bowl is unmixed.")

   :else
   (dosync
    (alter pan-state update-in [:contents] add-contents @bowl-ingredients)
    (ref-set bowl-ingredients {})
    (ref-set bowl-state {})
    :ok)))

(defn- bake [p m]
  (cond
   (= p
      {:flour 1
       :egg 1
       :sugar 1
       :butter 1})
   (cond
    (< m 30)
    :mushy-mess
    (> m 30)
    :burned-mess
    (= m 30)
    :cookies)

   (= p
      {:flour 2
       :egg 2
       :milk 1
       :sugar 1})
   (cond
    (< m 25)
    :mushy-mess
    (> m 25)
    :burned-mess
    (= m 25)
    :cake)

   (= p
      {:flour 2
       :milk 1
       :egg 2
       :ganache 4})
   (cond
    (< m 35)
    :mushy-mess
    (> m 35)
    :burned-mess
    (= m 35)
    :brownies)

   (nil? p)
   (do
     (println "Baking an empty pan.")
     nil)

   :else
   :randomness))

(defn bake-pan
  "Put the pan in the oven and bake it for so many minutes."
  [minutes]
  (cond
   (not (number? minutes))
   (error "I need a number of minutes to bake. You gave me a" (type minutes))

   (not= @location :prep-area)
   (error "I can only bake at the prep area. I am at the" @location)

   :else
    (do
      (println "Baking" minutes "minutes. . .")
      (dosync
       (alter pan-state update-in [:contents] bake minutes)
       (alter pan-state assoc :baking true))
      (println "Done!")
      (println "The result is" (name (:contents @pan-state)))
      :ok)))

(defn cool-pan
  "Remove the pan from the oven and put it in the cooling rack. Also grab a new pan to make something new! Returns the name of the cooling rack where the pan was placed."
  []
  (cond
   (not= @location :prep-area)
   (error "I can only cool the pan at the prep area. I am at the" @location)

   :else
   (let [r (gensym "cooling-rack-")]
     (dosync
      (alter cooling-rack assoc r (:contents @pan-state))
      (ref-set pan-state {}))
     (println "Cooling pan!")
     (println "I now have a fresh pan.")
     r)))

(defn status
  "Look around and report the status of the bakery."
  []
  (dosync

   (let [l (for [[k v] @loaded
                 :when (pos? v)]
             (if (_scoopables k)
               (if (= 1 v)
                 (str v " cup of " (name k))
                 (str v " cups of " (name k)))
               (if (= 1 v)
                 (str v " " (name k))
                 (str v " " (name k) "s"))))]
     (print "Loaded up: ")
     (if (seq l)
       (println (string/join ", " l))
       (println "nothing")))

   (println)

   (print "Location: ")
   (println (string/replace (name @location) "-" " "))

   (when (= @location :fridge)
     (print "Available ingredients: ")
     (if (seq _fridge-ingredients)
       (println (string/join ", " (map name _fridge-ingredients)))
       (println "none")))

   (when (= @location :pantry)
     (print "Available ingredients: ")
     (if (seq _pantry-ingredients)
       (println (string/join ", " (map name _pantry-ingredients)))
       (println "none")))

   (when (= @location :prep-area)
     (let [i (for [[k v] @prep-stock
                   :when (pos? v)]
               (if (_scoopables k)
                 (if (= 1 v)
                   (str v " cup of " (name k))
                   (str v " cups of " (name k)))
                 (if (= 1 v)
                   (str v " " (name k))
                   (str v " " (name k) "s"))))]
       (print "Available ingredients: ")
       (if (seq i)
         (println (string/join ", " i))
         (println "none")))

     (println)

     (print "In hand: ")
     (if @hand-state
       (println (string/replace (name @hand-state) "-" " "))
       (println "nothing"))

     (print "In bowl (")
     (if (:mixed @bowl-state)
       (print "mixed")
       (print "unmixed"))
     (print "): ")
     (if (seq @bowl-ingredients)
       (println (string/join ", " (for [[k v] @bowl-ingredients]
                                    (if (_scoopables k)
                                      (if (= 1 v)
                                        (str v " cup of " (name k))
                                        (str v " cups of " (name k)))
                                      (if (= 1 v)
                                        (str v " " (name k))
                                        (str v " " (name k) "s"))))))
       (println "nothing"))

     (print "In pan")
     (if (:baking @pan-state)
       (print " (in oven)")
       (print ""))
     (print ": ")
     (cond
      (nil? (:contents @pan-state))
      (println "nothing")

      (keyword? (:contents @pan-state))
      (println (name (:contents @pan-state)))

      :else
      (do
        (println (string/join ", " (for [[k v] (:contents @pan-state)]
                                     (if (_scoopables k)
                                       (str v " cups of " (name k))
                                       (str v " " (name k) "s")))))
        (println "nothing"))))
   (println)
   (print "On the cooling rack: ")
   (if (seq @cooling-rack)
     (println (string/join ", " (for [[k v] @cooling-rack]
                                  (str (name v) " in " (string/replace (name k) "-" " ")))))
     (println "nothing")))
  :ok)

(defn go-to
  "Go to the given location.
   Possible locations are :prep-area, :fridge, and :pantry."
  [place]
  (cond
   (not (_locations place))
   (error "I do not know where" place "is.")
   (= place @location)
   (do
     (println "I am already in" place)
     :ok)
   :else
   (dosync
    (println "Going to" place)
    (ref-set location place)
    :ok)))

(defn load-up
  "Load an ingredient from storage to take to the prep area."
  [ingredient]
  (cond
   (= :fridge @location)
   (if (_fridge-ingredients ingredient)
     (dosync
      (alter loaded update-in [ingredient] (fnil inc 0))
      :ok)
     (error ingredient "is not available at the fridge"))

   (= :pantry @location)
   (if (_pantry-ingredients ingredient)
     (dosync
      (alter loaded update-in [ingredient] (fnil inc 0))
      :ok)
     (error ingredient "is not available at the pantry"))

   :else
   (error "I can only load up at the fridge and pantry. I'm at the" @location)))

(defn unload
  "Unload an ingredient in the prep area."
  [ingredient]
  (cond
   (not= :prep-area @location)
   (error "I can only unload at the prep area")

   (zero? (@loaded ingredient 0))
   (error "I don't have any" ingredient "loaded")

   :else
   (dosync
    (alter prep-stock update-in [ingredient] (fnil inc 0))
    (alter loaded update-in [ingredient] dec)
    :ok)))

(def orderid (atom (rand-int 10000)))


(def streets ["Robot Ln", "Cyber Dr", "Electro Pkwy", "Servo St"])

(defn get-morning-orders
  "Get a new list of baking orders."
  []
  (doall
   (for [i (range 10)]
     {:orderid (+ i (swap! orderid inc))
      :address (str (rand-int 1000) " " (rand-nth streets))
      :items (into {} (for [x [:cake :cookies :brownies]
                            :let [n (rand-int 25)]
                            :when (pos? n)]
                        [x n]))})))

(defn delivery
  "Notify the delivery bot that something is ready to deliver"
  [receipt]
  (doall (:rackids receipt))
  :ok)

(defn bakery-help
  "Print out some helpful text to remember the available commands."
  []
  (println "Welcome to the bakery!")
  (println)
  (println "Available commands are: ")
  (println "grab \t\t   Pick something up.")
  (println "squeeze \t   Squeeze whatever you are holding.")
  (println "release \t   Release whatever you are holding.")
  (println "scoop \t\t   If you are holding the cup, fill it with an ingredient.")
  (println "add-to-bowl \t   Add the ingredient you are holding to the mixing bowl.")
  (println "mix \t\t   Mix the ingredients in the bowl.")
  (println "pour-into-pan \t   Pour the contents of the bowl into the pan.")
  (println "bake-pan \t   Put the pan in the oven for a certain number of minutes.")
  (println "cool-pan \t   After baking, put the pan on the cooling racks to cool.")
  (println "\t\t   Returns the id of the cooling rack.")
  (println "status \t\t   Print out the status of the bakery.")
  (println "start-over \t   If you get lost, this command will reset the bakery.")
  (println "go-to \t\t   Go to the given location.")
  (println "\t\t   Possible locations are :prep-area, :fridge, and :pantry.")
  (println "load-up \t   Load an ingredient from storage to take to the prep area.")
  (println "unload  \t   Unload an ingredient in the prep area.")
  (println "get-morning-orders Get a new list of baking orders.")
  (println "delivery \t   Notify the delivery bot that something is ready to deliver.")
  (println)
  (println "bakery-help \t Print out this message.")

  'welcome)

(bakery-help)
