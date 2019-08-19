(ns quiver.core)
(require '[clojure.string :as str])

(def open-url (js/require "openurl"))
(def fs (js/require "fs"))
(def base-path "/Users/SIS/Dropbox/quiver/Quiver.qvlibrary")
(def exclude-list ["Trash.qvnotebook" "Tutorial.qvnotebook" "meta.json" "Inbox.qvnotebook"])
(def match-list
  [(fn [st] (re-seq #"\*\*\* (TODO) (.*)" st))
   (fn [st] (re-seq #"\*\*\* (DONE) (.*)" st))
   (fn [st] (re-seq #"\*\*\* (WAITING) (.*)" st))])

(defn node-slurp [path]
    (.readFileSync fs path "utf8"))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn str->json->clj [str]
  (js->clj (.parse js/JSON str) :keywordize-keys true))

(defn list-files [path]
  (filter #(not (in? exclude-list %1)) (fs.readdirSync path)))

(defn full-path [path coll]
  (map #(str path "/" %1) coll))

(defn list-full-path-files [path]
  (full-path path (list-files path)))

(def notebooks (list-full-path-files base-path))
(defn notes [] (mapcat #(list-full-path-files %1) notebooks))

(defn getContent [note]
  (str->json->clj (node-slurp (str note "/content.json"))))

(defn match-category [re cell]
  (if-let [matches (re cell)]
    (map #(assoc {}
                 :value (%1 2)
                 :category (%1 1))
         matches)
    nil))

(defn match-org? [string]
  (flatten (keep #(when (match-category %1 string) (match-category %1 string)) match-list)))

(defn org-cell? [cell]
  (match-org? (:data cell)))

(defn org-note? [note]
  (let [cells (:cells note)
        actions (flatten (keep #(when (org-cell? %1) (org-cell? %1)) cells))]
    (if (empty? actions)
    false
    actions)))

(defn grep-notes [notes]
  (reduce
   (fn [acc note]
     (if (org-note? (getContent note))
       (conj acc
             (assoc {}
                    :note note
                    :actions (org-note? (getContent note))))
       acc))
   []
   notes))

(defn note-id [st]
  (let [matches (re-seq #"([A-Z0-9\-]*).qvnote$" st)]
  (if matches
    (str "(quiver-note-url/" ((first matches) 1) ")")
    false)))

(defn cat-value->action [cat-value]
  (str "[*** " (:category cat-value) " - " (:value cat-value) "]"))

(defn grepped-notes->actions [gnotes]
  (let [note-name (note-id (:note gnotes)) actions (:actions gnotes)]
    (str/join "\n" (map #(str (cat-value->action %1) note-name) actions))))

; (println (grep-notes (notes)))
(def actions (str (first (map grepped-notes->actions (grep-notes (notes))))))
(println actions)
; (.writeFileSync fs "./today.json"
;                 (.stringify js/JSON
;                             (clj->js
;                              (assoc {}
;                                     :title "Today"
;                                     :cells (into []
;                                                  (map grepped-notes->actions (grep-notes (notes))))))))

(.open open-url (str "quiver://actions/create-note?title=8-19-2019&type=markdown&content=" actions))
