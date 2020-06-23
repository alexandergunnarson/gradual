(require
  'leiningen.deploy
  'leiningen.jar
  'leiningen.pom)

(def deps|clj-1-8-0
 '[[clojure-future-spec    "1.9.0"]
   [frankiesardo/linked    "1.2.9"]
   [org.clojure/clojure    "1.8.0"]
   [org.clojure/test.check "0.9.0"]])

(def deps|clj-1-9-0
 '[[frankiesardo/linked    "1.2.9"]
   [org.clojure/clojure    "1.9.0"]
   [org.clojure/spec.alpha "0.1.143"]
   [org.clojure/test.check "0.9.0"]])

(def deps|clj-1-10-0
 '[[frankiesardo/linked    "1.2.9"]
   [org.clojure/clojure    "1.10.0"]
   [org.clojure/spec.alpha "0.2.176"]
   [org.clojure/test.check "0.9.0"]])

(def deps|latest deps|clj-1-10-0)

(def project-name 'gradual/gradual)
(def version "0.1.3")

(defn >base-profile [profile-ident #_keyword?]
  (let [relativized-version
          (if (= profile-ident :current)
              version
              (str version "-" (name profile-ident)))]
    {:version      relativized-version
     :jar-name     (str "gradual" "-" relativized-version ".jar")
     :uberjar-name (str "gradual" "-" relativized-version "-uberjar.jar")
     :pom-name     (str "gradual" "-" relativized-version ".pom")}))

(def profiles
  (let [dev-test {:global-vars '{*warn-on-reflection* true
                                 *unchecked-math*     :warn-on-boxed}}]
    {:dev        (merge dev-test
                   {:dependencies '[[expound   "0.8.4"]
                                    [leiningen "2.9.3"]
                                    [orchestra "2019.02.06-1"]]})
     :dev-test   dev-test
     :test       (merge dev-test)
     :clj-1.8.0  (-> (>base-profile :clj-1.8.0)
                     (assoc :dependencies deps|clj-1-8-0))
     :clj-1.9.0  (-> (>base-profile :clj-1.9.0)
                     (assoc :dependencies deps|clj-1-9-0))
     :clj-1.10.0 (-> (>base-profile :clj-1.9.0)
                     (assoc :dependencies deps|clj-1-10-0))
     :current    (-> (>base-profile :current)
                     (assoc :dependencies deps|latest))}))

(def config
  {:name         project-name
   :version      version
   :description  "Gradual, dependent typing for Clojure."
   :url          "https://github.com/alexandergunnarson/gradual"
   :license      {:name         "Creative Commons Attribution-ShareAlike 3.0 US (CC-SA)"
                  :url          "https://creativecommons.org/licenses/by-sa/3.0/us/"
                  :distribution :repo}
   :dependencies (->> deps|latest
                      (remove #(-> % first (= 'org.clojure/spec.alpha)))
                      vec)
   :deploy-repositories
     {"clojars" {:url "https://repo.clojars.org" :username :gpg :password :gpg}}
   :profiles     profiles})

(def the-project
  (leiningen.core.project/make config
    project-name version (some-> (clojure.java.io/file *file*) (.getParent))))

;; ===== Commands ===== ;;

(defn >profiled-project [profile-ident]
  (-> the-project
      (merge (get profiles profile-ident))
      ;; To prevent e.g.:
      ;; `[clojure-future-spec "1.9.0-beta4"]` and
      ;; `[clojure-future-spec/clojure-future-spec "1.9.0-beta4" :scope "test"]`
      (dissoc :profiles)
      leiningen.core.project/init-project))

(defn write-jar! [profile-ident #_keyword?]
  (clojure.java.shell/sh "rm" "-rf" "./target")
  (leiningen.jar/jar (>profiled-project profile-ident)))

(defn write-pom! [profile-ident #_keyword?]
  (leiningen.pom/pom (>profiled-project profile-ident)
    (get-in profiles [profile-ident :pom-name])))

(defn deploy! [profile-ident #_keyword?]
  (write-jar! profile-ident)
  (write-pom! profile-ident)
  (leiningen.deploy/deploy
    (>profiled-project profile-ident)
    "clojars"
    (str project-name)
    (get-in profiles [profile-ident :version])
    (str "./target/" (get-in profiles [profile-ident :jar-name]))
    (str "./"        (get-in profiles [profile-ident :pom-name]))))

;; A more flexible `:aliases`
;; E.g. LEIN_COMMAND="deploy-all" lein
(when-let [command (System/getenv "LEIN_COMMAND")]
  (case command
    "deploy-all" (do (deploy! :clj-1.8.0)
                     (deploy! :clj-1.9.0)
                     (deploy! :clj-1.10.0)
                     (deploy! :current))
    (throw (ex-info "Unhandled command" {:command command}))))

(def project the-project)
