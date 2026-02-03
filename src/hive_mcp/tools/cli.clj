(ns hive-mcp.tools.cli
  "CLI-style subcommand dispatcher for consolidated tools"
  (:require [clojure.string :as str]))

(defn format-help
  "Format help text listing all available commands."
  [handlers]
  (str "Available commands:\n"
       (str/join "\n" (map #(str "  - " (name %)) (keys handlers)))))

(defn parse-command
  "Parse command string into keyword path.
   \"status list\" → [:status :list]
   \"spawn\" → [:spawn]"
  [command]
  (when (and command (not (str/blank? command)))
    (->> (str/split (str/trim command) #"\s+")
         (mapv keyword))))

(defn resolve-handler
  "Resolve handler from nested tree given command path.
   Returns {:handler fn :path-used [...] :remaining [...]}
   
   Supports:
   - Leaf handlers (fn)
   - Nested maps with :_handler for defaults
   - Partial matches falling back to :_handler"
  [handlers path]
  (loop [tree handlers
         used []
         remaining path]
    (cond
      ;; No more path - check for _handler or return tree
      (empty? remaining)
      (if-let [h (or (when (fn? tree) tree)
                     (get tree :_handler))]
        {:handler h :path-used used :remaining []}
        {:tree tree :path-used used})
      
      ;; Try next segment
      :else
      (let [seg (first remaining)
            next-node (get tree seg)]
        (cond
          ;; Leaf handler found
          (fn? next-node)
          {:handler next-node :path-used (conj used seg) :remaining (vec (rest remaining))}
          
          ;; Subtree found - recurse
          (map? next-node)
          (recur next-node (conj used seg) (rest remaining))
          
          ;; Not found - check for _handler fallback
          :else
          (if-let [default (get tree :_handler)]
            {:handler default :path-used used :remaining (vec remaining)}
            {:error :not-found :path-used used :remaining (vec remaining)}))))))

(defn make-cli-handler
  "Create a CLI-style handler that dispatches on :command param.

   handlers: map of keyword -> handler-fn
   Returns: fn that dispatches to appropriate handler"
  [handlers]
  (fn [{:keys [command] :as params}]
    (let [cmd (keyword command)]
      (if-let [handler (get handlers cmd)]
        (handler params)
        (if (= cmd :help)
          {:type "text" :text (format-help handlers)}
          {:isError true :text (str "Unknown command: " command
                                    ". Valid: " (keys handlers))})))))
