(ns hive-mcp.agent.sdk.execution
  "SDK query execution via persistent ClaudeSDKClient (multi-turn P3-T2).

   Executes SAA phases by submitting query() + receive_response() coroutines
   to the session's persistent asyncio event loop. Each dispatch reuses the
   same connected client for multi-turn conversation continuity."
  (:require [clojure.core.async :as async :refer [chan >!! close!]]
            [hive-mcp.agent.sdk.python :as py]
            [hive-mcp.agent.sdk.session :as session]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn execute-phase!
  "Execute a single SAA phase via the persistent ClaudeSDKClient (P3-T2).

   Multi-turn architecture:
   - Uses the persistent event loop + client created at spawn time
   - Submits query() + receive_response() as a coroutine to the existing loop
   - No loop/client creation or teardown per phase

   Per-ling Python globals (set at spawn, reused here):
   - _hive_client_<safe_id>  -- Connected ClaudeSDKClient
   - _hive_loop_<safe_id>    -- Persistent asyncio event loop

   Per-dispatch temp globals (cleaned up after each call):
   - _hive_results_<safe_id> -- Phase results
   - _hive_session_<safe_id> -- Session ID from SDK messages

   Arguments:
     ling-id - Ling identifier
     prompt  - Task prompt for this phase/turn
     phase   - SAA phase keyword (:silence :abstract :act) or :dispatch for raw turns

   Returns:
     core.async channel that will receive messages from the phase.
     Channel closes when phase completes."
  [ling-id prompt phase]
  (let [out-ch (chan 1024)
        sess (session/get-session ling-id)
        safe-id (session/ling-id->safe-id ling-id)
        client-var (or (:client-ref sess)
                       (str "_hive_client_" safe-id))
        loop-var (or (:py-loop-var sess)
                     (str "_hive_loop_" safe-id))
        results-var (str "_hive_results_" safe-id)
        session-var (str "_hive_session_" safe-id)
        turn-count (inc (or (:turn-count sess) 0))]
    (log/info "[sdk.execution] Starting phase" {:ling-id ling-id
                                                :phase phase
                                                :turn turn-count
                                                :prompt (subs prompt 0 (min 100 (count prompt)))})
    ;; Update session phase tracking
    (session/update-session! ling-id {:phase phase
                                      :phase-started-at (System/currentTimeMillis)
                                      :turn-count turn-count})
    ;; Execute query in background thread
    (async/thread
      (try
        (py/py-run (str "import asyncio\n"
                        "\n"
                        "async def _hive_query_" safe-id "_t" turn-count "():\n"
                        "    client = globals().get('" client-var "')\n"
                        "    if client is None:\n"
                        "        raise RuntimeError('No persistent client for " safe-id "')\n"
                        "    results = []\n"
                        "    session_id = None\n"
                        "    await client.query(" (pr-str prompt) ")\n"
                        "    async for msg in client.receive_response():\n"
                        "        results.append(str(msg))\n"
                        "        if hasattr(msg, 'session_id'):\n"
                        "            session_id = msg.session_id\n"
                        "    return results, session_id\n"
                        "\n"
                        "_hive_query_future_" safe-id " = asyncio.run_coroutine_threadsafe(\n"
                        "    _hive_query_" safe-id "_t" turn-count "(),\n"
                        "    " loop-var "\n"
                        ")\n"
                        results-var ", " session-var " = _hive_query_future_" safe-id ".result()\n"))
        ;; Extract results
        (let [results (py/py->clj (py/py-get-global results-var))
              session-id (py/py->clj (py/py-get-global session-var))]
          ;; Store session ID for continuity
          (when session-id
            (session/update-session! ling-id {:session-id session-id}))
          ;; Put results on channel
          (doseq [msg (if (sequential? results) results [results])]
            (>!! out-ch {:type :message :phase phase :data msg})))
        (catch Exception e
          (log/error "[sdk.execution] Phase execution failed"
                     {:ling-id ling-id :phase phase :turn turn-count
                      :error (ex-message e)})
          (>!! out-ch {:type :error :phase phase :error (ex-message e)}))
        (finally
          ;; Clean up per-dispatch temp globals (NOT the persistent client/loop)
          (try
            (py/py-run (str "globals().pop('" results-var "', None)\n"
                            "globals().pop('" session-var "', None)\n"
                            "globals().pop('_hive_query_future_" safe-id "', None)\n"))
            (catch Exception _ nil))
          (close! out-ch)
          (session/update-session! ling-id {:phase-ended-at (System/currentTimeMillis)}))))
    out-ch))
