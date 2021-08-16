;;; Example of what the ENV.clj should contain
;;; Instructions:
;;;    - Copy this file to a local ENV.clj file
;;;    - Add ENV.clj to your .gitignore (see below)
;;;
;;; This ENV-MAP is used to expand {{*env*}} placeholders in my API helper functions
;;;
;;; IMPORTANT - The real ENV.clj file contains security secrets for accesssing Mambu environments
;;; DO NOT STORE this file in Github
;;; ADD to .gitignore file to make sure it never gets saved

(ns http.ENV-example)


(def ENV-MAP
  {
   "env1" {:url "https://europeshowcase.sandbox.mambu.com/api" :basic-auth ["fdrollover" "<password>"]}
   "env2" {:url "https://europeshowcase.sandbox.mambu.com/api" :ApiKey "<api-key>"} 
   })