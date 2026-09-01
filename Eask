;; -*- mode: eask; lexical-binding: t -*-

(package "chirp"
         "0.1.0"
         "Browse X timelines")

(website-url "https://github.com/LuciusChen/chirp")
(keywords "convenience" "comm")

(package-file "chirp.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "31.1")
(depends-on "appkit" :file "../appkit.el")
(depends-on "browser-session" :file "../browser-session")
(depends-on "plz" "0.8")
(depends-on "transient" "0.4.3")

(development
 (depends-on "evil"))

(script "refresh-local-deps"
        "eask -f install-file ../appkit.el && eask -f install-file ../browser-session")
(script "test" "eask test ert --quiet test/*.el")
(script "test-local"
        "eask -f install-file ../appkit.el && eask -f install-file ../browser-session && eask recompile && eask test ert --quiet test/*.el")
