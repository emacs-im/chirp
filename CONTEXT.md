# Chirp

Chirp presents one authenticated X account as navigable timelines, conversations, and detail views inside Emacs.

## Language

**Primary timeline**:
The single account-session browsing surface that presents the currently selected primary feed.
_Avoid_: Home timeline buffer, Following timeline buffer

**Primary feed**:
Either Home or Following, each with its own retained ordered post window, pagination edge, and browsing position.
_Avoid_: Tab cache, response cache

**Post collection**:
An ordered set of normalized posts presented by one query, including primary feeds, bookmarks, likes, lists, search results, and profile post views.
_Avoid_: Timeline cache, response list
