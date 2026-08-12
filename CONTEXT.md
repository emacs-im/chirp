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

## Publishing

**Compose draft**:
A not-yet-published post authored in Chirp, with text and optional reply or quote target and media attachments.
_Avoid_: Post (when still local), request, payload

**Reply target**:
The existing post that a compose draft replies to.
_Avoid_: Reply (when referring to the target)

**Reply audience**:
The post-level rule attached to a newly published post that determines which accounts may reply.
_Avoid_: Reply target, reply endpoint

**Media attachment**:
A media item included in a compose draft, with descriptive metadata that is independent of the post text and reply audience.
_Avoid_: Image (when the item may be a video or GIF)
