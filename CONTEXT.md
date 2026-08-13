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
A not-yet-published post authored in Chirp, with one or more ordered items, optional reply or quote target, reply audience, and media attachments. One draft may contain several items; publishing turns each item into its own post.
_Avoid_: Post (when still local), request, payload

**Unsent draft**:
An X-server draft created from a compose draft. One object stores the root item and any following items as `thread_tweets`.
_Avoid_: Compose draft (when referring to the server object)

**Scheduled post**:
An X-server post that will publish at `execute_at`. One object stores the same root-plus-`thread_tweets` shape as an unsent draft.
_Avoid_: Local timer, Emacs timer

**Reply target**:
The existing post that a compose draft replies to.
_Avoid_: Reply (when referring to the target)

**Reply audience**:
The post-level rule attached to a newly published post that determines which accounts may reply.
_Avoid_: Reply target, reply endpoint

**Compose submit**:
The current send, save, or schedule attempt on a compose draft, including optional upload progress and a cancel hook owned by the compose view.
_Avoid_: File upload job, HTTP request

**Media attachment**:
A media item included in a compose draft, with descriptive metadata that is independent of the post text and reply audience.
_Avoid_: Image (when the item may be a video or GIF)

## Conversations

**Focus tweet**:
The post a thread view was opened on.
_Avoid_: Selected tweet, current tweet, root (when the opened post is a reply)

**Ancestor chain**:
The linear parents from the conversation root down to the focus tweet, shown above that focus.
_Avoid_: Reply tree, nested thread, indent stack

**Reply tree**:
The replies that come after the focus tweet, nested by their parent below that focus.
_Avoid_: Ancestor chain, conversation root depth

**Hidden reply mentions**:
The leading @handles X prepends to a reply for the conversation participants. They are not part of the visible post text.
_Avoid_: Reply target, reply audience

**Action span**:
A rendered region that carries one Appkit action, such as a profile, hashtag search, link, media open, or tweet metric. RET and mouse-1 activate that action; mode keys stay on the view map.
_Avoid_: Key cheat sheet, open-at-point dispatcher
