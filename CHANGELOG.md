# Changelog

This file records user-visible Chirp changes from 2026-08-01 onward. Earlier
history remains available in Git.

## Unreleased

### Added

- Added `M-x chirp-open-url` and an opt-in `browse-url-handlers` regexp for opening supported HTTPS X and legacy Twitter links directly in their owning Chirp views. Tweet, list, profile, search, and account-collection URL parsing now share one trusted-host parser.

- Tweet timestamps now use localized X-style forms: list rows and replies show compact relative or calendar text at the live right edge, while a thread's focused tweet shows its exact time and date. The new `chirp-language` option defaults to Simplified Chinese.

- Edited posts now link to a read-only Appkit projection of X's complete edit history, grouped into the latest post and older versions with exact timestamps. Historical snapshots retain read navigation but omit every tweet mutation action.

- Added the optional Rust XChat module backed by pinned official `chat-xdk` and Juicebox SDK sources. It loads lazily, belongs to the Appkit session, performs one explicit PIN recovery call without Chirp-level retries, preserves remaining guesses and distinct failure states, cancels in-flight Tokio work, destroys opaque SDK state on `chirp-stop`, and returns only signature-verified plaintext rather than PIN or key material.
- `M-x chirp-login` captures an X browser session through browser-session, using Chirp's fixed persistent isolated profile root, validates only `auth_token` and `ct0`, and atomically writes Chirp's private provider auth file. `M-x chirp-forget-browser-session` deletes that file without signing out of the browser.
- Home, Following, search, bookmarks, notifications, user typeahead, accessible lists and their timelines, threads with full article expansion, core profile views, follower/following lists, translation, Likes, compose, image upload, and all current mutations now use authenticated direct X web API requests without `twitter-cli` or Python.
- Home and Following now share one Appkit-owned primary view identity while retaining independent session-owned canonical feed state, pagination, and semantic positions. Their keyed EWOC projection preserves multi-window positions, owns X reads, and invalidates media resources by dependent row; Appkit also owns Chirp's other session caches and bounded media, thumbnail, and link-card queues.
- `M-x chirp-direct-messages` explicitly unlocks XChat before opening an Appkit-owned read-only inbox with fresh conversation views and older-history pagination. Conversations automatically load required key history, project only signature-verified plaintext, and expose an Appkit trailing plain-text composer; reads never send acknowledgments.
- XChat plain-text sends now use the official XDK for native encryption and signing, submit one fixed GraphQL mutation without automatic retry or optimistic insertion, retain drafts until a validated acknowledgement, and report ambiguous transport outcomes as remotely unknown.
- Read-only GraphQL operations can refresh stale query IDs from the current bounded `TwitterInternalAPIDocument` API registry without replaying the failed request or affecting write IDs.
- Inline tweet translation from the actions menu with `T`.
- Native desktop notifications for account activity.
- User-handle completion while composing posts.
- Gapless row-sliced image and video/GIF cover thumbnails.
- Configurable case-insensitive filtering of reply content and author nicknames/handles, with conservative Chinese-first literal and all-fragment defaults collected from real public reply spam.
- Highlighted context labels for related tweets and reply-target handles in thread views.
- Inline `Show more` expansion for article previews while `RET` elsewhere on a tweet continues to open its detail thread.
- Persistent user spam phrases and keywords in a plain-text rule file, with `S`/`C-u S` capture from reply content or author names, one shared match scope, and a structured upstream submission form.
- Tweet reply, repost, like, quote, and bookmark metrics are Appkit actions. `RET` and `Mouse-1` run the same commands as the actions menu, including on a quoted card.
- Tweet timelines and threads now display X reply-audience controls and omit the Reply action when X explicitly limits the current account.
- Compose buffers now use Appkit's shared generated context, status-field, attachment, and editable-body boundary primitives.
- Post and quote drafts can set who may reply through the Appkit audience status field, and `CreateTweet` / `CreateNoteTweet` send that reply audience as `conversation_control`.
- A post draft can hold several ordered items on Appkit's multi-part compose surface. `C-c C-n` inserts another post after the current one, `C-c C-p` drops the current extra post, and send publishes the root then each following item as a reply.
- Compose status fields show the current item's X weighted length and switch to the long-form label above 280.
- Attached images can carry alt text through `C-c C-e` or the attachment row; Chirp writes it with X's media metadata API after upload.
- Compose can save an unsent draft on X with `C-c C-s` and schedule publication with `C-c C-t`. A multi-item draft is one `post_tweet_request` with `thread_tweets`; a later save edits the same draft ID.
- `x d` and `x t` open X drafts and scheduled posts in a `tabulated-list-mode` buffer. Buffer Menu marks flag rows, `x` deletes them, `RET` reopens text in compose, and `TAB` switches the two collections.
- Reopening an unsent post keeps its X `media_ids` and shows `media_entities` previews in compose. Sending that restored draft deletes the corresponding X draft or scheduled post after publish.
- Compose can attach one MP4 video through the same upload path as images. INIT uses `tweet_video`, large files are read in 1 MiB chunks, and a video cannot be mixed with other attachments.
- Compose shows Appkit submit progress while media uploads, including video `n/N` chunk status, and `C-c C-k` cancels that in-flight submit instead of refusing. Closing the compose buffer cancels the same view-owned upload.
- Tweet rendering uses Appkit action spans: RET or mouse-1 on an author, inline `@handle`, `#hashtag`, `$cashtag`, timestamp, retweeter name, link, `Show more`, media, or reply/repost/like/quote/bookmark metric runs that action. Inline text actions come from X's `entities` / `entity_set` code-point indices, the same set the web client walks in `tweetTextParts`. URL entities stay in the body as `display_url` instead of being pulled into a trailing list. Mentions and hashtags use the same link blue as the web. Retweets show the retweeter's display name.
- Evil bindings now cover timelines, threads, profiles, media, unsent lists, and compose. Normal state keeps `gg` and uses `g r` / `g j` / `g k` / `g n` instead of stealing `g`, `n`, and `p`.

### Breaking Changes

- `M-x chirp-thread` and `M-x chirp-list` now accept numeric IDs only. Use `M-x chirp-open-url` for supported X or Twitter URLs.
- Direct-message entry now requires an explicit absolute `chirp-xchat-native-module-file` and successful XChat unlock; Chirp does not infer or search for native build output.
- X account cookies are now read exclusively from Chirp's private auth file created by `M-x chirp-login`; `CHIRP_X_AUTH_TOKEN`, `CHIRP_X_CT0`, and `auth-source` entries are no longer used.
- Chirp now requires GNU Emacs 31.1 and Appkit 0.3.0 for semantic markup codecs, gapless inline image slices, action spans, compose lifecycle ownership, stable-key projections, and shared discussion and chat geometry.

### Fixed

- Browser-session login now tracks the returned request handle, rejects concurrent captures, and cancels an active capture when `chirp-forget-browser-session` runs.
- XChat media attachments now retain their verified `media_hash_key`, download ciphertext from X's cookie-authenticated TON media service, exclude url.el's header terminator from binary bodies, decrypt with the message's exact conversation-key version inside the native module, and render images and GIFs through Appkit semantic provider objects. Files open from Chirp's media cache, attached posts render as actionable Chirp tweet cards, and failed resources settle instead of remaining pending or retrying on redraw.

- X post permalinks opened through `chirp-open-url` now keep the linked post as the discussion focus, matching RET from a rendered post and keeping ancestor rows in the same order.

- Thread related/retweet attribution now uses Appkit's pre-heading context slot, preserving Chirp's context, author, and body order in discussion rows.

- Cached video and animated-GIF thumbnails now carry Appkit's play marker, so timeline previews remain visibly distinguishable from photos.
- Async image and video preview redraws no longer move point via `window-font-height`, which could scramble Home and thread rows after you switched away.
- Compose is now a chatbuf: committed posts render as draft rows, the trailing composer holds the current post, and undo no longer rewrites generated chrome.
- An empty post compose now shows only the header line and composer. Reply/quote context, media rows, and the Posts count appear when they have something to show.
- Opening a reply thread now shows the ancestor chain above the focus tweet with a prefix spine, then nests later replies from that focus instead of indenting the whole conversation from the root.
- Reply tweets no longer show the leading @handles X prepends for the conversation; visible text follows `display_text_range` when X sends it.
- Primary Home and Following rows no longer receive an extra projection separator between tweets.
- `chirp-stop` now cancels desktop notification polling with the Appkit session instead of allowing the next timer to recreate Chirp.
- Background image and link-card prefetches now enforce bounded protocols, redirects, time, and response sizes.
- Thread views now use Appkit discussion geometry with single-line tweet avatars, first-line timestamps, and stable parent/depth properties.
- Quoted tweets now render as Appkit card-prefixed normal tweet previews with author avatars, timestamps, reply context, media, and metrics.
- Acknowledged tweet deletion now removes the tweet from both retained primary feeds and updates the active projection without a merge refresh that could preserve the deleted row.
- Acknowledged XChat sends now bridge disjoint focused fragments through bounded older-history pages before merging one continuous conversation window; focused payloads may omit inbox-only deletion metadata, and inbox continuation may omit a false snapshot-restart flag.
- Verified XChat image attachments now use Appkit media resources and inline rendering when X provides a trusted URL, while verified reply previews and unsupported attachments no longer remain encrypted placeholders.
- XChat inbox entries now use Appkit's recent-session directory rows with participant avatars, previews, request/muted trails, and activity metrics. Inbox and conversation projections acquire only the peer, sender, and attachment resources used by their rendered rows, redraw only dependent rows, and use Appkit's shared one-line activity and two-line chat-avatar geometry.
- XChat message text now decodes and encodes through Appkit's plain semantic markup codec. The native bridge strictly decodes bounded verified JSON domain values, and only allowlisted X media URLs can enter the image resource pipeline.
- Fresh XChat conversation buffers now share one session-owned canonical conversation record. History, refresh, and verified decryption updates propagate to every matching conversation and inbox view while each buffer keeps its own history window, position, and draft.
- Website cards such as GitHub summaries now render from X's `card` payload instead of a later Open Graph fetch that GitHub pages exceed.
- Inline photos, video stills, compose previews, and website-card images use Appkit's two-step media API: cache `:height Nch` previews, then display through `appkit-media-insert-image-slices` or `appkit-media-image-slice-rows`. Tweet media grids no longer slice images locally.
- Avatars size to the current text line (`chirp-avatar-size` 28 is one line). `text-scale-mode` rebuilds avatars and card prefixes from cached data instead of leaving pixel chrome behind.
- Thread, profile, bookmarks, likes, lists, and search now use Appkit projections. Cached redraws request a view sync instead of erasing the buffer, so `text-scale-mode` and row identity survive.
- Tweet text decodes HTML entities with Emacs's `xml-substitute-special` while walking X's original indices, so "Scala & Java" displays correctly without a second index map.
- Videos carried only in X unified cards now expose their native thumbnail and bitrate variants for display and external playback.
- Photo media now prefers X's `media_url_https` over the tweet short link, and invalid HTML responses no longer become persistent image-cache entries.
- Structured X view metrics and detail-only bookmark counts now render their numeric values, while unavailable metric counts show only their icon rather than `-`.
- Home and Following now use the documented current HomeTimeline and HomeLatestTimeline operations and feature/field-toggle set, restoring public view counts when X returns them.
- Definitive stale read query IDs now refresh the public registry and retry once; writes and explicit query-ID overrides remain non-retrying.
- X GraphQL identities now prefer numeric `rest_id` values over opaque global IDs when routing profile and tweet requests.
- Direct GraphQL retweets now render the original author and content while preserving the retweeter context.
- Mention completion no longer moves point back to the `@` character.
- Tweet and media permalinks no longer appear as trailing links, while genuine external links highlight on hover.
- Publishing and media upload failures no longer trigger automatic mutation retries; structured X errors remain visible, while a response that could contain a partial mutation result is labeled as an unknown remote outcome.
- Binary multipart uploads and Unicode GraphQL bodies are encoded as unibyte HTTP data, transport setup errors redact session credentials, and unsafe control characters are rejected from authenticated headers.
- Ambiguous write failures remain visibly warned as an unknown remote outcome so drafts are not silently retried; authenticated POST retrievals disable `url.el` replay, write responses are bounded before copying or parsing, and errors or quits after dispatch cannot leave an unowned request.
- Recovered XChat keys are now bound to the registered X user for the native session, and outgoing Lisp input can no longer override the signing sender.
- Likes views now show every returned tweet with its like state active.
- Reply filtering now recognizes collected affiliate, dating, and drug-spam nickname templates, including the shared `返佣` marker and the combined `FoxLink` + `银狐` signature.
- `x r` and `chirp-reply-at-point` now refuse when X limits the current account, matching the hidden Reply metric.
- Compose keeps the draft until a send succeeds. Known failures leave it editable; unknown outcomes warn and require confirmation before sending again.
