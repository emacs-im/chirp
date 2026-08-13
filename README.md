# chirp.el

`chirp.el` is an Emacs browser for X/Twitter for people who want to read, search, inspect, and act on posts without leaving Emacs. Browsing views use dedicated Emacs buffers; read-only text views use Chirp's `special-mode` UI, image views use `image-mode`, and compose buffers remain editable. Chirp sends X web API requests directly through Emacs's `url.el`; Plz/curl is limited to the public, credential-free query-ID registry, while the separate `browser-session` helper is used only for explicit browser login capture.

## What you can do

Use Chirp to follow timelines, search and inspect posts, see X reply-audience restrictions, read direct messages, move from a post to its thread, author, profile lists, or media, and perform common post, reply, quote, like, bookmark, and follow actions without leaving Emacs. Optional desktop notifications surface account activity, and translation is available on demand.

Home, Following, search, bookmarks, notifications, user-handle completion, accessible lists and their timelines, threads with full article expansion, profile lookup, profile posts/replies/highlights/media, follower/following account lists, translation, the authenticated account's Likes view, compose, image upload, all current write actions, and XChat direct messages use direct X web APIs. `M-x chirp-direct-messages` requires one user-confirmed XChat unlock before opening the inbox; conversations then load the key history they need, project only signature-verified plaintext, and provide a plain-text composer backed by the official XDK. Direct-message reads never send read acknowledgments, and sends are never retried or inserted optimistically. Verified image attachments use Appkit's media cache and inline renderer when X provides a trusted media URL; unsupported attachments retain explicit summaries.

## Quick Start

Chirp requires GNU Emacs 29.1 or newer, Appkit 0.2.16 or newer, Plz 0.8 or newer with `curl`, Transient 0.4.3 or newer, and browser-session 0.1.0 or newer with its external helper. Direct X views and actions require an authenticated X web session.

### Sign in to X

Run `M-x chirp-login`. browser-session opens or attaches to a supported browser at `https://x.com`; sign in there if necessary. Chirp uses the fixed isolated root in `chirp-x-browser-session-profile-root`, which defaults to `(locate-user-emacs-file "chirp/browser-session/")`. It imports only the `auth_token` and `ct0` cookies into `chirp-x-auth-file`, which defaults to `(locate-user-emacs-file "chirp/auth.json")` and has Unix mode `0600`.

After a successful import, ordinary Chirp commands use the private auth file and do not need `chirp-login`. Run `M-x chirp-login` again only to refresh an expired session or switch accounts. Chirp never reads a browser cookie database, copied headers, `CHIRP_X_AUTH_TOKEN`, `CHIRP_X_CT0`, or `auth-source`. The generic capture file is private and deleted immediately after import; Chirp's provider auth file is the sole account-credential source. Run `M-x chirp-forget-browser-session` to delete it without signing out of the browser.

Configure the browser, proxy, or Firefox/Zen behavior through browser-session's public customization variables before running `chirp-login`. Customize `chirp-x-browser-session-profile-root` only to relocate Chirp's isolated profile; do not point it at a daily-use browser profile. Set `CHIRP_X_BEARER_TOKEN` only if X rotates its public web token. Chirp refreshes read-operation query IDs from the current `TwitterInternalAPIDocument` API registry after a definitive stale-query error; retry the failed read after the refresh, or run `M-x chirp-refresh-query-ids` explicitly. `chirp-x-query-id-overrides` remains authoritative, and write-operation IDs are never selected from the dynamic cache or retried.

### Load Chirp

```elisp
(add-to-list 'load-path "~/chirp")
(require 'chirp)
```

After a successful login, run `M-x chirp-home` to open the For You timeline in a dedicated buffer.

Direct messages require the optional source-built Rust module. Follow [`native/README.md`](native/README.md) to fetch the pinned official XDK/Juicebox sources, build it, and explicitly configure its absolute path with `chirp-xchat-native-module-file`. Chirp never searches for the module; other Chirp features remain usable when it is absent.

## Notifications

To receive account activity notifications, enable the global mode:

```elisp
(chirp-notifications-mode 1)
```

The first check establishes a baseline without showing old activity. Later
checks run every five minutes by default and notify only unseen activity. Linux
uses freedesktop notifications over D-Bus; macOS uses AppleScript. Titles and
bodies have Emacs text properties removed before reaching either backend.

To change the interval or the size of each recent-activity check:

```elisp
(setq chirp-notifications-interval 300
      chirp-notifications-max-results 20)
```

Tweet translation defaults to Chinese. Customize the target language with:

```elisp
(setq chirp-translation-language "zh")
```

## Entry Points

```elisp
M-x chirp-login
M-x chirp-forget-browser-session
M-x chirp-home
M-x chirp-following
M-x chirp-direct-messages
M-x chirp-bookmarks
M-x chirp-likes
M-x chirp-me
M-x chirp-list      ;; choose from your lists in the minibuffer
M-x chirp-search
M-x chirp-thread
M-x chirp-profile
M-x chirp-profile-followers
M-x chirp-profile-following-users
```

## Keys

`M-x chirp-home` and `M-x chirp-following` share one primary timeline buffer while retaining independent session-owned posts, pagination, and semantic positions. Reopening either subview, switching back with `TAB`, or recreating a killed buffer restores its canonical state without fetching again; use `g` when you want fresh data.

- `g`: refresh; on Home and Following, Chirp keeps the current timeline visible and merges newer posts at the top. Evil users use `g r` so `gg` stays beginning-of-buffer
- `TAB`: switch between Home and Following on those timelines; in profile buffers, cycle `Posts`, `Replies`, `Highlights`, `Media`, and `Likes` when available
- `n` / `p`: next or previous entry or direct-message event; on Home and Following, `n` on the last entry loads more older posts. Evil users use `g j` / `g k` instead, so `n` stays search-next
- `N`: load more older posts on Home and Following; in the direct-message inbox, load older conversations; in a conversation, load older message history. Evil users use `g n`
- `M-x chirp-dm-cancel-unlock`: cancel an active recovery; because a realm may already have processed the attempt, Chirp reports cancellation as an uncertain outcome and never retries automatically
- `q`: close the current Chirp window; on For You and Following, keep the timeline buffer alive so you can switch back later
- When Home or Following has no more older posts, Chirp says so instead of leaving the last loading message in place
- `RET` or `Mouse-1`: activate the Appkit action at point, or open the current tweet or profile. Actions include the author, an inline `@handle`, `#hashtag`, or `$cashtag` from X's tweet entities, a retweeter name, a link, `Show more`, media, reply/repost/like/quote/bookmark metrics, and profile summary controls. Opening a reply shows its ancestor chain above the focus tweet with a connecting prefix, then nests later replies from that focus. Leading @handles that X prepends to a reply are hidden, as on the web. A retweet shows `retweeted by` and the retweeter's display name.
- In an XChat conversation, `RET` on the timeline moves to the trailing composer; `RET` in the composer sends plain text, `C-u RET` inserts a newline, and `C-c C-c` explicitly sends
- In an XChat composer, `M-p` and `M-n` navigate sent-input history; Evil users enter Insert state normally with `i`. Compose buffers start in Insert state.
- In profile summaries, `RET` on `Followers` or `Following` opens that user list
- In profile buffers, `RET` on the subview strip switches between available profile timelines
- `m`: open the first media item for the current tweet
- `D`: download the current media, or choose one media item from the current tweet; photos try the original-resolution URL and videos use the highest-quality variant
- `A`: open the author profile
- `S`: add a persistent spam phrase or keyword using the active region or current tweet text; use `C-u S` to start with the author's display name
- `x`: open the actions menu for timeline switching, your own profile, bookmarks, liked tweets, lists, post/reply/quote, drafts, scheduled posts, follow/unfollow, translation, and tweet actions
- `x r`: reply to the tweet at point, or refuse when X limits replies from the current account
- `x T`: translate the tweet at point and show the result below its original text
- `o`: open the current item in a browser

Inside the compose buffer:

- `C-c C-a`: attach an image file (up to 4) or one MP4 video; video cannot be mixed with images
- `C-c C-v`: paste one image from the clipboard
- `C-c C-d`: remove an attached image from the current post
- `C-c C-e`: edit alt text for an attached image
- `C-c C-n`: commit the current composer text as a draft row and start the next post (new posts only)
- `C-c C-p`: drop the current extra draft row
- `RET` or `e` on a draft row: load that post back into the composer
- `M-TAB`: complete a user handle after typing an `@` prefix (also available
  to completion-at-point frontends such as Corfu)
- `C-c C-s`: save the draft on X as one unsent object; a later save updates that same draft
- `C-c C-t`: schedule the draft on X for a future local time and close the buffer after X acknowledges it
- `C-c C-c`: send the draft and keep it until X acknowledges success. A multi-post draft publishes the first item, then each later item as a reply to the previous one
- `C-c C-k`: cancel an in-flight send, save, or schedule, or close the draft when idle. Video uploads show `Uploading video n/N` progress in the status strip while chunks are sent.
- Click `Audience` on a post or quote to choose who can reply; replies inherit the conversation rule
- `x d` opens X drafts and `x t` opens scheduled posts. The list uses Emacs's tabulated list with Buffer Menu marks: `m` marks, `u` unmarks, `U` unmarks all, `d` flags deletion, `x` deletes flagged rows, `RET` reopens the item in compose, `TAB` switches drafts/scheduled, and `g` reloads
- The Appkit-backed compose surface is a chatbuf: committed posts are rendered draft rows, the trailing composer holds the current post, and the header line shows reply audience, weighted length, media count, in-flight submit progress, and the post count when a draft has more than one item. A new post starts as an empty composer. Reply and quote drafts show their target above the composer. Attached media appears only after a file is added. Emacs undo applies to the composer only. Click an image row or use `C-c C-e` to set alt text. A known send, save, or schedule failure leaves the draft editable. Canceling an in-flight submit also leaves the draft editable; if the remote outcome is unknown, Chirp warns and asks before repeating that submit. Reopening an unsent post restores its text parts and X media IDs, and shows image previews on the compose attachment rows. Publishing a restored draft or scheduled post deletes that unsent object after X acknowledges the new posts.

Premium accounts can send drafts over the standard 280 weighted-character limit. Chirp computes X's weighted length and selects the direct long-form operation for posts, replies, and quotes. JPEG, PNG, and WebP attachments may be up to 5 MiB; GIF attachments may be up to 15 MiB; MP4 videos may be up to 512 MiB and upload in 1 MiB chunks as `tweet_video`.

Tweet metrics also reflect the current local state: liked tweets show `Liked`, bookmarked tweets show `Saved`, and retweeted tweets show `RTed`. The visible reply, repost/retweet, like, and bookmark metrics also provide mouse-1 action controls.
Every tweet returned by the Likes view is shown with its like state active, even when an upstream payload omits the per-item `favorited` field.
Clipboard image paste uses `wl-paste` on Wayland and `pngpaste` on macOS when available.

## Thread Reply Filtering

Chirp hides likely spam replies using a conservative default list collected from repeated public reply spam, prioritizing Chinese templates before English ones. Matching configured literal phrases against reply text, expanded links, the author's display name, and the author's `@handle` ignores case and never filters the thread's focus tweet or its ancestor chain. A nested list requires every fragment to occur, which catches split templates without filtering on either broad fragment alone; set the option to nil to disable filtering.

Press `S` on a reply to edit and save a literal phrase, or select the useful portion first so it becomes the initial input. Use `C-u S` to start from the author's display name. Chirp stores accepted entries under `user-emacs-directory` (`~/.emacs.d/chirp/spam-rules.txt` in the usual setup), one UTF-8 literal per line, de-duplicates them without regard to case, and immediately refreshes the current view. Blank lines and lines beginning with `#` are ignored. Nicknames, handles, reply text, and expanded links all use this same combined rule set.

Run `M-x chirp-thread-edit-spam-rules` to edit the file directly, or customize `chirp-thread-spam-rules-file` to keep it in another location such as a dotfiles repository. After manual edits, refresh an open thread with `g`. Complex rules requiring every fragment to occur remain available through `chirp-thread-spam-keywords` and the source-controlled defaults.

To propose local rules for everyone, open the [spam rule submission form](https://github.com/LuciusChen/chirp/issues/new?template=spam-rule.yml), paste one or more entries from the local file, and include public examples or other evidence. The same submitted rule covers reply text and author identity, so it does not need separate nickname and content variants. Built-in additions are reviewed for false positives; maintainers can keep a specific literal rule or turn broad fragments into an all-fragment rule in [`lisp/chirp-spam-rules.el`](lisp/chirp-spam-rules.el).

```elisp
(setq chirp-thread-spam-keywords nil) ; Disable filtering entirely.

;; Or use Elisp for grouped or fully customized in-memory rules.
(setq chirp-thread-spam-keywords
      '("联系我领取"
        ("体制内幼师" "sao的很")
        "check my bio asappp"
        "t.me/"))
```

Refresh an open thread after changing the option.

X-provided related-tweet modules remain visible with a highlighted `Related tweet` context label and are not treated as replies by the keyword spam filter. Reply-target `@username` handles are highlighted separately from their muted `replying to` context, while standard quote tweets continue to render as nested `Quoted …` blocks.

## Appearance

Tweet lists use a lightweight separator between posts by default. To customize it:

```elisp
(setq chirp-tweet-separator "- - - - - - - - - - - -")
(setq chirp-tweet-separator-indent 6)
```

Set it to `nil` or an empty string to disable tweet separators.

Avatars size to one text line when `chirp-avatar-size` is 28, and both avatars and card prefixes rebuild after `text-scale-mode`. Avatars and tweet media thumbnails can be hidden independently:

```elisp
(setq chirp-show-avatars nil)
(setq chirp-show-tweet-media nil)
```

When tweet media thumbnails are hidden, Chirp keeps compact text media entries
so media commands still work, and shows alt text when X provides it.

## Media

- Chirp hides tweet permalinks and image/video resource links; genuine external links remain visible and highlight on hover.
- Images render as small thumbnails in timeline, thread, and profile post lists.
- Images and video/GIF cover thumbnails are split into gapless text-row slices, so point can move through a tall cover one row at a time. Multiple media items remain aligned in the same thumbnail grid. After `text-scale-mode`, those slices stay one current text row tall.
- Timeline, thread, and profile views now render cached avatars/thumbnails first; missing media are prefetched in the background so text appears faster.
- Video and animated GIF thumbnails are filled in asynchronously when Chirp can use an upstream preview image or extract one with `ffmpeg`.
- Press `RET` on a thumbnail to open the photo in a new Chirp media buffer when image display is available.
- In image and fallback media views, `q` closes the current media buffer and `D` downloads the current media item.
- Videos currently open externally through `mpv` when available, or the browser otherwise.

If you want a larger or fixed mpv window, customize:

```elisp
(setq chirp-video-player-window-size '(1280 . 720))
```

If you do not use `mpv`, either point `chirp-video-player-command` at another
player executable, or set it to `nil` to always open video URLs in the browser.

Downloaded media default to `~/Downloads/`.  To change that:

```elisp
(setq chirp-media-download-directory "~/Downloads/chirp/")
```

If you prefer the old blocking behavior, customize:

```elisp
(setq chirp-media-render-from-cache-only nil)
```

To disable background image prefetch, customize:

```elisp
(setq chirp-media-prefetch-images nil)
```

To trade freshness for faster repeated opens of the same thread/profile/article,
customize the short in-memory backend cache:

```elisp
(setq chirp-backend-read-cache-ttl 15)
```

## Notes

- Timelines, search, bookmarks, notifications, threads, core profile views, Likes, XChat direct messages, compose, and tweet mutations use X persisted web GraphQL operations through Emacs's built-in `url.el`. XChat message events are decoded from bounded Base64 Thrift documents; the configured official-XDK module retains key material, binds the recovered registered user as the session's non-overridable signing identity, returns only signature-verified domain data for reads, and prepares opaque encrypted/signed envelopes for one-shot plain-text sends.
- User typeahead, accessible-list discovery, relationship lists, translation, follow state changes, and multipart media upload use fixed, allowlisted X REST roots through the same transport.
- Appkit owns Chirp's lazy session lifecycle, Home/Following and direct-message view identity and canonical state, keyed timeline/directory reconciliation, exact chat history windows, persistent composer state, editable chat-mode boundary, position-preserving invalidation, and bounded media, thumbnail, and link-card queues. Media completion invalidates only timeline rows that depend on the completed resource; X protocol state, request semantics, cache paths, tweet rendering, and media geometry remain Chirp-owned.
- The parser is deliberately defensive because X's web payloads and persisted query IDs can drift. A bounded, unauthenticated registry refresh updates only GET operation IDs and retries one failed stale read with the refreshed ID; writes and explicit query-ID overrides are never replayed.
- Direct timeline pagination passes X's bottom cursor to the next GraphQL request without re-fetching the loaded prefix.
- Post, reply, quote, media INIT/APPEND/FINALIZE, and other write mutations are never retried automatically because a lost response can leave the remote outcome unknown. Authenticated POST retrievals disable `url.el` transport replay, mutation responses are bounded before copying or JSON parsing, and GraphQL errors or failures after dispatch preserve the unknown-outcome warning. Read-only media STATUS checks use bounded polling.
- Automated tests cover request shaping and response adaptation without sending live X writes. Write smoke tests require explicit opt-in, verify created artifacts, and delete created posts in reverse order.

## Development

Eask owns Chirp's package activation, local Appkit and browser-session dependencies, byte compilation, and ERT load paths:

```sh
eask run script test-local
```

## Opt-in write smoke

Live publishing checks are deliberately excluded from ERT. With disposable or otherwise approved account credentials configured, run `CHIRP_ALLOW_WRITE_SMOKE=1 emacs -Q --batch -L . -L lisp -l test/chirp-write-smoke.el --eval '(chirp-write-smoke-run)'`. The smoke creates a root post, reply, quote, long-form post, and image post, then deletes them in reverse order; ambiguous write failures are never retried.

## License

Chirp is available under the [MIT License](LICENSE).
