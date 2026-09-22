# Read-only verification protocol

Routine small refreshes normally need only the deterministic wrapper. For a
nontrivial independent review, or when the user requests one, delegate a bounded
read-only verification subtask. Keep one publisher; the verifier never applies
content or edits sources. Do not delegate merely to run the same successful
checks twice.

Give the verifier only this reference, manifest path, run/receipt path, relevant
source root, and affected section/course/object IDs. Avoid full conversation
history, raw bodies, credentials, or student data. Use inherited model settings.

Suggested task:

> Verify the completed Canvas refresh using the supplied manifest and receipt
> paths. Run refresh-content.ps1 with -VerifyOnly against the same run directory.
> Inspect detailed artifacts only for failures or a missing check. Perform no
> Canvas writes, source edits, commits, publishing, or grading. Writing the wrapper's run-local verification.json evidence is allowed. Do not print
> credentials. Return PASS/FAIL/BLOCKED, object counts, failing IDs and reason,
> receipt path, API request count, and whether required visual playback evidence
> exists. Stop on an API/identity mismatch; do not try guessed endpoints.

The verifier must check that the receipt corresponds to current source,
configuration, renderer, and requested scope. Fresh verification checks content,
metadata-derived video IDs/counts, placements/groups, and publication states;
a prior successful receipt alone is insufficient. Limit failure output to the
information the publisher needs to repair the problem.

Video playback review is needed for new/changed embeds or renderer changes.
Unchanged embeds can reuse documented playback evidence during prose-only
refreshes, while deterministic link/embed validation still runs. If the browser
cannot authenticate, report that limitation and any accessible player evidence;
do not claim visual inspection of a Canvas page that was not opened.

Return results to the publisher; do not initiate a repair. The publisher owns
all corrections and decides whether new changes require another verification.
