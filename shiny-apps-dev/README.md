# Shiny Apps Dev Workflow

This folder is the development source of truth for course Shiny apps.

## Development vs Published Apps

There are two important paths on the server:

### Development source

```text
/data/junior/boland_course/shiny-apps-dev
```

This repo folder is where app code should be edited, committed, and versioned.

### Published Shiny Server path

```text
/data/junior/shiny
```

Shiny Server serves app URLs from this path.

## Recommended Publish Pattern

Do not copy app folders into `/data/junior/shiny` as a second source of truth.

Instead:

1. keep the real app files in the repo under `shiny-apps-dev`
2. publish with a symlink from `/data/junior/shiny/...` to the repo app folder

Example for the Week 12 sampling app:

```text
/data/junior/boland_course/shiny-apps-dev/week12/samplinglab
```

published as:

```text
/data/junior/shiny/week12/samplinglab -> /data/junior/boland_course/shiny-apps-dev/week12/samplinglab
```

## Publish Steps For A New Dev App

From the server:

1. Pull the latest repo changes into:

```text
/data/junior/boland_course
```

2. Create the target week folder if needed:

```bash
mkdir -p /data/junior/shiny/week12
```

3. Create or refresh the symlink:

```bash
rm -rf /data/junior/shiny/week12/samplinglab
ln -s /data/junior/boland_course/shiny-apps-dev/week12/samplinglab /data/junior/shiny/week12/samplinglab
```

4. Open the published URL.

## Important Symlink Rule

The symlink belongs in `/data/junior/shiny/...`, not inside the app folder itself.

Good:

```text
/data/junior/shiny/week12/samplinglab -> /data/junior/boland_course/shiny-apps-dev/week12/samplinglab
```

Bad:

```text
/data/junior/boland_course/shiny-apps-dev/week12/samplinglab/samplinglab -> /data/junior/boland_course/shiny-apps-dev/week12/samplinglab
```

That bad pattern creates a recursive self-reference like:

```text
samplinglab/samplinglab/samplinglab/...
```

## Useful Checks

See where the published app really points:

```bash
readlink -f /data/junior/shiny/week12/samplinglab
```

List the repo app folder:

```bash
ls -la /data/junior/boland_course/shiny-apps-dev/week12/samplinglab
```

The repo app folder should contain real app files such as:

- `app.R`
- supporting `.md` files
- optional app assets

It should not contain another self-referential `samplinglab` symlink.

## Updating A Published App

1. commit and push changes from the development repo
2. pull them on the server repo
3. confirm the repo-side app file contains the new code
4. because the published path is a symlink, the app should immediately reflect the new repo copy
5. if Shiny still serves an old error state, refresh or restart the shiny container

## If The Browser Shows A Generic Startup Error

Check logs on the server:

```bash
docker logs shiny --tail 200
```

or:

```bash
tail -n 200 /var/log/shiny-server/*.log
```

Then verify the published path and repo path are pointing at the same app code before debugging the app itself.
