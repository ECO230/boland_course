# Codex Project Context

This file is safe to commit to the public repository. Use it for project context that helps future development without exposing private operational details.

## Project Shape

- This repository contains course materials, Quarto content, shared assets, infrastructure notes, and Shiny applications.
- Keep public course materials clear enough for students and collaborators to understand without needing private deployment details.
- Prefer small, focused changes that preserve the existing folder organization by week, app, or shared concern.

## Design Philosophy

- Course-facing content should be readable, reproducible, and easy to update.
- Shiny apps should prioritize direct student use over decorative complexity.
- Infrastructure and deployment documentation should explain the general approach publicly while keeping exact server details private.
- When adding new documentation, separate reusable public decisions from local operator notes.

## Public vs Private Context

Public context belongs in committed files such as this one, `README.md`, `docs/`, `infra/`, or app-specific documentation when it does not reveal sensitive details.

Private context belongs in `.codex/private.md`, which is intentionally ignored by Git. Use it for:

- server hostnames, IP addresses, and admin URLs
- usernames, access paths, deployment commands tied to private hosts
- credentials, tokens, secret names, and private environment values
- internal-only operational notes
- exact infrastructure topology that should not be public

Do not copy private details from `.codex/private.md` into public docs, code comments, issues, commits, or generated course materials.

## Working Notes For Codex

- If `.codex/private.md` exists locally, read it only when server, deployment, or private operational context is relevant.
- Treat `.codex/private.example.md` as the committed template for private notes.
- Keep generated docs plain Markdown unless a specific Quarto feature is needed.
