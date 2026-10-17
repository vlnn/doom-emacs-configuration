# Agent skills

pi loads every skill under this directory once the repo's trust review has
allowed it. Task worktrees carry their own copy, so a worktree's checkout is
what pi reads there, never the main checkout's.

This directory is an agent guard (`+tasks-guarded-files`, `+ai-trust`,
`agent.sb`): the jail keeps agents from writing here, the trust review shows
changes to it before pi starts, and `SPC 5 d` asks before merging a branch
that touches it. Read every change to a skill as a magit diff before
accepting it.

## grilling

The interview `SPC 5 s` starts: the agent maps the decisions a task forces,
looks facts up itself, asks you only what you alone can settle, and writes
`PLAN.md` once nothing is left open.

Upstream: [mattpocock/skills](https://github.com/mattpocock/skills), commit
`49dd158`, `skills/productivity/grilling/SKILL.md`; `skills-lock.json` holds
the content hash the installer computed. Installed with

```sh
npx skills@latest add mattpocock/skills -a pi -s grilling --copy
```

`--copy` matters: a symlink into npx's cache would put the file the trust
review reads outside the repo. Updating re-runs the same line; the resulting
diff is the review.

The upstream text says to dispatch a sub-agent for facts. Plain pi has no
sub-agents; `AGENTS.md` tells it to look things up itself with read, grep and
bash. `grill-me`, the companion skill, is a Claude Code primitive and is not
shipped.
