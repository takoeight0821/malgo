---
name: track
description: Task list management skill for tracking large-scale changes across multiple sessions. Use when the user says "create a task list", "plan changes", "follow todos/xxx.md", "continue from the track list", or "/track".
allowed-tools: Read, Write, Edit, Glob, Grep, Bash(git log:*), Bash(git rev-parse:*), Bash(date:*), Bash(ls:*), Bash(mkdir:*)
---

# Track - Persistent Task List Skill

Persist task lists in git-managed markdown files to track and execute
large-scale changes across multiple sessions.

## Track List Location

`todos/<description>.md` (under the project root).
Examples: `todos/refactor-flat-pass.md`, `todos/add-lsp-support.md`

## File Format

```markdown
# [Change Theme]

## Goal
[1-2 sentences describing the purpose of the change]

## Tasks

- [ ] Task 1
  - [ ] Subtask (if needed)
- [ ] Task 2
- [x] Completed task (2026-03-01, abc1234)
```

Completed tasks are annotated with the **date** (YYYY-MM-DD) and the **short commit hash**.
If there is no commit yet (recording before committing), use the date only.

---

## Flow 1: Create a New List

When the user says "create a task list" or "I want to plan changes":

1. **Interview**: Confirm the purpose, scope, and constraints of the change through dialogue with the user
2. **Codebase investigation**: Read related files with Grep/Glob/Read to identify necessary tasks
3. **Present draft**: Show the task list draft in the conversation and gather feedback
4. **Save file**: Once agreed upon, write to `todos/<description>.md`
   - If the `todos/` directory does not exist, create it with `mkdir -p todos`

## Flow 2: Implement from an Existing List

When the user says "follow todos/xxx.md" or "continue from the track list":

1. **Read the list**:
   - If a specific file is specified, open it with Read
   - If not specified, list available files with `Glob("todos/**/*.md")` and ask the user to choose
   - After reading the file, report the overall picture and the number of remaining tasks
2. **Implement**: Work through incomplete tasks (`- [ ]`) from top to bottom
3. **Update checkmarks**: After committing a task's implementation, get the commit hash and
   use Edit to update `- [ ] Task name` to `- [x] Task name (YYYY-MM-DD, abc1234)`
   - Date: obtained via `date +%Y-%m-%d`
   - Commit hash: obtained via `git rev-parse --short HEAD`
   - If recording before commit, use date only: `- [x] Task name (YYYY-MM-DD)`
4. **Repeat**: Move on to the next incomplete task
5. **Completion report**: Notify the user when all tasks are complete

## Important Rules

- Always use the Edit tool for task list changes to avoid breaking other parts of the file
- Work through tasks one at a time, updating checkmarks after each completion
- Break large tasks into subtasks before starting
- Skip tasks the user asks to skip and move to the next one
