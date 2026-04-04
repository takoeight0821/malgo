---
name: design
description: "Create a design document before implementation. Use when the user says /design, 'planして', '設計して', '計画を立てて', 'アーキテクチャを考えて', '設計文書を書いて', 'design this', or when a complex task clearly needs architectural decisions before coding. Always use this skill when the user requests planning or design work, even if they don't explicitly say 'design'. After writing the document, STOP — never proceed to implementation."
---

# Design

Create a design document that captures architectural decisions and implementation plans before writing any code. The document serves as a blueprint for agent teams to execute in parallel.

**Critical rule**: After writing the design document, STOP. Do not begin implementation until the user explicitly asks for it.

## Arguments

`/design <topic description>` — If omitted, infer the topic from conversation context.

## Workflow

### 1. Investigate the codebase

Understand the current state of the code relevant to the design topic.

- Use Explore agents to survey related files, modules, types, and data flow
- Identify existing architecture, dependencies, and constraints
- Launch multiple Explore agents in parallel when investigating independent areas
- If external research is needed (best practices, prior art), use WebSearch/WebFetch

### 2. Create the design document

Write to `docs/plans/YYYY-MM-DD-<description>.md`.

- `YYYY-MM-DD` is today's date
- `<description>` is a kebab-case summary of the topic (e.g., `query-based-compilation`, `lsp-server`)
- Write the document in the user's language
- Create the `docs/plans/` directory if it doesn't exist

### 3. Document structure

Follow this structure. Add extra sections as needed.

```markdown
# <Title>

Date: YYYY-MM-DD

## Context

Background and motivation for this design. Describe the current problems, goals, and constraints.
Include findings from codebase investigation — current architecture, relevant modules, data flow.

## Design Choices

Key design decisions and their rationale. List alternatives considered, why each was chosen or rejected, and the trade-offs involved.

## Implementation Plan

Break the work into tasks that agent teams can execute in parallel. Each task should be independently assignable to a teammate with its own worktree.

### Task 1: <task name>

- **Goal**: What this task achieves
- **Scope**: Files and modules to change
- **Dependencies**: Other tasks that must complete before this one (or "none")
- **Steps**: Concrete implementation steps with enough detail for an agent to execute
- **Verification**: How to confirm the task is complete

### Task 2: <task name>

...

## Verification

How to verify everything works after all tasks are merged. Test commands, expected results, integration checks.

## Risks

| Risk | Mitigation |
|------|------------|
| ... | ... |
```

### 4. Stop

Tell the user the path of the created design document. Do not ask "shall I start implementing?" — just stop and wait.
