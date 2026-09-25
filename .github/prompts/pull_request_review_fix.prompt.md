---
mode: 'agent'
description: 'Review pull request comments, apply fixes, and respond as needed in the Malgo project'
---
## Steps
Follow **Review** → **Implement**.

### Review
1. Fetch the review threads for pull request `${pr_number}` with their
   resolution state (`gh pr view --comments` does not show it):
   ```bash
   gh api graphql -F owner=takoeight0821 -F repo=malgo -F pr=${pr_number} -f query='
     query($owner:String!,$repo:String!,$pr:Int!){repository(owner:$owner,name:$repo){
       pullRequest(number:$pr){reviewThreads(first:100){nodes{isResolved path line
         comments(first:20){nodes{databaseId author{login} body}}}}}}}'
   ```
2. Skip threads with `isResolved: true`.
3. Prioritize comments from core maintainers if multiple comments target the same line.
4. For each comment:
   - If no change is needed, reply via:
     ```bash
     gh pr comment ${pr_number} --body "AI response: <explanation>"
     ```
     explaining why no change is required.
   - If a change is required, group related comments and add to a TODO list.

### Implement
5. Execute each TODO item in order:
   - Apply the code changes.
   - Run project checks:
     ```bash
     mise run build
     mise run test
     ```
   - Repeat until checks pass.
6. Stage and commit changes for each group:
   ```bash
   git add -u
   git add <new-file>   # for any new files
   git commit -m "<short description of changes>"
   ```
7. Push the updates to the current branch:
   ```bash
   git push
   ```

## Commands Reference
- Build & test: `mise run build` / `mise run test`
- Reply to comment: `gh pr comment`
