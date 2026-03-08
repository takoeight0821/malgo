---
mode: "agent"
tools: ["codebase"]
---

# Test-run & error-analysis assistant for our Haskell language-processor project

#codebase

1. **Setup**

   - Change directory to the project root that contains the Haskell sources.
   - Ensure the correct tool versions are loaded via `mise` (`mise use` if needed).

2. **Execute tests**

   - Run `mise run test` and capture _all_ stdout/stderr produced by the command.
   - Save the raw log so it can be quoted verbatim in the report.

3. **Parse the log**

   - Detect each failing test case (name, module, file : line) and its corresponding stack trace / error message.
   - Classify each failure by _type_ (compilation error, runtime exception, assertion failure, missing dependency, etc.).

4. **Map to source**

   - Using **only** `.hs` files from #codebase, locate the exact lines referenced in the errors.
   - For each location, quote the relevant snippet (±5 lines of context).

5. **Root-cause analysis**

   - Explain _why_ the failure occurs, referencing Haskell language rules, library behavior, or build setup as appropriate.
   - If multiple errors share a common cause, identify that pattern once.

6. **Proposed solutions**
   For every failure, provide:

   - **Fix idea** – concise description of the change (e.g. adjust type signature, import module, update test expectation).
   - **Patch sketch** – a minimal diff or code block showing the fix in situ.
   - **Confidence** – High / Medium / Low, based on certainty.

7. **Prioritized action plan**

   - Order the fixes by how many tests they unblock or by dependency chain.
   - Note any environment issues (e.g. outdated package index) that must be resolved first.

8. **Output format**
   Produce a Markdown report with these sections:

```

## Error summary

\| Test | Failure type | File\:Line |
...

## Detailed analysis & fixes

### <Test name or error group>

   <Quoted log>
   <Code snippet>
   <Root-cause explanation>
   <Proposed patch>
   ...

## Action plan

1. ...

```

9. **Constraints**

- Do **not** modify non-Haskell files.
- Assume GHC 9.12 and cabal-install 3.16; note if the fix depends on other versions.
- Keep the report under 500 lines; link to full log if larger.

10. **Finish**

- Return the Markdown report as the reply. Confirm that the test suite passes after applying all suggested fixes, or list any remaining failures.
