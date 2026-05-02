#!/usr/bin/env python3
"""Generate a dependency graph from cabal's plan.json.

Usage:
    cabal build all --dry-run   # generates dist-newstyle/cache/plan.json
    python3 scripts/dep-graph.py [--dot | --svg | --summary]

Options:
    --dot       Output DOT format to stdout (default)
    --svg       Output SVG file to deps.svg (requires graphviz/dot)
    --summary   Print dependency summary per direct dependency
"""

import json
import subprocess
import sys
from pathlib import Path

PLAN_JSON = Path("dist-newstyle/cache/plan.json")

# GHC boot libraries and other "free" packages (shipped with GHC)
BOOT_LIBS = frozenset({
    "base", "ghc-prim", "ghc-internal", "ghc-bignum", "rts",
    "ghc-boot-th", "ghc-boot", "ghc", "ghci", "ghc-heap",
    "ghc-platform", "template-haskell", "array", "deepseq",
    "process", "time", "unix", "stm", "binary", "bytestring",
    "containers", "directory", "filepath", "pretty", "text",
    "transformers", "mtl", "parsec", "Cabal-syntax", "Cabal",
})

PROJECT_PKGS = frozenset({"malgo", "malgo-lsp"})


def load_plan():
    with open(PLAN_JSON) as f:
        return json.load(f)


def build_graph(plan):
    """Build {uid -> pkg_name} and {pkg_name -> set(dep_names)} maps."""
    id_to_name = {}
    for pkg in plan.get("install-plan", []):
        id_to_name[pkg.get("id", "")] = pkg.get("pkg-name", "")

    deps_map = {}
    for pkg in plan.get("install-plan", []):
        name = pkg.get("pkg-name", "")
        if name in BOOT_LIBS:
            continue
        dep_names = {id_to_name.get(d, d) for d in pkg.get("depends", [])}
        dep_names -= BOOT_LIBS
        dep_names.discard(name)
        existing = deps_map.get(name, set())
        existing.update(dep_names)
        deps_map[name] = existing

    return deps_map


def transitive_deps(deps_map, pkg, visited=None):
    """Compute transitive closure of dependencies for a package."""
    if visited is None:
        visited = set()
    if pkg in visited or pkg in BOOT_LIBS:
        return visited
    visited.add(pkg)
    for dep in deps_map.get(pkg, set()):
        transitive_deps(deps_map, dep, visited)
    return visited


def generate_dot(deps_map):
    lines = [
        "digraph deps {",
        "  rankdir=LR;",
        '  node [shape=box, fontsize=9];',
    ]
    for pkg in PROJECT_PKGS:
        if pkg in deps_map:
            color = "lightblue" if pkg == "malgo" else "lightyellow"
            lines.append(f'  "{pkg}" [style=filled, fillcolor={color}];')

    for src, dsts in sorted(deps_map.items()):
        for dst in sorted(dsts):
            lines.append(f'  "{src}" -> "{dst}";')

    lines.append("}")
    return "\n".join(lines)


def print_summary(deps_map):
    print("=== Dependency Summary ===\n")

    for pkg in sorted(PROJECT_PKGS):
        if pkg not in deps_map:
            continue
        direct = sorted(deps_map.get(pkg, set()))
        all_trans = transitive_deps(deps_map, pkg)
        all_trans.discard(pkg)

        print(f"{pkg}: {len(direct)} direct, {len(all_trans)} total transitive\n")
        print(f"  Direct dependencies:")
        for dep in direct:
            dep_trans = transitive_deps(deps_map, dep)
            dep_trans.discard(dep)
            only_via = dep_trans - {d for d2 in direct if d2 != dep for d in transitive_deps(deps_map, d2)}
            unique_str = f" (unique: {len(only_via)})" if only_via else ""
            print(f"    {dep:30s} +{len(dep_trans):3d} transitive{unique_str}")
        print()


def main():
    if not PLAN_JSON.exists():
        print(f"Error: {PLAN_JSON} not found. Run 'cabal build all --dry-run' first.", file=sys.stderr)
        sys.exit(1)

    plan = load_plan()
    deps_map = build_graph(plan)
    mode = sys.argv[1] if len(sys.argv) > 1 else "--dot"

    if mode == "--summary":
        print_summary(deps_map)
    elif mode == "--svg":
        dot_content = generate_dot(deps_map)
        result = subprocess.run(
            ["dot", "-Tsvg", "-o", "deps.svg"],
            input=dot_content, text=True, capture_output=True,
        )
        if result.returncode != 0:
            print(f"Error running dot: {result.stderr}", file=sys.stderr)
            sys.exit(1)
        print("Generated deps.svg")
    else:
        print(generate_dot(deps_map))


if __name__ == "__main__":
    main()
