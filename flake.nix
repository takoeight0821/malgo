{
  description = "Malgo: a statically typed functional language (Lean 4 + Zig backend)";

  inputs = {
    # Not `nixpkgs.follows = "lean4-nix/nixpkgs"`: lean4-nix's own pin
    # predates `zig_0_16` (nixpkgs added it after lean4-nix's pinned
    # revision), and that attribute name is a hard requirement, not a
    # preference -- Zig minor releases break std (see mise.toml), so
    # silently falling back to zig_0_15 would be the wrong kind of quiet.
    # The Lean source-build overlay below only needs long-stable nixpkgs
    # packages (cmake, gmp, git, ...), not anything as version-sensitive as
    # `zig_0_16`, so applying it to an independently-pinned nixpkgs is safe.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    lean4-nix.url = "github:lenianiva/lean4-nix";
  };

  outputs =
    { self, nixpkgs, lean4-nix }:
    let
      # aarch64-darwin is where this flake was built and verified (PR #421).
      # x86_64-linux/aarch64-linux were added because nix-config's own CI
      # (Ubuntu, x86_64-linux) evaluates `inputs.malgo.packages.${pkgs.system}`
      # as part of building its own config, which forced this flake to
      # actually have a package for that system, not merely be evaluable on
      # it. Not x86_64-darwin: nothing has asked for it yet, and there's no
      # machine here to verify it against -- add it the same way, verified
      # the same way, if that ever changes.
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Everything below was `system`-free before -- a single `let` binding
      # computed once for the hardcoded aarch64-darwin. Wrapped in a function
      # over `system` instead of switching to flake-utils: `genAttrs` above
      # is already reachable through the existing `nixpkgs` input, so a
      # second flake input for one helper function wasn't justified.
      mkMalgo =
        system:
        let
          # `binary = false` builds Lean from source (lean4-nix's own CMake
          # bootstrap) instead of the default `binary = true`, which fetches
          # a pre-built binary release. On aarch64-darwin specifically, that
          # pre-built release's `libInit_shared.dylib` has no Mach-O header
          # slack for `install_name_tool` to rewrite its rpath -- every
          # workaround tried for that failed to make the binary loadable; a
          # from-source build gets correct rpaths as a normal side effect of
          # compilation instead. Full history of what was tried: PR #421.
          # Using the same from-source path uniformly across all systems
          # here, not just aarch64-darwin where it was originally forced:
          # lean4-nix's own CI (github.com/lenianiva/lean4-nix's flake.nix)
          # tests this exact `readToolchainFile { binary = false; }` overlay
          # on aarch64-darwin, aarch64-linux, x86_64-darwin, and x86_64-linux
          # alike, so it isn't a Darwin-specific workaround being applied
          # somewhere it doesn't belong.
          #
          # This means no `cache.nixos.org` substitute (nixpkgs' own `lean4`
          # package is pinned to 4.30.0, not this project's 4.32.0), so the
          # first build on any given system is a genuinely slow, full local
          # Lean bootstrap -- several thousand tiny per-module derivations,
          # roughly 45-60 minutes measured on an M-series Mac for
          # aarch64-darwin. That tradeoff was not weighed against
          # downgrading malgo's own toolchain to nixpkgs' cached 4.30.0:
          # `lean/lean-toolchain` has pinned 4.32.0 since the Lean port's
          # original scaffold, predating this flake by months, and the M0-M9
          # port is large enough that revisiting the toolchain version is a
          # separate decision from packaging it, not a free substitution
          # here.
          lean432Overlay = lean4-nix.readToolchainFile {
            toolchain = ./lean/lean-toolchain;
            binary = false;
          };

          pkgs = import nixpkgs {
            inherit system;
            overlays = [ lean432Overlay ];
          };

          # zig only needs to be on PATH at `malgo compile` *runtime* -- it's
          # not a build input. `lake build` embeds runtime/zig/runtime.zig as
          # text (`include_str`, see Backend/Zig/Runtime.lean) and never
          # invokes zig itself; only the resulting `malgo` binary's
          # `compile` subcommand shells out to a real `zig build-exe` when a
          # user runs it. nixpkgs pins zig_0_16 to exactly 0.16.0, matching
          # mise.toml's pin -- use that name explicitly rather than the
          # unversioned `zig` alias, which tracks whatever nixpkgs currently
          # defaults to. Confirmed nixpkgs carries `zig_0_16` for all three
          # `supportedSystems` above, not just aarch64-darwin.
          zig = pkgs.zig_0_16;
        in
        # `lake-manifest.json` (empty `packages`, no Lake dependencies -- see
        # lean/lake-manifest.json) lives under lean/, not the repo root, so
        # lean4-nix's `lake2nix.mkPackage` (built to shadow *other packages'*
        # `.lake/packages/` from the Nix store) solves a problem this
        # project doesn't have. A plain derivation is simpler and avoids
        # fighting that machinery over the subdirectory layout.
        #
        # `src` has to be the whole repo, not just `lean/`: Runtime.lean's
        # `include_str "../../../../runtime/zig/runtime.zig"` reaches
        # outside `lean/` to embed the Zig runtime source, so `runtime/`
        # must be present alongside `lean/` in the Nix build sandbox.
        pkgs.stdenv.mkDerivation {
          pname = "malgo";
          # Matches the latest release tag (`git tag`), not an independent
          # flake-only version line.
          version = "4.0.0";
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter =
              path: type:
              let
                base = baseNameOf path;
              in
              pkgs.lib.cleanSourceFilter path type
              && base != ".lake"
              && base != ".malgo-work"
              && base != "vendor";
          };

          nativeBuildInputs = [
            pkgs.lean.lean-all
            pkgs.makeWrapper
          ];

          buildPhase = ''
            runHook preBuild
            cd lean
            lake build malgo
            cd ..
            runHook postBuild
          '';

          # `$out/bin/malgo` alone isn't a complete package: any real
          # `.mlg` program needs `import "Builtin.mlg"`/`"Prelude.mlg"`
          # (and often `Either.mlg`) to resolve to *something*, and those
          # live in this source tree, not inside the compiled binary.
          # Installing them under `$out/share` makes that an explicit,
          # versioned part of this derivation's output instead of an
          # implicit contract on this flake's own `runtime/malgo/` layout
          # -- a consumer reaching past `${malgo}` into `${self}`'s raw
          # source tree (as a first attempt at using this flake did)
          # breaks the moment that layout changes, silently, in neither
          # repo. Not `runtime/malgo/compiler/` (the self-hosted
          # evaluator's own internals, not meant for external `.mlg`
          # programs to import) or `runtime/zig/` (already embedded into
          # the compiled binary via `include_str`, per Runtime.lean).
          installPhase = ''
            runHook preInstall
            mkdir -p "$out/bin" "$out/share/malgo/runtime/malgo"
            cp lean/.lake/build/bin/malgo "$out/bin/malgo"
            cp runtime/malgo/Builtin.mlg runtime/malgo/Prelude.mlg runtime/malgo/Either.mlg \
              "$out/share/malgo/runtime/malgo/"
            wrapProgram "$out/bin/malgo" --prefix PATH : ${pkgs.lib.makeBinPath [ zig ]}
            runHook postInstall
          '';
        };
    in
    {
      packages = forAllSystems (system: { default = mkMalgo system; });
    };
}
