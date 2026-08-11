{
  description = "Malgo: a statically typed functional language (Lean 4 + Zig backend)";

  inputs = {
    # Not `nixpkgs.follows = "lean4-nix/nixpkgs"`: lean4-nix's own pin
    # predates `zig_0_16` (nixpkgs added it after lean4-nix's pinned
    # revision), and that attribute name is a hard requirement, not a
    # preference -- Zig minor releases break std (see mise.toml), so
    # silently falling back to zig_0_15 would be the wrong kind of quiet.
    # `readToolchainFile`'s overlay only fetches a pinned Lean *binary*
    # release; it doesn't depend on nixpkgs' own package versions, so
    # applying it to an independently-pinned nixpkgs is safe.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    lean4-nix.url = "github:lenianiva/lean4-nix";
  };

  outputs =
    { self, nixpkgs, lean4-nix }:
    let
      system = "aarch64-darwin";

      # Not the `lean4-nix.readToolchainFile ./lean/lean-toolchain`
      # convenience wrapper: it calls `toolchain.nix` with no override, so
      # `fixDarwinDylibNames` resolves to nixpkgs' real hook, which fails on
      # this pre-built (not Nix-compiled) macOS binary --
      # `install_name_tool: ... larger updated load commands do not fit
      # (the program must be relinked)`. The reference flake at
      # github.com/lenianiva/lean4-nix/blob/main/templates/mathlib-demo/flake.nix
      # works around the identical failure the same way: call `toolchain.nix`
      # directly with a no-op override.
      lean432Overlay =
        final: prev:
        {
          lean = (
            (final.callPackage "${lean4-nix}/lib/toolchain.nix" {
              fixDarwinDylibNames = final.writeTextFile {
                name = "noop-fix-darwin-dylib-names-hook";
                destination = "/nix-support/setup-hook";
                text = "";
              };
            }).fetchBinaryLean
              (import "${lean4-nix}/manifests/v4.32.0.nix")
          );
        };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ lean432Overlay ];
      };

      # zig only needs to be on PATH at `malgo compile` *runtime* -- it's not
      # a build input. `lake build` embeds runtime/zig/runtime.zig as text
      # (`include_str`, see Backend/Zig/Runtime.lean) and never invokes zig
      # itself; only the resulting `malgo` binary's `compile` subcommand
      # shells out to a real `zig build-exe` when a user runs it. nixpkgs
      # pins zig_0_16 to exactly 0.16.0, matching mise.toml's pin -- use
      # that name explicitly rather than the unversioned `zig` alias, which
      # tracks whatever nixpkgs currently defaults to.
      zig = pkgs.zig_0_16;

      # `lake-manifest.json` (empty `packages`, no Lake dependencies -- see
      # lean/lake-manifest.json) lives under lean/, not the repo root, so
      # lean4-nix's `lake2nix.mkPackage` (built to shadow *other packages'*
      # `.lake/packages/` from the Nix store) solves a problem this project
      # doesn't have. A plain derivation is simpler and avoids fighting that
      # machinery over the subdirectory layout.
      #
      # `src` has to be the whole repo, not just `lean/`: Runtime.lean's
      # `include_str "../../../../runtime/zig/runtime.zig"` reaches outside
      # `lean/` to embed the Zig runtime source, so `runtime/` must be
      # present alongside `lean/` in the Nix build sandbox.
      malgo = pkgs.stdenv.mkDerivation {
        pname = "malgo";
        version = "0.1.0";
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

        # STATUS: this derivation does not build yet. `lake build malgo`
        # fails with:
        #   dyld[...]: Library not loaded: @rpath/libInit_shared.dylib
        #   Referenced from: .../lean/bin/.lake-wrapped
        #   Reason: tried: '.../lean/lib/lean/libInit_shared.dylib'
        #     (load commands do not fit in __TEXT segment filesize) ...
        #
        # Root cause is upstream in lean4-nix's handling of the pre-built
        # (not Nix-compiled) Lean 4.32.0 darwin_aarch64 release, not in this
        # flake's own derivation. What's been ruled out, each confirmed by
        # direct testing rather than assumption:
        #   - Not a stale-rpath-with-a-fixup-available problem: the real
        #     `fixDarwinDylibNames` hook's `install_name_tool` rewrite fails
        #     outright on this binary ("larger updated load commands do not
        #     fit (the program must be relinked)") -- there is no free
        #     header space to rewrite the load command in place, which is
        #     why the no-op override above exists at all.
        #   - Not a missing-file problem: `libInit_shared.dylib` genuinely
        #     exists at the exact path dyld's own error message lists as
        #     "tried" (${pkgs.lean}/lib/lean/libInit_shared.dylib) --
        #     confirmed with `find` against the built `pkgs.lean` output.
        #   - Not stdenv-darwin's DYLD_* stripping: `DYLD_LIBRARY_PATH` was
        #     set both as a derivation attribute and as an `export` inside
        #     this phase's own script body (i.e. after stdenv's setup
        #     script runs); neither took effect, and reading `lean`'s own
        #     `lake` wrapper script confirms it doesn't touch DYLD_* itself
        #     -- it only rewrites PATH before `exec -a "$0" .lake-wrapped`.
        #   - Not a hardened-runtime/entitlement problem: `codesign -dv
        #     --entitlements -` on `.lake-wrapped` shows
        #     `flags=0x20002(adhoc,linker-signed)`, no restrict flag and no
        #     entitlements at all, so macOS has no declared reason to ignore
        #     DYLD_* for this binary.
        # None of these four rule-outs identifies what dyld is actually
        # doing instead, so the true root cause is still open. Two
        # escape hatches for whoever picks this up next, neither attempted
        # here: (a) build Lean from source via lean4-nix's bootstrap path
        # instead of fetching the pre-built binary release, sidestepping
        # binary relocation entirely; (b) drop the flake and have
        # consumers invoke a `mise`-built `malgo` directly -- the actual
        # Phase 2 requirement was always "a reproducible way to get
        # compiled `.scm`", not "a flake" specifically, and nixpkgs'
        # `chez` (10.4.1, matching what's already validated locally) is
        # confirmed available on aarch64-darwin regardless of how `malgo`
        # itself gets built.
        buildPhase = ''
          runHook preBuild
          export DYLD_LIBRARY_PATH="${pkgs.lean}/lib:${pkgs.lean}/lib/lean"
          cd lean
          lake build malgo
          cd ..
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          mkdir -p "$out/bin"
          cp lean/.lake/build/bin/malgo "$out/bin/malgo"
          wrapProgram "$out/bin/malgo" --prefix PATH : ${pkgs.lib.makeBinPath [ zig ]}
          runHook postInstall
        '';
      };
    in
    {
      packages.${system}.default = malgo;
    };
}
