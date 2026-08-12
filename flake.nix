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
      system = "aarch64-darwin";

      # `binary = false` builds Lean from source (lean4-nix's own CMake
      # bootstrap: a stage0 compiler, then Init/Std/Lean/Lake compiled by
      # it) instead of the default `binary = true`, which fetches a
      # pre-built macOS release. The pre-built release's own
      # `libInit_shared.dylib` has no Mach-O header slack for
      # `install_name_tool` to rewrite its rpath ("load commands do not fit
      # in __TEXT segment filesize"), and every workaround tried for that
      # (a no-op `fixDarwinDylibNames`, setting `DYLD_LIBRARY_PATH`) failed
      # to make the resulting binary loadable. Building from source
      # sidesteps this: a binary Nix's own `stdenv` compiles gets its
      # rpaths right as a normal side effect of compilation, no post-hoc
      # rewrite needed. `manifests/v4.32.0.nix`'s `bootstrap` is exactly
      # this version -- it already carries an OpenSSL >=3 fix specific to
      # 4.32.0's TLS support, so it's not a generic/untested path.
      lean432Overlay = lean4-nix.readToolchainFile {
        toolchain = ./lean/lean-toolchain;
        binary = false;
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

        buildPhase = ''
          runHook preBuild
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
