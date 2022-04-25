let
  opam-nix-integration = import (
    fetchTarball "https://github.com/vapourismo/opam-nix-integration/archive/345ccc3c2491b597a55de481af05ff82953f6c11.tar.gz"
  );

  pkgs =
    import
      (fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz")
      { overlays = [ opam-nix-integration.overlay ]; };

  opam-repository = pkgs.fetchFromGitLab {
    owner = "tezos";
    repo = "opam-repository";
    rev = "fcd1fdae683f183947eeecf85887160dbf547a84";
    sha256 = "sha256-e1yai29/EzunOAKoegEckXvV+uF2A/P87ngYXW8xRo4=";
  };

  packageSet = pkgs.opam-nix-integration.makePackageSet {
    repository = opam-repository;

    overlays = [
      # First overlay simply picks the package versions from Tezos'
      # opam-repository.
      (final: prev:
        builtins.mapAttrs
          (name: versions: versions.latest)
          prev.repository.packages
      )
      # The second overlay tweaks the dependencies.
      (final: prev: {
        ocaml-base-compiler = prev.ocaml-base-compiler.override {
          # Compile faster!
          jobs = 10;
        };

        hacl-star-raw = prev.hacl-star-raw.overrideAttrs (old: {
          # Uses unsupported command-line flags
          NIX_CFLAGS_COMPILE = "-Wno-unused-command-line-argument";
        });

        conf-rust = prev.conf-rust.overrideAttrs (old: {
          propagatedNativeBuildInputs =
            (old.propagatedNativeBuildInputs or [ ]) ++
            # Need Rust compiler - already fixed in upstream opam-repository
            [ pkgs.rustc ];
        });

        tezos-rust-libs = prev.tezos-rust-libs.overrideAttrs (old: {
          propagatedNativeBuildInputs =
            (old.propagatedNativeBuildInputs or [ ]) ++
            # Missing libiconv dependency
            [ pkgs.libiconv ];
        });
      })
    ];
  };

  packages =
    builtins.filter
      pkgs.lib.attrsets.isDerivation
      (builtins.attrValues packageSet);

  packageLibDirs =
    builtins.filter builtins.pathExists (
      builtins.map (package: "${package}/lib/${package.pname}") packages
    );

  packageLibSearchPath = builtins.concatStringsSep ":" packageLibDirs;

  packageLibArgs = builtins.map (dir: "-L${dir}") packageLibDirs;

  packageIncludeArgs = builtins.map (dir: "-I${dir}") packageLibDirs;
in

pkgs.mkShell {
  # To get static libraries into scope.
  NIX_LDFLAGS = packageLibArgs;

  NIX_CFLAGS_COMPILE =
    # Silence errors (-Werror) for unsupported flags on MacOS.
    [ "-Wno-unused-command-line-argument" ] ++
    # Make sure headers files are in scope.
    packageIncludeArgs;

  DYLD_LIBRARY_PATH = packageLibSearchPath;
  LD_LIBRARY_PATH = packageLibSearchPath;

  buildInputs = packages ++ (with pkgs; [
    nodejs
    cacert
    curl
  ]);

  # Disable OPAM usage in Makefile.
  TEZOS_WITHOUT_OPAM = true;

  shellHook = ''
    # Some components want to do stuff to the Opam switch, but we don't really
    # have one.
    mkdir -p _opam
    export OPAM_SWITCH_PREFIX=$(pwd)/_opam

    # Sapling parameters
    test -d _opam/share/zcash-params || ./scripts/install_sapling_parameters.sh
  '';
}
