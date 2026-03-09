let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in

let
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };

in
pkgs.mkShell {
  buildInputs = [
    stack-wrapped
  ]
  ++ (with pkgs; [
    fourmolu
    hlint
    (haskell-language-server.override { supportedGhcVersions = [ "9103" ]; })
    zlib
    pkg-config
  ]);

  NIX_PATH = "nixpkgs=" + pkgs.path;
}
