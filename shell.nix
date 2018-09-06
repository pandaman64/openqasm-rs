with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "openqasm-rs";
  buildInputs = [
    bashInteractive
    rustup
  ];
}
