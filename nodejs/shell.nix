{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  name = "node";
  nativeBuildInputs = with pkgs; [ ijq jq nodejs_21 ];
}
