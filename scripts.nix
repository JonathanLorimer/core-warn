{ s }:
rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:oka-sock-it-to-me' --allow-eval --warnings";
  allScripts = [ ghcidScript ];
}
