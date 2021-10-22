{ s }:
rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:coercion-check' --allow-eval --warnings";
  allScripts = [ ghcidScript ];
}
