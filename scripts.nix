{ s }:
rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:coercion-check' --allow-eval --warnings";
  ghcidTestScript = s "dev-test" "ghcid --command 'cabal new-repl test' --allow-eval --warnings";
  allScripts = [ ghcidScript ghcidTestScript ];
}
