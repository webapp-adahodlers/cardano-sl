{ localLib ? import ./../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, config ? {}
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../.git
, systemStart ? 0
, configurationKey ? "testnet_full"
, configurationKeyLaunch ? "testnet_launch"
, numCoreNodes ? 7
}:

with localLib;

let
  iohkPkgs = import ./../.. { inherit config system pkgs gitrev; };

  ghc = iohkPkgs.ghc.withPackages (ps: [ ps.cryptonite ps.canonical-json ]);
  python = pkgs.python3.withPackages (ps: [ ps.pyyaml ]);

  genesis-hash = pkgs.stdenv.mkDerivation {
    name = "genesis-hash";
    src = ./genesis-hash.hs;
    buildInputs = [ ghc ];
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -pv $out/bin
      echo "src is $src"
      ghc $src -o $out/bin/genesis-hash
    '';
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  };

  yaml2json = pkgs.writeScriptBin "yaml2json" ''
    #!${python}/bin/python
    import json
    import sys
    import yaml
    json.dump(yaml.load(open(sys.argv[1])), sys.stdout)
  '';

  genesisConfig = pkgs.stdenv.mkDerivation {
    name = "genesis-config";
    buildInputs = with pkgs; [ yaml2json jq python3 genesis-hash iohkPkgs.cardano-sl-tools ];
    src = iohkPkgs.cardano-sl.src + "/configuration.yaml";
    phases = [ "installPhase" ];
    installPhase = ''
      set -eo pipefail

      mkdir -p $out
      cd $out
      cp $src $out/configuration-launch.yaml

      # Generate very big random integer. For actual networks this
      # should be kept secret in a safe place.
      python -c "import secrets; print(secrets.randbelow(2**256))" > seed.txt

      ${if systemStart != 0 then "echo ${toString systemStart}" else "date '+%s'"} > system-start.txt

      cardano-keygen --system-start $(cat system-start.txt) \
        --configuration-file configuration-launch.yaml \
        --configuration-key ${configurationKeyLaunch} \
        --configuration-seed $(cat seed.txt) \
        dump-genesis-data --path genesis.json

      genesis-hash genesis.json > hash.txt

      # create updated configuration.yaml
      sed $(yaml2json configuration-launch.yaml | jq -r ".${configurationKey}.core.genesis.src|@text \"-e s/\\(.file)/genesis.json/ -e s/\\(.hash)/$(cat hash.txt)/\"") < configuration-launch.yaml > configuration.yaml

      cardano-keygen --system-start 0 \
        --configuration-file configuration.yaml \
        --configuration-key ${configurationKeyLaunch} \
        --configuration-seed $(cat seed.txt) \
        generate-keys-by-spec --genesis-out-dir ./genesis-keys
    '';
    passthru = { inherit genesis-hash; };
  };

in
  genesisConfig
