let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "master";
        rev = "4937e428ca69b814775d05c256d4425d4a3f317b"; # latest as of Sep 10
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            fmt
            aeson
            unordered-containers
            extra
            cmark
            named
            time
            aeson
            neat-interpolation
            optics
            p.ihp
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
