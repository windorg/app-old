let
    # ihp = builtins.fetchGit {
    #     url = "https://github.com/digitallyinduced/ihp.git";
    #     ref = "refs/tags/v0.15.0";
    # };
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "master";
        rev = "4348601f54a43ebcaadf887de7b735439283e066";
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
            ihp-sentry
            p.ihp
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
