{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: {
  project = {
    name = "dualizer";
    summary = "Automatically generate dual constructions";
  };

  imports = [./hlint.nix];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git.enable = true;
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt.enable = true;
    vale.enable = true;
  };

  ## CI
  services.garnix.enable = true;
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      self.lib.githubSystems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    systems = self.lib.githubSystems;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {"${config.project.name}" = config.project.name;};
    latestGhcVersion = "9.10.1";
    exclude = [
      ## Fails to load libgmp.dylib.
      {
        bounds = "--prefer-oldest";
        ghc = "8.6.1";
        os = "macos-13";
      }
      ## FIXME: These should live in Flaky, only here for testing!
      {
        ghc = "9.4.1";
        os = "macos-14";
      }
    ];
    include = map (bounds: {
      inherit bounds;
      ghc = "9.4.5";
      os = "macos-14";
    }) ["--prefer-oldest" ""];
  };

  ## publishing
  services.github.enable = true;
  services.github.settings.repository.topics = [];
}
