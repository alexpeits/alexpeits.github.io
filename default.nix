{ pkgs ? import <nixpkgs> {}, statue ? null }:

let
  config = {
    siteTitle = "Alex's blog";
    navPages = [ ./projects.nix ./talks.nix ./about.md ];
    rootDir = ./.;
    postsDir = ./posts;
    htmlHead = ''
      <link rel="shortcut icon" type="image/png" href="/images/favicon.png"/>
      <link rel="stylesheet" href="/css/default.css" />
      <link rel="stylesheet" href="/css/syntax.css" />
      <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=PT+Serif:400,400italic,700%7CPT+Sans:400" />
      <script type="text/x-mathjax-config">
       MathJax.Hub.Config({
         "HTML-CSS": { linebreaks: { automatic: true } },
         SVG: { linebreaks: { automatic: true } },
         messageStyle: "none"
       });
      </script>
      <script
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        type="text/javascript">
      </script>
    '';
    extraScript = {
      inputs = p: [ p.minify ];
      script = ''
        cp -R ${./static/images} $out/images
        cp -R ${./static/keybase.txt} $out/keybase.txt

        mkdir -p $out/css
        for css in $(find ${./static/css} -name '*.css'); do
          minify -o $out/css/$(basename $css) $css
        done
      '';
    };
  };

  statue-src-github =
    let src = pkgs.lib.importJSON ./statue.json; in
      pkgs.fetchFromGitHub {
        owner = "alexpeits";
        repo = "statue";
        rev = src.rev;
        sha256 = src.sha256;
      };

  statue-src = if statue != null then statue else statue-src-github;

in

import statue-src { config = config; }
