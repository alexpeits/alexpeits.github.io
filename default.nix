{ pkgs ? import <nixpkgs> {}, statue ? null }:

let
  config = {
    siteTitle = "Alex's blog";
    navPages = [ ./projects.nix ./talks.nix ./about.md ];
    rootDir = ./.;
    postsDir = ./posts;
    staticDir = ./static;
    extraFilesDir = ./extra_files;
    htmlHead = ''
      <link rel="shortcut icon" type="image/png" href="/static/images/favicon.png"/>
      <link rel="stylesheet" href="/static/css/default.css" />
      <link rel="stylesheet" href="/static/css/syntax.css" />
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
  };

  statue-src-github = pkgs.fetchFromGitHub {
    owner = "alexpeits";
    repo = "statue";
    rev = "5828b98a655cc139f75deddfea126c21c09f5ad3";
    sha256 = "084m82gf29ri7iiysa17a5nqamgpc7asmiy6v91dhlyq0mvb95pj";
  };

  statue-src = if statue != null then statue else statue-src-github;

in

import statue-src { config = config; }
