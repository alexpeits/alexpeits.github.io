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
  };

  statue-src-github = pkgs.fetchFromGitHub {
    owner = "alexpeits";
    repo = "statue";
    rev = "9e4836c04680d987f7a9a4f4898044c2b24923fa";
    sha256 = "15mzkgdm1dbym8y301l5rd4vflycmsvwpkpdhlzh1g0m17qqfsbz";
  };

  statue-src = if statue != null then statue else statue-src-github;

in

import statue-src { config = config; }
