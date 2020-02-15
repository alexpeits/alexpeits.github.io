{ pkgs ? import <nixpkgs> {} }:

let

  mkProject = { url, name, desc, docs }: ''
    <tr>
      <td>
        <a href="${url}" class="posts-table-title">${name}</a>
        <div class="posts-table-description">${desc}</div>
        <div class="posts-table-tags tags"><a href="${docs}">Go to docs</a></div>
      </td>
    </tr>
  '';

  harg =
    mkProject {
      url = "https://github.com/alexpeits/harg";
      name = "harg";
      desc = "Haskell program configuration using higher kinded data";
      docs = "http://alexpeits.github.io/harg";
    };
  haskell-nix-cookiecutter =
    mkProject {
      url = "https://github.com/alexpeits/haskell-nix-cookiecutter";
      name = "nix-haskell-cookiecutter";
      desc = "Cookiecutter template to quickly generate haskell projects (cabal, nix, direnv)";
      docs = "https://github.com/alexpeits/haskell-nix-cookiecutter/blob/master/README.md";
    };
  statue =
    mkProject {
      url = "https://github.com/alexpeits/statue";
      name = "statue";
      desc = "Simple static site generator using only nix";
      docs = "https://github.com/alexpeits/statue/blob/master/README.md";
    };

  projects = [
    harg
    haskell-nix-cookiecutter
    statue
  ];

  content = ''
    <table class="posts-table">
      <tbody>
        ${pkgs.lib.concatStringsSep "\n" projects}
      </tbody>
    </table>
  '';

in

{
  meta = { title = "Projects"; };
  content = content;
}
