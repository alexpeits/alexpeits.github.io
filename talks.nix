{ pkgs ? import <nixpkgs> {} }:

let

  mkTalk = { url, name, date, locationUrl, location }: ''
    <tr>
      <td>
        <a href="${url}" class="posts-table-title">${name}</a>
        <div class="posts-table-description">${date}</div>
        <div class="posts-table-tags tags"><a href="${locationUrl}">${location}</a></div>
      </td>
    </tr>
  '';

  proofs =
    mkTalk {
      url = "https://github.com/alexpeits/habito-proofs-talk";
      name = "Proofs in Haskell using type-level features and dependent types";
      date = "April 2019";
      locationUrl = "https://www.habito.com";
      location = "habito";
    };

  python-metaclasses =
    mkTalk {
      url = "https://alexpeits.github.io/metaclasses-pythonmeetup-hsgr";
      name = "Python Metaclasses and Descriptors";
      date = "June 2017";
      locationUrl = "https://www.hackerspace.gr";
      location = "hackerspacegr";
    };

  talks = [
    proofs
    python-metaclasses
  ];

  content = ''
    Here you can find all my past talks:

    <table class="posts-table">
      <tbody>
        ${pkgs.lib.concatStringsSep "\n" talks}
      </tbody>
    </table>
  '';

in

{
  meta = { title = "Talks"; };
  content = content;
}
