{ pkgs ? import <nixpkgs> {} }:

let
  lib = pkgs.lib;
  b = builtins;
  py = pkgs.python37.withPackages (p: [ p.markdown p.pygments ]);

  ghc-with-pandoc = pkgs.haskellPackages.ghcWithPackages (p: [ p.pandoc ]);
  pygments-filter = pkgs.runCommand "pygments-filter" { buildInputs = [ ghc-with-pandoc ]; } ''
    mkdir -p $out/bin
    ghc --make ${./pygments_filter.hs} -o $out/bin/pygments-filter
  '';

  extractDate = fname:
    let
      parts = lib.forEach (lib.take 3 (lib.splitString "-" fname)) lib.toInt;
    in
      {
        year = lib.elemAt parts 0;
        month = lib.elemAt parts 1;
        day = lib.elemAt parts 2;
      };

  cmpDates = dt1: dt2:
    let
      y1 = dt1.year; m1 = dt1.month; d1 = dt1.day;
      y2 = dt2.year; m2 = dt2.month; d2 = dt2.day;
    in
      if y1 == y2 then (if m1 == m2 then d1 < d2 else m1 < m2) else y1 < y2;

  fmtDate = { year, month, day }:
    let
      months =
        [ "January" "February" "March" "April" "May" "June" "July"
          "August" "September" "October" "November" "December"
        ];
      fmtMonth = lib.elemAt months (month - 1);
    in
      "${fmtMonth} ${b.toString day}, ${b.toString year}";

  fmtDateShort = { year, month, day }:
    let
      pad = x: lib.fixedWidthString 2 "0" (b.toString x);
    in
    "${b.toString year}.${pad month}.${pad day}";

  fmtTag = tag: "<a href=\"/tags/${tag}.html\">${tag}</a>";

  processMeta = fname: meta:
    meta // {
      title = lib.elemAt meta.title 0;
      date = extractDate fname;
      tags = if (meta ? tags) then meta.tags else [];
    };

  sortMds = mds:
    let
      cmp = md1: md2: cmpDates md1.meta.date md2.meta.date;
    in
      lib.sort (x: y: ! (cmp x y)) mds;

  parseMd = path:
    let
      fname = lib.head (lib.splitString "." (lib.last (lib.splitString "/" (b.toString path))));
      name = b.replaceStrings [ "/" "." ] [ "-" "-" ] fname;

      meta = pkgs.runCommand (name + "-meta") { buildInputs = [ py ]; } ''
        python ${./front_matter_to_json.py} -i ${path} -o $out
      '';

      html = pkgs.runCommand (name + "-html") { buildInputs = [ pkgs.pandoc py ]; } ''
        pandoc ${path} --mathjax --to=html > $out
      '';
        # pandoc ${path} --mathjax --to=html -F ${pygments-filter}/bin/pygments-filter > $out

    in
      { meta = processMeta fname (lib.importJSON meta);
        html = lib.readFile html;
        fname = fname;
      };

  # filepaths
  allFilesIn = path: b.attrNames (b.readDir path);
  allFilepathsIn = path: map (n: path + ("/" + n)) (allFilesIn path);
  allFilesWithExtIn = path: ext:
    b.filter (n: b.match ".*\\.${ext}" n != null) (allFilesIn path);
  allFilepathsWithExtIn = path: ext:
    map (n: path + ("/" + n)) (allFilesWithExtIn path ext);

  # TODO: sort
  tagsMap = mds:
    let
      go = acc: md: lib.foldl (addTag md) acc md.meta.tags;
      addTag = md: acc: t:
        let v = if (acc ? "${t}") then acc."${t}" else [];
        in acc // { "${t}" = v ++ [md]; };
      mapping = lib.foldl go {} mds;
    in
      lib.mapAttrs (k: v: sortMds v) mapping;

  tmplBase = title: content: ''
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Alex's blog - ${title}</title>
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
  </head>
  <body>
    <header>
      <div class="logo">
        <a href="/">Alex's blog</a>
      </div>
      <nav>
        <a class="nav-link" href="/">Home</a>
        <a class="nav-link" href="/tags.html">Tags</a>
        <a class="nav-link" href="/projects.html">Projects</a>
        <a class="nav-link" href="/talks.html">Talks</a>
        <a class="nav-link" href="/about.html">About</a>
      </nav>
    </header>

    <main role="main">
      <h1>${title}</h1>
      ${content}
    </main>

    <footer>
      Site generated using <a href="https://github.com/alexpeits/alexpeits.github.io/blob/develop/default.nix">nix</a>
    </footer>
  </body>
</html>
  '';

  tmplPost = { meta, html, ... }: ''
<article>
  <section class="header">
    Posted on ${fmtDate meta.date}
  </section>
  <section class="header tags">
    ${if (b.length meta.tags == 0) then "" else "Tags:"} ${b.concatStringsSep ", " (map fmtTag meta.tags)}
  </section>
  <section>
    ${html}
  </section>
</article>
  '';

  postUrl = {fname, ...}: "/posts/${fname}.html";

  fmtPostSummaryTable = md: ''
<tr>
  <td class="posts-table-date">${fmtDateShort md.meta.date}</td>
  <td>
    <a href="${postUrl md}" class="posts-table-title">${md.meta.title}</a>
    <div class="posts-table-tags tags">
      ${b.concatStringsSep ", " (map fmtTag md.meta.tags)}
    </div>
  </td>
</tr>
  '';

  tmplPostTable = mds: ''
<table class="posts-table">
  <tbody>
    ${b.concatStringsSep "\n" (map fmtPostSummaryTable (sortMds mds))}
  </tbody>
</table>
  '';

  fmtPostSummaryList = md: ''
<li>
  <a href="${postUrl md}">${md.meta.title}</a> - ${fmtDate md.meta.date}
</li>
  '';

  tmplPostList = mds: ''
<ul>
  ${b.concatStringsSep "\n" (map fmtPostSummaryList mds)}
</ul>
  '';

  tagUrl = tag: "/tags/${tag}.html";

  fmtTagSummaryList = tag: mds: ''
<li class="tags">
  <a href="${tagUrl tag}">${tag}</a>
</li>
  '';

  tmplTagList = tags: ''
<ul>
  ${b.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs fmtTagSummaryList tags))}
</ul>
  '';

  tmplTagPostList = mds: tmplPostList mds;
  mkTagPostList = tag: mds:
    tmplBase ''Posts tagged "${tag}"'' (tmplTagPostList mds);

  # testing
  posts = map parseMd (allFilepathsWithExtIn ./posts "md");
  postTable = tmplBase "Posts" (tmplPostTable posts);
  tags = tagsMap posts;
  tagList = tmplBase "Tags" (tmplTagList tags);
  # tag -> content
  tagPages = lib.mapAttrs mkTagPostList tags;
  tagPagesScript =
    let
      script = tag: content: ''
      cat << \EOF > $out/tags/${tag}.html
        ${content}
      EOF
      '';
    in
      b.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs script tagPages));
  postsScript =
    let
      script = md: ''
        cat << \EOF > $out/${postUrl md}
          ${tmplBase md.meta.title (tmplPost md)}
        EOF
      '';
    in
      b.concatStringsSep "\n" (map script posts);

  otherPages = map parseMd [./projects.md ./talks.md ./about.md];
  otherPagesScript =
    let
      script = md: ''
        cat << \EOF > $out/${md.fname}.html
          ${tmplBase md.meta.title md.html}
        EOF
      '';
    in
      b.concatStringsSep "\n" (map script otherPages);

  buildInfo = "build-info.txt";
  buildInfoScript = ''
    rm -f $out/${buildInfo}
    touch $out/${buildInfo}
    git -C ${./.} log -1 --format=%H >> $out/${buildInfo}
    git -C ${./.} log -1 --format=%cd >> $out/${buildInfo}
  '';
in

pkgs.runCommand "blog" { buildInputs = [ pkgs.git ]; } ''
  # static files
  mkdir -p $out/static
  cp -R ${./static}/* $out/static

  # index
  cat << \EOF > $out/index.html
    ${postTable}
  EOF

  # posts
  mkdir -p $out/posts
  ${postsScript}

  # other pages
  ${otherPagesScript}

  # tags list
  cat << \EOF > $out/tags.html
    ${tagList}
  EOF

  # per-tag pages
  mkdir -p $out/tags
  ${tagPagesScript}

  # extra files, copy as-is
  cp -R ${./extra_files}/* $out/

  ${buildInfoScript}
''
