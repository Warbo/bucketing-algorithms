{ lib, mkBin, python }:

with builtins;
with lib;
with rec {
  resources = {
    "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
      = resources/bootstrap.min.css;

    "//code.jquery.com/jquery-1.11.0.min.js"
      = resources/jquery-1.11.0.min.js;

    "//cdnjs.cloudflare.com/ajax/libs/flot/0.8.2/jquery.flot.min.js"
      = resources/jquery.flot.min.js;

    "//cdnjs.cloudflare.com/ajax/libs/flot/0.8.2/jquery.flot.time.min.js"
      = resources/jquery.flot.time.min.js;

    "//cdnjs.cloudflare.com/ajax/libs/flot/0.8.2/jquery.flot.selection.min.js"
      = resources/jquery.flot.selection.min.js;

    "//cdnjs.cloudflare.com/ajax/libs/flot/0.8.2/jquery.flot.categories.min.js"
      = resources/jquery.flot.categories.min.js;

    "//netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min.js"
      = resources/bootstrap.min.js;
  };

  mkCode = url: file: ''
    # Awkward comment to force Python-friendly indentation
      with open("${file}", 'r') as h:
        data = h.read()
      mime = {"js" : "text/javascript",
              "css" : "text/css"}["${file}".split(".")[-1]]
      content = content.replace(
        """${url}""",
        "data:" + mime + ";base64," + data.encode('base64'))
  '';

  replacements = mapAttrs mkCode resources;
};
mkBin {
  name   = "fixHtml";
  paths  = [ (python.withPackages (p: [])) ];
  script = ''
    #!/usr/bin/env python
    import sys
    if len(sys.argv) < 2:
      raise Exception("Need ASV dir as arg")

    import os
    d = sys.argv[1]
    if not os.path.exists(d):
      raise Exception("Given dir '{0}' doesn't exist".format(d))

    def transformFile(path, func):
      if not os.path.exists(path):
        raise Exception("No '{}' found".format(path))

      with open(path, 'r') as h:
        content = h.read()

      with open(path, 'w') as h:
        h.write(func(content))

    def inlineReplacements(content):
      ${concatStringsSep "\n  " (attrValues replacements)}
      return content

    def forceMimeTypes(content):
      return content.replace(
        'dataType: "json",',
        """
          dataType: "json",
          beforeSend: function(xhr){
            if (xhr.overrideMimeType) {
              xhr.overrideMimeType("application/json");
            }
          },
        """).replace(
          "url: 'regressions.json',",
          """
            url: 'regressions.json',
            beforeSend: function(xhr){
              if (xhr.overrideMimeType) {
                xhr.overrideMimeType("application/json");
              }
            },
          """)

    sys.stderr.write('Replacing third-party resources with data URIs\n')
    transformFile(d + '/index.html', inlineReplacements)

    sys.stderr.write('Forcing JSON MIME types\n')
    transformFile(d + '/asv.js',         forceMimeTypes)
    transformFile(d + '/regressions.js', forceMimeTypes)
  '';
}
