#!/bin/sh
# Generate wiki API documentation using ocamldoc + wikidoc plugin.
# Usage: ./build/gen_wikidoc.sh
#
# Output goes to _build/doc/dev/api/.
# To publish, copy the generated wikis to doc/dev/api/ on the
# [wikidoc] branch.

set -e

WIKIDOC_DIR=$(ocamlfind query wikidoc)
PACKAGES=logs,react,lwt,lwt.unix,lwt_react,cohttp,cohttp-lwt,cohttp-lwt-unix,ipaddr,re,xml-light,cryptokit,ssl,syslog-message,http,uri
OUTDIR=_build/doc/dev/api

dune build @check || true

# Wrapper objects directories produced by dune
SERVER_OBJS=_build/default/src/server/.ocsigenserver.objs/byte
BASELIB_OBJS=_build/default/src/baselib/.baselib.objs/byte
HTTP_OBJS=_build/default/src/http/.ocsigen_http.objs/byte
COOKIE_OBJS=_build/default/src/http/.ocsigen_cookie_map.objs/byte
LIBBASE_OBJS=_build/default/src/baselib/.ocsigen_lib_base.objs/byte
POLY_OBJS=_build/default/src/baselib/polytables/.polytables.objs/byte

PKG_INCLUDES=""
for pkg in $(echo "$PACKAGES" | tr ',' ' '); do
  pkgdir=$(ocamlfind query "$pkg" || true)
  [ -n "$pkgdir" ] && PKG_INCLUDES="$PKG_INCLUDES -I $pkgdir"
done

# Includes for cross-references between wrappers.
WRAPPER_INCLUDES="-I $SERVER_OBJS -I $BASELIB_OBJS -I $HTTP_OBJS \
                  -I $COOKIE_OBJS -I $LIBBASE_OBJS -I $POLY_OBJS"

TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

# Create short-name aliases for each wrapped module so ocamldoc can
# resolve [Module.X] references (instead of [Wrapper__Module.X]).
make_aliases () {
  WRAPPER="$1"
  CMI_DIR="$2"
  for cmi in "$CMI_DIR"/${WRAPPER}__*.cmi; do
    [ -f "$cmi" ] || continue
    base=$(basename "$cmi" .cmi)
    short=$(echo "$base" | sed "s/^${WRAPPER}__//")
    modname=$(echo "${base}" | sed 's/^./\U&/')
    echo "include ${modname}" > "$TMPDIR/${short}.ml"
  done
}
make_aliases ocsigen "$SERVER_OBJS"
make_aliases ocsigen_base "$BASELIB_OBJS"
make_aliases ocsigen_http "$HTTP_OBJS"

for pass in 1 2 3 4; do
  compiled=0
  for ml in "$TMPDIR"/*.ml; do
    [ -f "$ml" ] || continue
    short=$(basename "$ml" .ml)
    if [ ! -f "$TMPDIR/${short}.cmi" ]; then
      if eval ocamlfind ocamlc -package "$PACKAGES" -c \
        $WRAPPER_INCLUDES -I "$TMPDIR" \
        -o "$TMPDIR/${short}.cmo" "$ml" 2>/dev/null; then
        compiled=$((compiled + 1))
      fi
    fi
  done
  [ "$compiled" -eq 0 ] && break
done
echo "Created $(ls "$TMPDIR"/*.cmi 2>/dev/null | wc -l) module aliases"

rm -rf "$OUTDIR"
mkdir -p "$OUTDIR"

# Collect all .mli files from wrapped libraries, standalone modules
# and extensions, plus all the relevant -I paths so a single ocamldoc
# invocation can produce a complete index.wiki.
MLI_FILES=""
for d in src/server/Ocsigen src/baselib/Ocsigen_base src/http/Ocsigen_http; do
  MLI_FILES="$MLI_FILES $(ls $d/*.mli 2>/dev/null)"
done
MLI_FILES="$MLI_FILES \
  src/baselib/ocsigen_lib_base.mli \
  src/http/ocsigen_cookie_map.mli \
  src/baselib/polytables/polytables.mli"

EXT_INCLUDES=""
for ext in accesscontrol authbasic cors deflatemod extendconfiguration \
           outputfilter redirectmod revproxy rewritemod staticmod userconf; do
  EXT_INCLUDES="$EXT_INCLUDES -I _build/default/src/extensions/.${ext}.objs/byte"
  MLI_FILES="$MLI_FILES src/extensions/${ext}.mli"
done

echo "Generating wiki documentation..."
eval ocamldoc \
  -colorize-code -stars -sort \
  -I "$TMPDIR" $WRAPPER_INCLUDES $EXT_INCLUDES $PKG_INCLUDES \
  -intro doc/indexdoc \
  -g "$WIKIDOC_DIR/odoc_wiki.cma" \
  -d "$OUTDIR" \
  $MLI_FILES

echo "Done: $OUTDIR/"
echo "  $(ls "$OUTDIR"/*.wiki 2>/dev/null | wc -l) wiki files"
