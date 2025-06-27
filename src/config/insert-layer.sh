
# This script inserts layer with id=$1 into SVG file $2

xmlstarlet ed --inplace \
-N s=http://www.w3.org/2000/svg \
-s /s:svg -t attr -n xmlns:inkscape -v http://www.inkscape.org/namespaces/inkscape \
-s /s:svg -t elem -n g \
--var NEWL '$prev' \
-s '$NEWL' -t attr -n inkscape:label -v hdt \
-s '$NEWL' -t attr -n inkscape:groupmode -v layer \
-s '$NEWL' -t attr -n id -v "$1" \
"$2"

