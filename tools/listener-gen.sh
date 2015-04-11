#!/usr/bin/env bash

set -euo pipefail
declare -r projectDir="$(dirname "$(dirname "$(realpath "$0")")")"
. "$projectDir/common.sh"

forEachListener() {
    local fn="${1:?forEachListener requires the name of a function to call.}"

    # Keep the imports in the Haskell section up-to-date with these definitions.
    $fn Int "int" "TInt"
    $fn IntInt "int|int" "TInt|TInt"
    $fn QString "QString" "TObj c_QString"
}

#### Generate C++ listener classes.

echo
msg "Generating C++ listener classes."
exec \
    {fhpp}>"$projectDir/lang/cpp/listeners.hpp" \
    {fcpp}>"$projectDir/lang/cpp/listeners.cpp"
sayHpp() { echo "$*" >&$fhpp; }
sayCpp() { echo "$*" >&$fcpp; }

sayHpp '#include <QObject>'

sayCpp '#include "listeners.hpp"'

writeCpp() {
    local -r name="${1:?}" params="${2:?}"
    local -r className="Listener${name}"
    local -r paramTypeList="$(echo "${params}" | sed 's/|/, /g')"
    local paramList=""
    local paramNameList=""
    local n=1
    while read type; do
        [[ -n $paramList ]] && paramList+=', '
        [[ -n $paramNameList ]] && paramNameList+=', '
        paramList+="${type} arg${n}"
        paramNameList+="arg${n}"
        ((n++))
    done < <(tr '|' '\n' <<<"$params")

    sayHpp
    sayHpp "class ${className} : public QObject {"
    sayHpp "    Q_OBJECT"
    sayHpp
    sayHpp "public:"
    sayHpp "    typedef void (*fptr)(${paramTypeList});"
    sayHpp "    ${className}(fptr f, QObject* parent = 0);"
    sayHpp
    sayHpp "public slots:"
    sayHpp "    void invoke(${paramList});"
    sayHpp
    sayHpp "private:"
    sayHpp "    const fptr f_;"
    sayHpp "};"

    sayCpp
    sayCpp "${className}::${className}(${className}::fptr f, QObject* parent) :"
    sayCpp "    QObject(parent), f_(f) {}"
    sayCpp
    sayCpp "void ${className}::invoke(${paramList}) {"
    sayCpp "    f_(${paramNameList});"
    sayCpp "}"
}

forEachListener writeCpp
exec {fhpp}>&- {fcpp}>&-
unset fhpp fcpp sayHpp sayCpp writeCpp

#### Generate Haskell binding definitions for the listeners.

echo
msg "Generating Haskell listener binding definitions."
exec {fhs}>"$projectDir/lang/hs/src-generator/Graphics/UI/Qtah/Internal/Interface/Listeners.hs"
say() { echo "$*" >&$fhs; }

say 'module Graphics.UI.Qtah.Internal.Interface.Listeners where'
say
say 'import Foreign.Cppop.Generator.Spec'
say 'import Graphics.UI.Qtah.Internal.Interface.QObject'
say 'import Graphics.UI.Qtah.Internal.Interface.QString'

writeHs() {
    local -r name="${1:?}" hsTypes="${3:?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"

    local argTypeList=""
    while read type; do
        [[ -n $argTypeList ]] && argTypeList+=', '
        argTypeList+="$type"
    done < <(tr '|' '\n' <<<"$hsTypes")
    argTypeList="[$argTypeList]"

    say
    say "${classVar} ="
    say "  makeClass (ident \"${className}\") Nothing [c_QObject]"
    say "  [ Ctor (toExtName \"${className}_new\")"
    say "         [TPtr \$ TFn ${argTypeList} TVoid]"
    say "  , Ctor (toExtName \"${className}_newWithParent\")"
    say "         [TPtr \$ TFn ${argTypeList} TVoid, TPtr \$ TObj c_QObject]"
    say "  ]"
    say "  []"
}
forEachListener writeHs

say
say "allListeners :: [Export]"
say "allListeners ="
cont="["
writeHs() {
    local -r name="${1:?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"

    say "  ${cont} ExportClass ${classVar}"
    if [[ $cont = '[' ]]; then cont=','; fi
}
forEachListener writeHs
say "  ]"

exec {fhs}>&-
unset fhs writeHs
