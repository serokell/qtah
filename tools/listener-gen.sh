#!/usr/bin/env bash

set -euo pipefail
declare -r projectDir="$(dirname "$(dirname "$(realpath "$0")")")"
. "$projectDir/common.sh"

forEachListener() {
    local fn="${1:?forEachListener requires the name of a function to call.}"

    # Keep the imports in the Haskell section up-to-date with these definitions.
    #
    # TODO Check whether these C++ types need to be kept whitespace-free in
    # order for SLOT() to work below.
    $fn Bool "bool"
    $fn Int "int"
    $fn IntInt "int|int"
    $fn PtrQObject "QObject*"
    $fn QString "QString"
    $fn "" ""
}

#### Generate C++ listener classes.

echo
msg "Generating C++ listener classes."
exec \
    {fhpp}>"$projectDir/qtah/cpp/listeners.hpp" \
    {fcpp}>"$projectDir/qtah/cpp/listeners.cpp"
sayHpp() { echo "$*" >&$fhpp; }
sayCpp() { echo "$*" >&$fcpp; }

sayHpp '////////// GENERATED FILE, EDITS WILL BE LOST //////////'
sayHpp
sayHpp '#ifndef QTAH_LISTENERS_HPP'
sayHpp '#define QTAH_LISTENERS_HPP'
sayHpp
sayHpp '#include <string>'
sayHpp '#include <QObject>'
sayHpp '#include "callbacks.hpp"'

sayCpp '////////// GENERATED FILE, EDITS WILL BE LOST //////////'
sayCpp
sayCpp '#include "listeners.hpp"'
sayCpp
sayCpp '#include <iostream>'

writeCpp() {
    local -r name="${1?}" params="${2?}"
    local -r className="Listener${name}"
    local -r callbackClassName="Callback${name}Void"
    local paramList=""
    local paramTypeList=""
    local paramNameList=""
    local n=1
    if [[ -n $params ]]; then
        while read type; do
            [[ -n $paramList ]] && paramList+=', '
            [[ -n $paramTypeList ]] && paramTypeList+=','
            [[ -n $paramNameList ]] && paramNameList+=', '
            paramList+="${type} arg${n}"
            paramTypeList+="${type}"
            paramNameList+="arg${n}"
            ((n++))
        done < <(tr '|' '\n' <<<"$params")
    fi

    sayHpp
    sayHpp "class ${className} : public QObject {"
    sayHpp "    Q_OBJECT"
    sayHpp
    sayHpp "public:"
    sayHpp "    typedef ${callbackClassName} callback;"
    sayHpp "    ${className}(callback f, QObject* parent = 0);"
    sayHpp "    bool connectListener(QObject* source, const std::string& signal);"
    sayHpp
    sayHpp "public slots:"
    sayHpp "    void invoke(${paramList});"
    sayHpp
    sayHpp "private:"
    sayHpp "    callback f_;"
    sayHpp "    bool connected_;"
    sayHpp "};"

    sayCpp
    sayCpp "${className}::${className}(${className}::callback f, QObject* parent) :"
    sayCpp "    QObject(parent), f_(f), connected_(false) {}"
    sayCpp
    sayCpp "bool ${className}::connectListener(QObject* source, const std::string& signal) {"
    sayCpp "    if (connected_) {"
    sayCpp "        std::cerr <<"
    sayCpp "            \"${className}::connectListener: Internal error, already connected.  \""
    sayCpp "            \"Not connecting again.\\n\" << std::flush;"
    sayCpp "        return false;"
    sayCpp "    }"
    sayCpp "    setParent(source);"
    sayCpp "    return connected_ = connect(source, signal.c_str(), SLOT(invoke(${paramTypeList})));"
    sayCpp "}"
    sayCpp
    sayCpp "void ${className}::invoke(${paramList}) {"
    sayCpp "    f_(${paramNameList});"
    sayCpp "}"
}

forEachListener writeCpp
sayHpp
sayHpp '#endif'
exec {fhpp}>&- {fcpp}>&-
unset fhpp fcpp sayHpp sayCpp writeCpp

#### Generate Haskell binding definitions for the listeners.

echo
msg "Generating Haskell listener binding definitions."
exec {fhs}>"$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs"
say() { echo "$*" >&$fhs; }

say '---------- GENERATED FILE, EDITS WILL BE LOST ----------'
say
say 'module Graphics.UI.Qtah.Internal.Interface.Listener where'
say
say 'import qualified Foreign.Cppop.Generator.Spec as S'
say 'import qualified Foreign.Cppop.Generator.Std as Std'
say 'import qualified Graphics.UI.Qtah.Internal.Interface.Callback as C'
say 'import qualified Graphics.UI.Qtah.Internal.Interface.QObject as QObject'

writeHs() {
    local -r name="${1?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"
    local -r callbackVar="cb_${name}Void"

    say
    say "${classVar} ="
    say "  S.makeClass (S.ident \"${className}\") Nothing [QObject.c_QObject]"
    say "  [ S.makeCtor (S.toExtName \"${className}_new\")"
    say "    [S.TCallback C.${callbackVar}]"
    say "  , S.makeCtor (S.toExtName \"${className}_newWithParent\")"
    say "    [S.TCallback C.${callbackVar}, S.TPtr \$ S.TObj QObject.c_QObject]"
    say "  ]"
    say "  [ S.makeMethod \"connectListener\" (S.toExtName \"${className}_connectListener\")"
    say "    S.MNormal S.Nonpure [S.TPtr \$ S.TObj QObject.c_QObject, S.TObj Std.c_std__string] S.TBool"
    say "  ]"
}
forEachListener writeHs

say
say "allListeners :: [S.Class]"
say "allListeners ="
cont="["
writeHs() {
    local -r name="${1?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"

    say "  ${cont} ${classVar}"
    if [[ $cont = '[' ]]; then cont=','; fi
}
forEachListener writeHs
say "  ]"

exec {fhs}>&-
unset fhs writeHs

#### Generate a GHC .hs-boot file for cycles in the module graph around listeners.

echo
msg "Generating Haskell listener .hs-boot file."
exec {fhs}>"$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs-boot"
say() { echo "$*" >&$fhs; }

say '---------- GENERATED FILE, EDITS WILL BE LOST ----------'
say
say 'module Graphics.UI.Qtah.Internal.Interface.Listener where'
say
say 'import Foreign.Cppop.Generator.Spec (Class)'
say

writeHs() {
    local -r name="${1?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"

    say "${classVar} :: Class"
}
forEachListener writeHs

exec {fhs}>&-
unset fhs writeHs
