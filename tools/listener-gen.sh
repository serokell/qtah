#!/usr/bin/env bash

set -euo pipefail
declare -r projectDir="$(dirname "$(dirname "$(readlink -f "$0")")")"
. "$projectDir/common.sh"

installGen() {
    local src="${1:?}" dest="${2:?}"
    if cmp -s "$src" "$dest"; then
        rm "$src"
    else
        mv "$src" "$dest"
    fi
}

forEachListener() {
    local fn="${1:?forEachListener requires the name of a function to call.}"

    # Keep the includes in the C++ section up-to-date with the types used here.
    $fn Bool "bool"
    $fn IntInt "int|int"
    $fn PtrQAction "QAction*"
    $fn PtrQObject "QObject*"
    $fn PtrQWidgetPtrQWidget "QWidget*|QWidget*"
    $fn QPoint "QPoint"
    $fn QSize "QSize"
    $fn QString "QString"
    $fn "" ""
}

#### Generate C++ listener classes.

echo
msg "Generating C++ listener classes."
exec \
    {fhpp}>"$projectDir/qtah/cpp/listener.hpp.new" \
    {fcpp}>"$projectDir/qtah/cpp/listener.cpp.new"
sayHpp() { echo "$*" >&$fhpp; }
sayCpp() { echo "$*" >&$fcpp; }

sayHpp '////////// GENERATED FILE, EDITS WILL BE LOST //////////'
sayHpp
sayHpp '#ifndef QTAH_LISTENERS_HPP'
sayHpp '#define QTAH_LISTENERS_HPP'
sayHpp
sayHpp '#include <string>'
sayHpp '#include <QAction>'
sayHpp '#include <QObject>'
sayHpp '#include <QPoint>'
sayHpp '#include <QSize>'
sayHpp '#include "b_callback.hpp"'

sayCpp '////////// GENERATED FILE, EDITS WILL BE LOST //////////'
sayCpp
sayCpp '#include "listener.hpp"'
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
installGen "$projectDir/qtah/cpp/listener.hpp"{.new,}
installGen "$projectDir/qtah/cpp/listener.cpp"{.new,}

#### Generate Haskell binding definitions for the listeners.

echo
msg "Generating Haskell listener binding definitions."
exec {fhs}>"$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs.new"
say() { echo "$*" >&$fhs; }

say '---------- GENERATED FILE, EDITS WILL BE LOST ----------'
say
say 'module Graphics.UI.Qtah.Internal.Interface.Listener where'
say
say 'import qualified Foreign.Cppop.Generator.Spec as S'
say 'import qualified Foreign.Cppop.Generator.Std.String as String'
say 'import qualified Graphics.UI.Qtah.Internal.Generator.Types as T'
say 'import qualified Graphics.UI.Qtah.Internal.Interface.Callback as C'
say 'import qualified Graphics.UI.Qtah.Internal.Interface.Core.QObject as QObject'
say
say '{-# ANN module "HLint: ignore Use camelCase" #-}'

writeHs() {
    local -r name="${1?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"
    local -r callbackVar="cb_${name}Void"

    say
    say "${classVar} ="
    say "  S.makeClass (S.ident \"${className}\") Nothing [QObject.c_QObject]"
    say "  [ S.mkCtor \"new\" [S.TCallback C.${callbackVar}]"
    say "  , S.mkCtor \"newWithParent\""
    say "    [S.TCallback C.${callbackVar}, S.TPtr \$ S.TObj QObject.c_QObject]"
    say "  ]"
    say "  [ S.mkMethod \"connectListener\""
    say "    [S.TPtr \$ S.TObj QObject.c_QObject, S.TObj String.c_string] S.TBool"
    say "  ]"
}
forEachListener writeHs

say
say "mod_Listener :: S.Module"
say "mod_Listener ="
say "  S.addReqIncludes [S.includeLocal \"listener.hpp\"] \$"
say "  S.modifyModule' (S.makeModule \"listener\" \"b_listener.hpp\" \"b_listener.cpp\") \$"
say "  S.addModuleExports"
cont="["
writeHs() {
    local -r name="${1?}"
    local -r className="Listener${name}"
    local -r classVar="c_${className}"

    say "  ${cont} S.ExportClass ${classVar}"
    if [[ $cont = '[' ]]; then cont=','; fi
}
forEachListener writeHs
say "  ]"
say
say "qmods_Listener :: [T.QtModule]"
say "qmods_Listener = []"

exec {fhs}>&-
unset fhs writeHs
installGen "$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs"{.new,}

#### Generate a GHC .hs-boot file for cycles in the module graph around listeners.

echo
msg "Generating Haskell listener .hs-boot file."
exec {fhs}>"$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs-boot.new"
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
installGen "$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs-boot"{.new,}
