#!/usr/bin/env bash

# This file is part of Qtah.
#
# Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

set -euo pipefail

msg() {
    echo "listener-gen.sh: $*"
}

help() {
    cat <<EOF
listener-gen.sh: Generates listener classes for wrapping Qt signals.

Options:
  --help              : Display this menu.
  --gen-cpp-dir <dir> : Generates C++ listener classes in listener.cpp and
                        listener.hpp in the given directory.
  --gen-hs-dir <dir>  : Generates bindings to be linked into a Hoppy generator
                        in the Haskell project rooted at the given directory.

At least one of --gen-cpp-dir or --gen-hs-dir is required.
EOF
}

cppDir=
hsDir=

while [[ $# -ne 0 ]]; do
    case "$1" in
        --help) help; exit 0;;
        --gen-cpp-dir) shift; cppDir="${1:?--gen-cpp-dir requires an argument.}";;
        --gen-hs-dir) shift; hsDir="${1:?--gen-hs-dir requires an argument.}";;
        *) msg "Unrecognized argument: $1"; exit 1;;
    esac
    shift
done

if [[ -z $cppDir ]] && [[ -z $hsDir ]]; then
    msg "At least one of --gen-cpp-dir or --gen-hs-dir is required.  See --help."
    exit 1
fi

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
    $fn Double "double"
    $fn Int "int"
    $fn IntBool "int|bool"
    $fn IntInt "int|int"
    $fn PtrQAbstractButton "QAbstractButton*"
    $fn PtrQAbstractButtonBool "QAbstractButton*|bool"
    $fn PtrQAction "QAction*"
    $fn PtrQObject "QObject*"
    $fn PtrQWidgetPtrQWidget "QWidget*|QWidget*"
    $fn QAbstractSliderAction "QAbstractSlider::SliderAction"
    $fn QClipboardMode "QClipboard::Mode"
    $fn QPoint "QPoint"
    $fn QSize "QSize"
    $fn QString "QString"
    $fn "" ""
}

#### Generate C++ listener classes.

if [[ -n $cppDir ]]; then
    msg "Generating C++ listener classes."
    exec \
        {fhpp}>"$cppDir/listener.hpp.new" \
        {fcpp}>"$cppDir/listener.cpp.new"
    sayHpp() { echo "$*" >&$fhpp; }
    sayCpp() { echo "$*" >&$fcpp; }

    sayHpp '////////// GENERATED FILE, EDITS WILL BE LOST //////////'
    sayHpp
    sayHpp '#ifndef QTAH_LISTENERS_HPP'
    sayHpp '#define QTAH_LISTENERS_HPP'
    sayHpp
    sayHpp '#include <string>'
    sayHpp '#include <QAbstractButton>'
    sayHpp '#include <QAbstractSlider>'
    sayHpp '#include <QAction>'
    sayHpp '#include <QClipboard>'
    sayHpp '#include <QObject>'
    sayHpp '#include <QPoint>'
    sayHpp '#include <QSize>'
    sayHpp '#include <QWidget>'
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
        sayCpp "    connected_ = connect(source, signal.c_str(), SLOT(invoke(${paramTypeList})));"
        sayCpp "    return connected_;"
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
    installGen "$cppDir/listener.hpp"{.new,}
    installGen "$cppDir/listener.cpp"{.new,}
fi

#### Generate Haskell binding definitions for the listeners.

if [[ -n $hsDir ]]; then
    msg "Generating Haskell listener binding definitions."
    exec {fhs}>"$hsDir/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs.new"
    say() { echo "$*" >&$fhs; }

    say '---------- GENERATED FILE, EDITS WILL BE LOST ----------'
    say
    say 'module Graphics.UI.Qtah.Internal.Interface.Listener where'
    say
    say 'import qualified Foreign.Hoppy.Generator.Spec as S'
    say 'import qualified Foreign.Hoppy.Generator.Types as S'
    say 'import qualified Foreign.Hoppy.Generator.Std.String as String'
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
        say "  [ S.mkCtor \"new\" [S.callbackT C.${callbackVar}]"
        say "  , S.mkCtor \"newWithParent\""
        say "    [S.callbackT C.${callbackVar}, S.ptrT \$ S.objT QObject.c_QObject]"
        say "  ]"
        say "  [ S.mkMethod \"connectListener\""
        say "    [S.ptrT \$ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT"
        say "  ]"
    }
    forEachListener writeHs

    say
    say "aModule :: T.AModule"
    say "aModule ="
    say "  T.AHoppyModule \$"
    say "  S.addReqIncludes [S.includeLocal \"listener.hpp\"] \$"
    say "  S.moduleModify' (S.makeModule \"listener\" \"b_listener.hpp\" \"b_listener.cpp\") \$"
    say "  S.moduleAddExports"
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

    exec {fhs}>&-
    unset fhs writeHs
    installGen "$hsDir/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs"{.new,}

    # Also generate a GHC .hs-boot file for cycles in the module graph around listeners.

    msg "Generating Haskell listener .hs-boot file."
    exec {fhs}>"$hsDir/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs-boot.new"
    say() { echo "$*" >&$fhs; }

    say '---------- GENERATED FILE, EDITS WILL BE LOST ----------'
    say
    say 'module Graphics.UI.Qtah.Internal.Interface.Listener where'
    say
    say 'import Foreign.Hoppy.Generator.Spec (Class)'
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
    installGen "$hsDir/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs-boot"{.new,}
fi
