#!/bin/bash

set -e


declare -r -A componentBuildFunctions=(
    ['controller']='stdBuildComponent'
    ['data-model']='stdBuildComponent'
    ['obelisk']='stdBuildComponent'
    ['rpc']='stdBuildComponent'
    ['dialplan']='stdBuildComponent'
)

declare -r -a cabalInstallOnlyDependenciesOptions=(
    '--disable-documentation'
    '--enable-tests'
    '--enable-benchmarks'
)

declare -r -a cabalConfigureOptions=(
    '--enable-tests'
    '--enable-benchmarks'
    '--enable-executable-stripping'
    '--disable-shared'
    '--disable-executable-dynamic'
)

# Append $BUILD_NUMBER to package version.
updateVersionInCabalFile()
{
    local -r cabalFile="$1"; shift

    if [[ -z "$BUILD_NUMBER" ]]; then
        printf "Error: \`%s': %s\n       %s\n"           \
            'BUILD_NUMBER' 'Environment variable empty.' \
            'Was this script executed by Jenkins?' 1>&2
        exit 3
    fi

    sed -r --in-place=.orig "s/^([Vv]ersion: .*)$/\1.$BUILD_NUMBER/" \
        "$cabalFile"
}

restoreOriginalCabalFile()
{
    local -r cabalFile="$1"; shift

    if [[ -e "${cabalFile}.orig" ]]; then
        mv -f "${cabalFile}.orig" "$cabalFile"
    fi
}

stdBuildComponent()
{
    local -r root="$1"; shift

    local -i ret=0

    printf "\n\n*** Used tools ***"

    for tool in ghc cabal; do
        printf "\n\n%s:\n" "$tool"
        "$tool" --version | sed 's/^/  /'
    done

    set -ex


    prepareCabalSandbox

    # If there is more then one Cabal file, then cabal-install will fail,
    # therefore there is no need to check it here.
    updateVersionInCabalFile *.cabal

    stdCabalBuild "$root" *.cabal || ret=1

    # It is necessary to restore original cabal file so that Git will not
    # fail when pulling patches from remote repository.
    restoreOriginalCabalFile *.cabal

    # We want to rebuild everything from scratch so that any problems with
    # package version bounds are found as soon as possible.
    fullCabalClean

    exit $ret
}

prepareCabalSandbox()
{
    if [[ ! -f 'cabal.sandbox.config' ]]; then
        cabal sandbox init
    fi
    cabal install --jobs --only-dependencies \
        "${cabalInstallOnlyDependenciesOptions[@]}"
}

# Move package in to directory from which it will be archived by Jenkins. This
# i s so that we can use "cabal clean" without removing it. Older packages are
# removed first, so that Jenkins would not archive them too. Cabal file is
# archived Along with package tarball, so that it is possible to automate
# package distribution.
archivePackage()
{
    local -r root="$1"; shift
    local -r tarball="$1"; shift
    local -r cabalFile="$1"; shift

    local -r pkgDir="$root/out/package"

    if [[ -d "$pkgDir" ]]; then
        rm -f "$pkgDir"/*
    else
        mkdir -p "$pkgDir"
    fi
    mv "$tarball" "$pkgDir/"
    cp "$cabalFile" "$pkgDir/"
}

archiveExecutables()
{
    local -r root="$1"; shift
    local -r cabalFile="$1"; shift

    local -r archiveDir="$root/out/bin"
    local -a executables=()

    if [[ -d "$archiveDir" ]]; then
        rm -f "$archiveDir"/*
    else
        mkdir -p "$archiveDir"
    fi

    mapfile -t executables \
    < <(sed -nr '/^[Ee]xecutable /{s/^[^ ]+ +([^ ]+).*/\1/;p}' "$cabalFile")

    for exe in "${executables[@]}"; do
        mv "./dist/build/$exe/$exe" "$archiveDir"
    done
}

stdCabalBuild()
{
    local -r root="$1"; shift
    local -r cabalFile="$1"; shift

    cabal configure "${cabalConfigureOptions[@]}" && cabal build \
        && archiveExecutables "$root" "$cabalFile"
    cabal clean
    local -r tarball="$(cabal sdist | sed -n '${s/^[^:]*: *//; p}')"

    # This checks that distribution tarball is fully functional package.
    cabal configure
    cabal install "$tarball" && archivePackage "$root" "$tarball" "$cabalFile"
}

fullCabalClean()
{
    cabal clean
    cabal sandbox delete
}

printHelp()
{
    local progName="${0##*/}"

    cat << __EOF__
Obelisk build script intended for Jenkins CI server.

Usage:

  ${progName}
  ${progName} {-h|--help}

Options:

  -h, --help

    Print this help message.
__EOF__
}

build()
{
    printf "\n\n*** OS info ***\n\n"

    printf 'uname: %s\n\n' "$(uname -a)"
    lsb_release -a

    stdBuildComponent "`pwd`"
}

main()
{
    local arg=''

    while (( $# )); do
        arg="$1"; shift
        case "$arg" in
          -h|--help)
            printHelp
            exit 0
            ;;
          -b|--build)
            action='build'
            ;;
          *)
            printf "Error: \`%s': %s\n\n" \
                "$arg" 'Unknown option.' 1>&2
            printHelp 1>&2
            exit 1
            ;;
        esac
    done

    case "$action" in
      'build')
        build
        ;;
      *)
        printf "Error: \`%s': Unknown action, this is probably a bug.\n\n" \
            "$action" 1>&2
        exit 2
        ;;
    esac
}

main "$@"

# vim: softtabstop=4 shiftwidth=4 expandtab
