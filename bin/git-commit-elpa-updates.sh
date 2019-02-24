#!/bin/sh
set -e

usage () {
    echo "usage: sh ./bin/git-commit-elpa-updates.sh" >&2
    echo >&2
    echo "Given the assumption that third-party packages from ELPA are updated locally, "
    echo "commit all changes and write the appropriate commit message."
    echo >&2
    echo "This will create an individual commit per package with a commit message like:"
    echo "git commit -m 'Update evil-20190104.1026 => evil-20190222.1212'"
    echo >&2
    echo "If the batch (-b) option is passed in, it will create a single commit for ALL "
    echo "packages and explicitly write each package upgrade in the long commit message."
    echo >&2
    echo "Options:" >&2
    echo "-b    Batch all the commits" >&2
    echo "-h    Show this help" >&2
}

batch=0
while getopts bh flag; do
    case "$flag" in
        b) batch=1;;
        h) usage; exit 2;;
    esac
done
shift $(($OPTIND -1))

ls_files_untracked () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root --exclude-standard --others
}

ls_files_deleted () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root -d
}

ls_files_modified () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root -m
}

PACKAGES=$(ls_files_untracked | grep -Eo 'elpa/[^/]+/' | sort -u | sed 's/\(.*\)-.*/\1/')
COMMIT_MSG=$'Update packages\n'

for i in $PACKAGES; do
    UNTRACKED_PACKAGE_SHORT=$(ls_files_untracked | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1 | sed 's/\(.*\)-.*/\1/')
    UNTRACKED_PACKAGE_LONG=$(ls_files_untracked | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1)
    UNTRACKED_PACKAGE_DIRECTORY=$(ls_files_untracked | grep -Ee "$i")

    DELETED_PACKAGE_SHORT=$(ls_files_deleted | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1 | sed 's/\(.*\)-.*/\1/')
    DELETED_PACKAGE_LONG=$(ls_files_deleted | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1)
    DELETED_PACKAGE_DIRECTORY=$(ls_files_deleted | grep -Ee "$i")

    if [ "$UNTRACKED_PACKAGE_SHORT" = "$DELETED_PACKAGE_SHORT" ]; then
        for x in $UNTRACKED_PACKAGE_DIRECTORY; do
            # echo "git add "$x""
            git add "$x"
        done

        echo '--------------------------------------------------------------------------'
        for y in $DELETED_PACKAGE_DIRECTORY; do
            # echo "git add "$y""
            git add "$y"
        done

        if [ $batch -eq 1 ]; then
            COMMIT_MSG="$COMMIT_MSG"$'\nUpdate '$""$DELETED_PACKAGE_LONG" => "$UNTRACKED_PACKAGE_LONG""
            # echo $COMMIT_MSG
        else
            # echo "git commit -m 'Update "$DELETED_PACKAGE_LONG" => "$UNTRACKED_PACKAGE_LONG"'"
            git commit -m "Update "$DELETED_PACKAGE_LONG" => "$UNTRACKED_PACKAGE_LONG""
        fi
    else
        echo "No action taken."
    fi
done

# Update the archive contents too
ELPA_ARCHIVES=$(ls_files_modified | grep -Ee 'elpa/archives/')

if [ "$ELPA_ARCHIVES" ]; then
    for x in $ELPA_ARCHIVES; do
        git add "$x"
    done

    if [ $batch -eq 1 ]; then
        COMMIT_MSG="$COMMIT_MSG"$"\nUpdate ELPA package archive contents"
    else
        git commit -m "Update ELPA package archive contents."
    fi
fi

# Finally, if the batch option (-b) is passed in, make one big finally commit at the end
if [ $batch -eq 1 ]; then
    git commit -m "$COMMIT_MSG"
fi
