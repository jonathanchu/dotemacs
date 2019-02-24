#!/bin/sh
set -e

usage () {
    echo "usage: sh ./bin/git-commit-elpa-updates.sh" >&2
    echo >&2
}

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

        # echo "git commit -m 'Update "$DELETED_PACKAGE_LONG" -> "$UNTRACKED_PACKAGE_LONG"'"
        # git commit -m "Update "$DELETED_PACKAGE_LONG" => "$UNTRACKED_PACKAGE_LONG""
        COMMIT_MSG="$COMMIT_MSG"$'\nUpdate '$""$DELETED_PACKAGE_LONG" => "$UNTRACKED_PACKAGE_LONG""
        # echo $COMMIT_MSG
    else
        echo "No action taken."
    fi
done

git commit -m "$COMMIT_MSG"

# Update the archive contents now
ELPA_ARCHIVES=$(ls_files_modified | grep -Ee 'elpa/archives/')

if [ "$ELPA_ARCHIVES" ]; then
    for x in $ELPA_ARCHIVES; do
        git add "$x"
    done

    git commit -m "Update ELPA package archive contents."
fi
