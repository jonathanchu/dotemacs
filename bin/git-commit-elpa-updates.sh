#!/bin/sh
set -e

ls_files () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root --exclude-standard --others
}

ls_files1 () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root -d
}

PACKAGES=$(ls_files | grep -Eo 'elpa/[^/]+/' | sort -u | sed 's/\(.*\)-.*/\1/')

for i in $PACKAGES; do
    UNTRACKED_PACKAGE_SHORT=$(ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1 | sed 's/\(.*\)-.*/\1/')
    UNTRACKED_PACKAGE_LONG=$(ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1)
    UNTRACKED_PACKAGE_DIRECTORY=$(ls_files | grep -Ee "$i")

    DELETED_PACKAGE_SHORT=$(ls_files1 | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1 | sed 's/\(.*\)-.*/\1/')
    DELETED_PACKAGE_LONG=$(ls_files1 | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1)
    DELETED_PACKAGE_DIRECTORY=$(ls_files1 | grep -Ee "$i")

    if [ "$UNTRACKED_PACKAGE_SHORT" = "$DELETED_PACKAGE_SHORT" ]; then
        for x in $UNTRACKED_PACKAGE_DIRECTORY; do
            # echo "git add "$x""
            git add "$x"
        done

        echo '--------------------------------------------'
        for y in $DELETED_PACKAGE_DIRECTORY; do
            # echo "git add "$y""
            git add "$y"
        done

        # echo "git commit -m 'Update "$DELETED_PACKAGE_LONG" -> "$UNTRACKED_PACKAGE_LONG"'"
        git commit -m "Update "$DELETED_PACKAGE_LONG" -> "$UNTRACKED_PACKAGE_LONG""
    else
        echo "No action taken"
    fi
done
