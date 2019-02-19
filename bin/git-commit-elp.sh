#!/bin/sh
set -e

# Get git root
# use git ls-files --exclude-standard --others
# Parse the package name and remove the date (ace-window)
# elpa/ace-window-20190205.1357 -> elpa/ace-window-20190210.1648
# Add all untracked directories/files for "elpa/<package-name>"
# Stage the deleted directories/files for "elpa/<package-name>"
# Commit "Update ace-window-20190205.1357 -> ace-window-20190210.1648"
# Repeat!

ls_files () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root --exclude-standard --others
}

ls_files1 () {
    root="$(git rev-parse --show-toplevel)"
    git ls-files $root -d
}

# GIT_ROOT=$(git rev-parse --show-toplevel)
# echo $GIT_ROOT
# LC=$(git status | wc -l)
# echo $LC

# Get the deleted files
# ls_files1 | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u

# Get the files not tracked
# ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u

NUMBER_OF_PACKAGES=$(ls_files | grep -Eo 'elpa/[^/]+/' | sort -u | sed 's/\(.*\)-.*/\1/')
NOT_TRACKED=$(ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u)
DELETED=$(ls_files1 | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u)

for i in $NUMBER_OF_PACKAGES; do
    # Get the first package off the list
    echo $i
    UNTRACKED_PACKAGE=$(ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1 | sed 's/\(.*\)-.*/\1/')
    DELETED_PACKAGE=$(ls_files1 | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | head -n 1 | sed 's/\(.*\)-.*/\1/')

    # ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u
    # ls_files | grep -Eo 'elpa/[^/]+/' | cut -c 6- | rev | cut -c 2- | rev | sort -u | wc -l
    echo $UNTRACKED_PACKAGE
    echo $DELETED_PACKAGE

    if [ "$UNTRACKED_PACKAGE" = "$DELETED_PACKAGE" ]; then
        echo "YAY"
        # git add ...
    else
        echo "No action taken"
    fi
done
