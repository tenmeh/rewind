#!/usr/bin/env bash
# Find the commit on main that a CRAN tarball was built from.
#
# Usage: find-release-commit.sh <package> <version> <tarball>
#
# Prints the full hash of the oldest commit on the first-parent history of
# origin/main that has <version> in DESCRIPTION, and whose files are the
# same as the files in <tarball>. Exits with status 1 if no commit matches.
#
# Why not "the commit that set the version": for 0.2.0 the version was set
# first, and two fixes were merged before the submission. Why not "the last
# commit with the version": a fix can be merged after the submission, before
# the version moves on. The files that CRAN published decide it.
#
# Files that R CMD build makes or changes are not compared: DESCRIPTION (CRAN
# adds fields to it), MD5, and the folder build/. A file that is in the
# tarball but not in a commit is a difference, so a commit from before the
# file was added does not match.
set -euo pipefail

pkg="$1"
version="$2"
tarball="$3"

dir=$(mktemp -d)
tar -xzf "$tarball" -C "$dir"
cd "$dir/$pkg"
files=$(find . -type f ! -name DESCRIPTION ! -name MD5 ! -path './build/*' | sed 's#^\./##' | sort)
cd - > /dev/null

for sha in $(git rev-list --first-parent --reverse origin/main); do
  v=$(git show "$sha:DESCRIPTION" 2> /dev/null | sed -n 's/^Version: *//p')
  [ "$v" = "$version" ] || continue
  same=true
  for f in $files; do
    # The CR strip: a file built on Windows can have CRLF line
    # endings, while git holds LF.
    if ! cmp -s <(git show "$sha:$f" 2> /dev/null | tr -d '\r') <(tr -d '\r' < "$dir/$pkg/$f"); then
      same=false
      break
    fi
  done
  if [ "$same" = true ]; then
    echo "$sha"
    exit 0
  fi
done

echo "No commit on main matches $pkg $version as CRAN published it." >&2
exit 1
