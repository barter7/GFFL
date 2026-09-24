#!/usr/bin/env bash
# Vercel "Ignored Build Step" (vercel.json ignoreCommand).
# Exit 0 = skip the build, exit 1 = build.
#
# Vercel bills build CPU minutes, so a build runs only when it can change
# what the site serves:
#   - only the production branch (main) builds; Claude/mirror branch
#     pushes don't need preview deployments
#   - only when a commit since the last successful deployment touched a
#     file that ships (pipeline scripts, raw data and workflow files don't)
#   - an explicit redeploy of the same commit always builds
set -u

# paths whose changes never reach the deployed site
EXCLUDES=(
  ':(exclude,glob).github/**'
  ':(exclude,glob)data/**'
  ':(exclude,glob)scripts/**'
  ':(exclude,glob)www/**'
  ':(exclude,glob)**/*.R'
  ':(exclude,glob)*.Rproj'
  ':(exclude,glob)README.md'
)

ref="${VERCEL_GIT_COMMIT_REF:-}"
if [ "$ref" != "main" ]; then
  echo "Skip: '$ref' is not the production branch."
  exit 0
fi

base="${VERCEL_GIT_PREVIOUS_SHA:-}"
head="$(git rev-parse HEAD)"
if [ -z "$base" ] || [ "$base" = "$head" ]; then
  echo "Build: first deploy or explicit redeploy."
  exit 1
fi
if ! git cat-file -e "${base}^{commit}" 2>/dev/null; then
  echo "Build: last deployed commit ${base:0:7} isn't in the shallow clone."
  exit 1
fi
if git diff --quiet "$base" HEAD -- . "${EXCLUDES[@]}"; then
  echo "Skip: nothing that ships changed since ${base:0:7}."
  exit 0
fi
echo "Build: site files changed since ${base:0:7}."
exit 1
