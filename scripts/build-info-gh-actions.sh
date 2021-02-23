#!/usr/bin/env bash

build-info-json() {
    cat <<EOF
{
  "date": "$(git log -1 --format=%cd)",
  "sha": "$(git log -1 --format=%H)",
  "gh_actor": "$GITHUB_ACTOR",
  "gh_repository": "$GITHUB_REPOSITORY",
  "gh_event_name": "$GITHUB_EVENT_NAME",
  "gh_run_id": "$GITHUB_RUN_ID",
  "gh_run_number": "$GITHUB_RUN_NUMBER",
  "gh_sha": "$GITHUB_SHA",
  "gh_ref": "$GITHUB_REF"
}
EOF
}

main() {
    declare -r rootDir="$1"
    declare -r output="$2"
    declare -r buildInfo="$(build-info-json)"

    echo "$buildInfo"
    echo "$buildInfo" > "$rootDir/$output"
}

main "$@"
