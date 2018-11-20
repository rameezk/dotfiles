#!/usr/bin/bash

# Colour constants for nicer output.
GREEN='\033[0;32m'
RESET='\033[0m'

# Push the current branch to origin, set upstream, open the PR page if possible.
gpr() {
    # Get the current branch name, or use 'HEAD' if we cannot get it.
    branch=$(git symbolic-ref -q HEAD)
    branch=${branch##refs/heads/}
    branch=${branch:-HEAD}

    # Pushing take a little while, so let the user know we're working.
    echo "Opening pull request for ${GREEN}${branch}${RESET}..."

    # Push to origin, grabbing the output but then echoing it back.
    push_output=`git push origin -u ${branch} 2>&1`
    echo ""
    echo ${push_output}

    # If there's anything which starts with http, it's a good guess it'll be a
    # link to GitHub/GitLab/Whatever. So open it.
    link=$(echo ${push_output} | grep -o 'http.*' | sed -e 's/[[:space:]]*$//')
    if [ ${link} ]; then
        echo ""
        echo "Opening: ${GREEN}${link}${RESET}..."
        python -mwebbrowser ${link}
    fi
}
