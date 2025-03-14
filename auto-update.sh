#!/bin/bash
#set -e

# checking if I have the latest files from github
git pull

# Below command will backup everything inside the project folder
git add --all .

# Give a comment to the commit if you want

echo "\n####    Commit message    ####"


# Committing to the local repository with a message containing the time details and commit text

#read input
#git commit -m "$input"
git commit -m auto-update

# Push the local files to github

if grep -q main .git/config; then
	echo "Using main"
		git push -u origin main
fi

if grep -q master .git/config; then
	echo "Using master"
		git push -u origin master
fi
