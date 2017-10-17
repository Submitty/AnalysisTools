#!/bin/sh

VERSION=$1
BINARIES="count plagiarism diagnostics"

if [ -z $VERSION ]
    then VERSION="latest"
fi

echo "Building analysis tools version $VERSION..."

if [ $VERSION = "latest" ]
then stack --allow-different-user --install-ghc --copy-bins build
else for b in $BINARIES;
        do if [ -e $b ]
            then echo "File exists, no need to pull release"
            else wget "https://github.com/Submitty/AnalysisTools/releases/download/$VERSION/$b"
        fi;
    done
fi
