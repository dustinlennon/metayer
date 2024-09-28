#!/bin/bash
VERSION=$1
TAG=v${VERSION}

if [ -z "$VERSION" ]; then 
    echo syntax error: no version specified
    exit 1
fi

has_tag=$(sh -c "git tag --list | grep -cm1 $TAG 2>&1")
if [ "$has_tag" -eq 0 ] ; then
  echo "Error: tag $TAG is not available"
  exit 1
fi

# compute an md5 hash for docs/*
touch /tmp/old_hash
docs_hash() {
  sh -c 'find ./docs -type f -not -path "./docs/news/*" | sort | xargs -I {} cat {} | md5sum' | awk -e '{print $1}'
}
docs_new_hash=$(docs_hash)
docs_old_hash=$(cat /tmp/old_hash || echo none)

# Hopefully we can avoid rebuilding the entire site, but always rebuild the news
if [ "$docs_new_hash" != "$docs_old_hash" ]; then
  echo "Building site"
  Rscript -e "build_site()"
  
  echo "$(docs_hash)" > /tmp/old_hash
fi
  
if [ "$(git status --porcelain)" ]; then 
  echo "error: branch is not clean"
  exit 1
fi

echo "Pushing origin main"
git push origin main

echo "Packaging website"
sh -c "find ./docs -type d | xargs chmod 775" && \
  tar -czf docs.tgz docs/*

echo "Deploying website"
REMOTE_DIR=/home/deploy/metayer/tag-${TAG}
scp docs.tgz 192.168.1.102:/home/dustin/docs.tgz &&
  ssh dustin@192.168.1.102 "mkdir -p $REMOTE_DIR; tar -zxf docs.tgz -C $REMOTE_DIR"


