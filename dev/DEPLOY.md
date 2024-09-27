
```bash
# initially, remotely: sudo chmod -R g+s ~deploy/metayer

# locally
find ./docs -type d | xargs chmod 775
tar -czf docs.tgz docs/*
scp docs.tgz 192.168.1.102:/home/dustin/docs.tgz

TAG=$(sh -c 'git tag --list | tail -n 1') ssh dustin@192.168.1.102 "tar -zxf docs.tgz -C /home/deploy/metayer/tag-${TAG}"
```