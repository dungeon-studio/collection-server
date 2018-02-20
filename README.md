# Description

Static Resource Server for application/vnd.collection+json

Static resource server that serves application/vnd.collection+json resources
from a directory of yaml files.

# Getting Started

## Running

Given a directory of `application/collection+json` items encoded as yaml
(`${RESOURCES}`), we can use the following command to start collection server
directly:

```bash
docker run --publish=80 --env=COLLECTION_SERVER_RESOURCE_PATH=/resource --volume=${RESOURCES}:/srv quay.io/dungeon.studio/collection-server
```

## Base Docker Image

Two things are needed when building a derivative Dockerfile:

1. specify a location of the resource directory to serve
2. populate the specified directory with resource definitions

Example Dockerfile:

```Dockerfile
FROM quay.io/dungeon.studio/collection-server

ENV COLLECTION_SERVER_RESOURCE_PATH /resources

COPY resources /resources
```

# Reporting Issues

Any issues discovered should be recorded on [github][issues].  If you believe
you've found an error or have a suggestion for a new feature; please, ensure
that it is reported.

If you would like to contribute a fix or new feature; please, submit a pull
request.  This project follows [git flow] and utilizes [travis] to automatically
check pull requests before a manual review.

# Contributors

More information about contributing can be found in the `CONTRIBUTING.md` file.

[docker-compose]: https://docs.docker.com/compose/
[docker]: https://docs.docker.com/
[issues]: https://github.com/alunduil/collection-server/issues
