Caterpillar
================
*package building tool set
Abstract
----------------
Caterpillar is distributed build tool which provide abilities to assemble packages for various environments.
It consists on centralized services (repository, notify, storage, api, deploy) and several builders.
Currently caterpillar supports compile, test, build, deploy .deb packages, monitor build progress for 3-step makefiles or build scripts (clean, test, package).
Every package is described by file named pkg.config stored in / of repository. It helps builders to handle dependencies, paralellize jobs and form eg. control file
for debian packages (the example Makefile and pkg.config are here ./, caterpillar builds himself).
