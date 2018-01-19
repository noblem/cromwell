#!/usr/bin/env bash

set -e
set -x

docker pull ubuntu:latest

ENABLE_COVERAGE=true sbt \
  -Dbackend.providers.Local.config.filesystems.local.localization.0=copy \
  "++2.11.11" test:compile
