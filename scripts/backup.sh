#!/usr/bin/env bash

rsync -ahzP \
      --exclude="node_modules" \
      --exclude=".cpcache"\
      --exclude=".eggs" \
      --exclude="__pycache__" \
      --exclude="*.egg-info$" \
      --exclude=".vscode" \
      --exclude="target" \
      --exclude=".m2" \
      --exclude="go" \
      --exclude="elpa" \
      --exclude="var" \
      --exclude=".cache" \
      --exclude="eclipse.jdt.ls" \
      --exclude="eln-cache" \
      --exclude="workspace" \
      --delete \
      --ignore-existing \
      /home/wanderson/arch/ /run/media/wanderson/Backup/28_02_2021_rsync_backup
