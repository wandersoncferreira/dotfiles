#!/bin/env bash

echo "Install packages and Repositories"
sudo ./packages.sh

echo "Start configuration"
./config.sh
