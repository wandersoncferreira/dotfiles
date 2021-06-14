#!/usr/bin/env bash

# 1. Download pgFormatter Release
wget https://github.com/darold/pgFormatter/archive/refs/tags/v5.0.zip

# 2. Unzip
unzip v5.0.zip

# 3. Install Perl Dependencies
sudo dnf install perl-ExtUtils-MakeMaker perl-autodie perl-open perl-FindBin

# 4. Create make file
cd pgFormatter-5.0/
perl Makefile.PL

# 5. Install
make
sudo make install

# Clean files
cd ..
rm -rf v5.0.zip pgFormatter-5.0
