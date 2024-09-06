#
# ioq3 Makefile
#
# GNU Make required
#
COMPILE_PLATFORM=$(shell uname | sed -e 's/_.*//' | tr '[:upper:]' '[:lower:]' | sed -e 's/\//_/g')
COMPILE_ARCH=$(shell uname -m | sed -e 's/i.86/x86/' | sed -e 's/^arm.*/arm/')

#arm64 hack!
ifeq ($(shell uname -m), arm64)
  COMPILE_ARCH=arm64
endif
ifeq ($(shell uname -m), aarch64)
  COMPILE_ARCH=arm64
endif


ifndef BUILD_STANDALONE
  BUILD_STANDALONE =
endif
ifndef BUILD_CLIENT
  BUILD_CLIENT     =
endif
ifndef BUILD_SERVER
  BUILD_SERVER     =
endif
ifndef BUILD_GAME_SO
  BUILD_GAME_SO    =
endif
ifndef BUILD_BASEGAME
  BUILD_BASEGAME =
endif
ifndef BUILD_RENDERER_OPENGL2
  BUILD_RENDERER_OPENGL2=
endif
ifndef BUILD_AUTOUPDATER  # DON'T build unless you mean to!
  BUILD_AUTOUPDATER=0
endif


#############################################################################
#
# If you require a different configuration from the defaults below, create a
# new file named "Makefile.local" in the same directory as this file and define
# your parameters there. This allows you to change configuration without
# causing problems with keeping up to date with the repository.
#
#############################################################################
-include Makefile.local

ifeq ($(COMPILE_ARCH),i86pc)
  COMPILE_ARCH=x86
endif

ifeq ($(COMPILE_ARCH),amd64)
  COMPILE_ARCH=x86_64
endif
ifeq ($(COMPILE_ARCH),x64)
  COMPILE_ARCH=x86_64
endif

ifndef ARCH
ARCH=$(COMPILE_ARCH)
endif
export ARCH

ifneq ($(PLATFORM),$(COMPILE_PLATFORM))
  CROSS_COMPILING=1
else
  CROSS_COMPILING=0

  ifneq ($(ARCH),$(COMPILE_ARCH))
    CROSS_COMPILING=1
  endif
endif
export CROSS_COMPILING

ifndef VERSION
VERSION=0.1.0
endif

ifndef CLIENTBIN
CLIENTBIN=ringsce
endif

ifndef SERVERBIN
SERVERBIN=ringsceded
endif

ifndef BASEGAME
BASEGAME=base
endif

ifndef BASEGAME_CFLAGS
BASEGAME_CFLAGS=
endif

ifndef COPYDIR
COPYDIR="/usr/local/games/ringsce"
endif

ifndef COPYBINDIR
COPYBINDIR=$(COPYDIR)
endif

ifndef BUILD_DIR
BUILD_DIR=build
endif

ifndef TEMPDIR
TEMPDIR=/tmp
endif

ifndef GENERATE_DEPENDENCIES
GENERATE_DEPENDENCIES=1
endif



# The autoupdater uses curl, so figure out its flags no matter what.
# We won't need this if we only build the server

# set PKG_CONFIG_PATH or PKG_CONFIG to influence this, e.g.
# PKG_CONFIG_PATH=/opt/cross/i386-mingw32msvc/lib/pkgconfig or
# PKG_CONFIG=arm-linux-gnueabihf-pkg-config
ifeq ($(CROSS_COMPILING),0)
  PKG_CONFIG ?= pkg-config
else
ifneq ($(PKG_CONFIG_PATH),)
  PKG_CONFIG ?= pkg-config
else
  # Don't use host pkg-config when cross-compiling.
  # (unknown-pkg-config is meant to be a non-existant command.)
  PKG_CONFIG ?= unknown-pkg-config
endif
endif


all:
	cd gui && gcc `pkg-config --cflags gtk+-3.0` -o example-0 main.c `pkg-config --libs gtk+-3.0`

clean:
	rm  gui/example-0

