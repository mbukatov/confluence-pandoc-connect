# You will need to extend this if your cabal build depends on non
# haskell files (here '.lhs' and '.hs' files).
SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')
NAME="confluence-pandoc-connect"

.PHONY: clean build

sandbox-init:
			cabal sandbox init
			cabal sandbox add-source submodule/atlassian-connect-descriptor

dev: $(SOURCE) sandbox-init
			cabal install -f development

setup: sandbox-init
			cabal install --only-dependencies --enable-tests

setup-verbose-install: sandbox-init
			cabal install --only-dependencies --enable-tests -v

compile:
			cabal build

build: clean
			cabal build

install: setup
			cabal install

dist: build
			@(strip `find dist/build/${NAME} -type f -maxdepth 1`)
			@(upx `find dist/build/${NAME} -type f -maxdepth 1`)

clean:
			cabal clean

test-compile:
			cabal clean
			cabal configure --enable-tests
			cabal build

test:
			cabal configure --enable-tests
			cabal test --show-details=always

doctest:
			@(doctest -XOverloadedStrings $(SOURCE))


# Ensure `cabal-constraints` is installed as per:
# https://github.com/benarmston/cabal-constraints
freeze: build
			cabal install
			@(cabal-constraints dist/dist-sandbox-*/setup-config > cabal.config)
