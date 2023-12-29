# build everything
build: packages

# run all tests
test: test-js

# run other checks
check: prettier

# delete build artifacts, but not dependencies or downloaded files
clean:
	git clean -Xdf packages -e '!node_modules'

# do everything
all: build test check

# fetch JavaScript dependencies
npm:
	npm i

# check Prettier formatting
prettier: npm
	npx prettier --check .

# build `packages/`
packages: core site vscode

# run JavaScript tests
test-js: test-core test-site

## `packages/core`

# build
core: npm
	cp README.md packages/core
	npm run --workspace=rose build

# test
test-core: npm
	npm run --workspace=rose test -- run --no-threads

## `packages/site`

site-deps: npm core

# build
site: site-deps
	npm run --workspace=@rose-lang/site build

# test
test-site: site-deps
	npm run --workspace=@rose-lang/site test -- run --no-threads

## `packages/vscode`

# fetch encircled icon
packages/vscode/encircled-rose.png:
	node fetch.js https://github.com/rose-lang/rose-icons/raw/efcc218832d65970a47bed597ee11cecd3d1cc3c/png/encircled-rose.png $@

# fetch plain icon
packages/vscode/plain-rose.png:
	node fetch.js https://github.com/rose-lang/rose-icons/raw/efcc218832d65970a47bed597ee11cecd3d1cc3c/png/plain-rose.png $@

# build
vscode: npm packages/vscode/encircled-rose.png packages/vscode/plain-rose.png
	npm run --workspace=rose-vscode build
