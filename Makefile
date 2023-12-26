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
packages: core site

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
