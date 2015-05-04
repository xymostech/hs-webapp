.PHONY: server build test install

install:
	npm install

server: install
	./node_modules/.bin/webpack-dev-server

build: install
	./node_modules/.bin/webpack

test: install
	cd js && find ./ -type d -name "__tests__" \
		| xargs -I"%" find "%" -type f \
		| xargs ../node_modules/.bin/babel-node test.js
