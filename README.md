# flea

A quick'n'dirty [bullet](https://github.com/extend/bullet) for Yaws.

## Clock example

To run the orignial Bullet example, enter the following commands :

	git clone git@github.com:lud/flea.git
	cd flea/examples/clock
	make
	bin/start

And open [http://localhost:8000/](http://localhost:8000/) in your
browser.

## Chat example

	git clone git@github.com:lud/flea.git
	cd flea/examples/chat
	make
	bin/start

And open [http://localhost:8000/](http://localhost:8000/) in **two**
browsers.

To run the two examples, yaws beam files must be in erlang's search
path. If not, you can add it as a dep in examples/*/rebar.config.

Ah, and of course you need rebar in your path :)
