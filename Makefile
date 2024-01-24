PROJECT_NAME=mini_comp

default: release

release:
	dune exec $(PROJECT_NAME) --profile=release

run:
	dune exec $(PROJECT_NAME)

build:
	dune build

doc:
	dune build @doc
