.PHONY: all unix clean
-include Makefile.config

JOBS ?= -j 4
OFLAGS ?= # -classic-display

MIR-DEBUG ?= 1
export MIR-DEBUG

OS = $(shell uname -s | tr '[A-Z]' '[a-z]' | sed -e 's/darwin/macosx/g')
ARCH = $(shell uname -m)
NODE = $(shell ocamlfind query js_of_ocaml 2>/dev/null)

ifeq ($(OS) $(ARCH),linux x86_64)
XEN_BUILD=xen
endif

UNIX_BUILD= unix-direct unix-socket

all: $(XEN_BUILD) $(UNIX_BUILD)
	@ :

doc:
	for spec in $(XEN_BUILD) $(UNIX_BUILD); do \
	  SPEC=$$spec ocamlbuild $(OFLOAGS) $(JOBS) doc.otarget; \
	done

unix-direct:
	@mkdir -p _build
	@env SPEC=unix-direct ocamlbuild $(OFLAGS) $(JOBS) unix.otarget

unix-socket:
	@mkdir -p _build
	@env SPEC=unix-socket ocamlbuild $(OFLAGS) $(JOBS) unix.otarget

xen:
	@mkdir -p _build
	@env SPEC=xen ocamlbuild $(OFLAGS) $(JOBS) xen.otarget

node:
	@mkdir -p _build
	@env SPEC=node ocamlbuild $(OFLAGS) $(JOBS) node.otarget

clean:
	@ocamlbuild -clean
