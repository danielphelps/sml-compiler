SHELL=/usr/bin/env bash
ENTRY_POINT=MyLang.main
IMAGE_NAME=mylang-compiler-img
ML_BUILD=ml-build
ML_BUILD_OPTS=sources.cm

#ml-build will determine if the heap image is up to date.
mylang-img:
	$(ML_BUILD) $(ML_BUILD_OPTS) $(ENTRY_POINT) $(IMAGE_NAME)

tags: 
	etags -R *.sml *.grm *.lex -o tags

check: mylang-img
	@echo TODO: a script that will run and report regression tests

clean: 
	rm -rf $(IMAGE_NAME)* .cm/ mylang.grm.* mylang.lex.sml
