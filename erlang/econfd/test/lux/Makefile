include $(TEST_DIR)/support/lux_testarea.mk

#Add your own  build commands if needed
build:
	$(MAKE) build_testarea
.PHONY: build


clean:
	$(MAKE) clean_testarea
.PHONY: clean

test:
	$(MAKE) build
	$(MAKE) test_testarea
.PHONY: test

desc:
	@$(MAKE) lux_desc --no-print-directory
.PHONY: desc

