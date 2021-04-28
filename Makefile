
adirRepo := $(shell cd $(dir $(firstword $(MAKEFILE_LIST))) && pwd)
# https://stackoverflow.com/questions/322936/common-gnu-makefile-directory-path

default:
	@echo ***available targets***: prepare_ci, run_ci

prepare_ci:
	@echo about to setup_artifacts
	@mkdir -p $(adirRepo)/ci_artifacts
	@rm -rf $(adirRepo)/ci_artifacts/status
	@mkdir -p $(adirRepo)/ci_artifacts/status/pass
	@touch $(adirRepo)/ci_artifacts/status/pass/empty
	@mkdir -p $(adirRepo)/ci_artifacts/status/fail
	@touch $(adirRepo)/ci_artifacts/status/fail/empty
	@echo finished setup_artifacts

run_ci: prepare_ci
	-bash $(adirRepo)/medikanren/.run_ci.sh
	-bash $(adirRepo)/medikanren2/.run_ci.sh


