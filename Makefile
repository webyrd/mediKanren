
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

install_pkgs:
	-bash $(adirRepo)/medikanren/.install_pkgs.sh
	-bash $(adirRepo)/medikanren2/.install_pkgs.sh

docker_build:
	rm -rf $(adirRepo)/docker/medikanren-trapi/target
	mkdir -p $(adirRepo)/docker/medikanren-trapi/target
	(cd $(adirRepo) && find medikanren2 -not -path medikanren2/data/\* -type f | cpio -p --make-directories docker/medikanren-trapi/target/)
	cd $(adirRepo)/docker/medikanren-trapi && \
		docker build -t medikanren-trapi .
