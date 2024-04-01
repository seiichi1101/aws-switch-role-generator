include .env
export

run:
	stack run

build:
	stack build

install:
	stack install

upload-templates:
	aws s3 cp ./template/admin-switch-role.yaml s3://$${TEMPLATE_STORING_BUCKET}/
	aws s3 cp ./template/viewer-switch-role.yaml s3://$${TEMPLATE_STORING_BUCKET}/
	aws s3 cp ./template/account-checker.yaml s3://$${TEMPLATE_STORING_BUCKET}/
