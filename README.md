# aws-switch-role-generator

This is a tool to generate a link of CloudFormation template for switching role.

## Example

- Admin Switch Role Stack: <https://us-east-1.console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks/create/review?&templateURL=https://arai-switch-role-temlate-buckt.s3.us-east-1.amazonaws.com/admin-switch-role.yaml&stackName=admin-switch-role-stack&param_XXXBucketURL=https://arai-switch-role-temlate-buckt.s3.us-east-1.amazonaws.com>
- Viewer Switch Role Stack: <https://us-east-1.console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks/create/review?&templateURL=https://arai-switch-role-temlate-buckt.s3.us-east-1.amazonaws.com/viewer-switch-role.yaml&stackName=viewer-switch-role-stack&param_XXXBucketURL=https://arai-switch-role-temlate-buckt.s3.us-east-1.amazonaws.com>

## Pre-requirements

- Haskell Stack

## Install Command

```bash
stack install
```
