#!/usr/bin/env bash

if [ -z "$AWS_ACCESS_KEY_ID" ]; then
  echo ">>> AWS_ACCESS_KEY_ID is not configured; please export it."
  exit 1
fi

if [ -z "$AWS_SECRET_ACCESS_KEY" ]; then
  echo ">>> AWS_SECRET_ACCESS_KEY is not configured; please export it."
  exit 1
fi

## Configure AWS CLI
##   - see how to install it here: 
##     http://docs.aws.amazon.com/cli/latest/userguide/installing.html
aws configure set aws_access_key_id $AWS_ACCESS_KEY_ID
aws configure set aws_secret_access_key $AWS_SECRET_ACCESS_KEY

## Sync s3 bucket with local logs folder
aws s3 sync s3://lasp-instrumentation-logs logs/
