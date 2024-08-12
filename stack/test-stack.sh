#!/usr/bin/env bash

set -eu

for version in '9.2' '9.4' '9.6'; do
    for flag in '+os-string' ''; do
        stack_yaml="stack/stack.ghc-${version}${flag}.yaml"
        echo "### TESTING ${stack_yaml}"
        rm -rf .stack-work stack.yaml stack.yaml.lock
        cp "${stack_yaml}" stack.yaml
        stack test --pedantic
        cp stack.yaml.lock "${stack_yaml}.lock"
    done
done

rm -rf .stack-work stack.yaml stack.yaml.lock
ln -s "${stack_yaml}" stack.yaml
ln -s "${stack_yaml}.lock" stack.yaml.lock
