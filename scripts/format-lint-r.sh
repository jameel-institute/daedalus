#!/usr/bin/env bash
# Format and lint R code

# Format with Air
if command -v air &> /dev/null; then
    echo "Formatting R code with Air"
    air format .
    echo "✓ R code formatting complete"
else
    echo "!!! air not found, skipping R formatting"
fi

# Lint with Jarl (allows fixes that modify code)
if command -v jarl &> /dev/null; then
    echo "Linting R code with Jarl"
    jarl check --allow-dirty --fix .
    echo "✓ R code linting complete"
else
    echo "!!! jarl not found, skipping R linting"
fi
