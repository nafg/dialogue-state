#!/usr/bin/env bash
# Generates .bsp/bleep.json (gitignored) so IDEs can import the worktree over BSP. Non-fatal.

bleep setup-ide || true
