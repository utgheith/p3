# Multi-stage Dockerfile for p3 Haskell project
# Using GHC 9.6.7 to match Stack LTS 23.0

# Base stage with dependencies
FROM haskell:9.6.7-slim AS base

# Install system dependencies
RUN apt-get update && apt-get install -y \
  git \
  curl \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

# Copy stack configuration for dependency resolution
COPY stack.yaml package.yaml ./

# Download dependencies (cached layer)
RUN stack setup && stack build --only-dependencies

# Copy source code
COPY . .

# Build the project
RUN stack build --copy-bins --local-bin-path /usr/local/bin

# Run the simulator by default
CMD ["sim"]
