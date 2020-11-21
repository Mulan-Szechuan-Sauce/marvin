FROM fpco/stack-build:latest

# Download the stack-docker-image-build executable and make it executable
RUN curl -L https://github.com/fpco/stack-docker-image-build/releases/download/v0.1.0.0/stack-docker-image-build > /usr/local/bin/stack-docker-image-build && chmod +x /usr/local/bin/stack-docker-image-build

# Copy over our source directory, which must include the stack.yaml file and
# all local packages
ADD ./ /src

# Kick off the build. This must be run from the directory containing the
# stack.yaml file (thus the cd /src). The executable will take care of
# everything else.
RUN cd /src && /usr/local/bin/stack-docker-image-build