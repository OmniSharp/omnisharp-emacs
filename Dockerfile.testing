FROM mcr.microsoft.com/dotnet/core/sdk:2.1

WORKDIR /usr/src

# Install dependencies
RUN apt-get update \
    && apt-get install -y git emacs24-nox curl make \
    && rm -rf /var/lib/apt/lists/*

# Install Cask
RUN curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
ENV PATH /root/.cask/bin:$PATH

# Install omnisharp and it's dependency packages via Cask
COPY Cask Cask
COPY omnisharp*.el ./
RUN cask install

# run cask build to check if the thing compiles to byte code
RUN ! (cask build 2>&1 | tee /dev/stderr | grep -iq "^.*\\.el:.*:Error:")

# Copy other files
# COPY doc doc
# COPY features-tbd-on-the-server-side features-tbd-on-the-server-side
# COPY ignored-from-melpa-build ignored-from-melpa-build
# COPY melpa-testing.recipe melpa-testing.recipe
COPY test/MinimalProject test/MinimalProject
COPY test/*.el test/
COPY test/buttercup-tests test/buttercup-tests
COPY ignored-from-melpa-build/*.el ignored-from-melpa-build/
COPY test-stuff test-stuff
COPY .git .git

