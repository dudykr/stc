docker run -d --restart always --name github-runner \
  -e REPO_URL="https://github.com/kdy1/stc" \
  -e RUNNER_NAME="kdy1-desktop" \
  -e RUNNER_TOKEN="$TOKEN" \
  -e RUNNER_WORKDIR="/tmp/github-runner-stc" \
  -e RUNNER_GROUP="my-group" \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /tmp/github-runner-stc:/tmp/github-runner-stc \
  myoung34/github-runner:latest
