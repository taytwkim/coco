sudo docker run --platform linux/amd64 --rm -v `pwd`/:/build/ csci-ga-2130/runner:latest bash -c "sh /opt/temu-driver.sh 1 0.5 $@"
