ARGS="$@"
sudo docker run --platform linux/amd64 --rm -v `pwd`/:/build/ csci-ga-2130/runner:latest bash -c "source ~/.profile; cd /build/; riscv32-unknown-elf-gcc $ARGS"
