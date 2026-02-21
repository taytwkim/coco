# NOTE: need riscv64-unknown-elf-gcc and qemu-system-riscv32
# On Mac w/ Homebrew, can do:
#   $ brew tap riscv-software-src/riscv
#   $ brew install riscv-tools
#   $ brew install qemu

TMP_BASE="$(mktemp -t ps3.XXXXXX)"
TMP_OUT="${TMP_BASE}.out"
cleanup() { rm -f "$TMP_OUT"; }
trap cleanup EXIT

cp "$@" "$TMP_S" \
&& riscv64-unknown-elf-gcc \
   -march=rv32im -mabi=ilp32 \
   -static -nostdlib \
   -Wl,--no-warn-rwx-segments \
   -T bare-metal-qemu-stuff/linker.ld \
   bare-metal-qemu-stuff/start.S \
   "$@" \
   -o "$TMP_OUT"

if [ $? -ne 0 ]; then
  echo "Error: gcc failed"
  exit 1
fi

echo "Compilation succeeded."
echo "Running qemu..."
qemu-system-riscv32 -M virt -bios none -nographic -kernel "$TMP_OUT"
echo "Result:   $?"
