#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int64_t bird_main() asm("bird_main");

int main(int argc, char** argv) {
  int64_t result = bird_main();
  printf("%"PRId64"\n", result);
  return 0;
}
