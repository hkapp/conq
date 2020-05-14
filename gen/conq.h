/* The runtime library for generated Conq C programs.
 * Handles the input string and abstracts this aways from the compiler.
 */
#include <stdio.h>
#include <string.h>

// Compiler API

#define INIT \
  if (argc != 2) {                  \
    return -1;                      \
  }                                 \
  const char* const INPUT_VAR = argv[1];              \
  const unsigned short INPUT_LEN = strlen(INPUT_VAR); \
  unsigned short POS_VAR = 0;       \
  unsigned short MATCH_VAR = 0;

#define START_MATCH \
  MATCH_VAR = POS_VAR;

#define HAS_MORE_INPUT  HAS_MORE(1)

#define PREFIX_EQUALS(prefix, preflen) \
  HAS_MORE(preflen) && (strncmp(INPUT_NOW, prefix, preflen) == 0)

#define ADVANCE(n) \
  pos += n;

#define FINAL_SUCCESS \
  printf("Matched: >%.*s<\n", (POS_VAR - MATCH_VAR), (INPUT_VAR + MATCH_VAR)); \
  return 0;

#define FINAL_FAILURE \
  printf("No match\n"); \
  return -1;

// Internal

#define INPUT_VAR input
#define INPUT_LEN input_length
#define POS_VAR   pos
#define MATCH_VAR match
#define INPUT_NOW  (INPUT_VAR + POS_VAR)

#define HAS_MORE(n) \
  POS_VAR + (n) <= INPUT_LEN 
