#include <string.h>

#include <git2.h>

int int_size_bytes() {
  return (int)sizeof(int);
}

int size_t_size_bytes() {
  return (int)sizeof(size_t);
}

int git_clone_options_version() {
  return GIT_CLONE_OPTIONS_VERSION;
}

git_repository **make_git_repository() {
  git_repository **ptr = (git_repository **)malloc(sizeof(git_repository *));
  *ptr = NULL;
  return ptr;
}

git_repository *get_git_repository(git_repository **ptr) {
  return *ptr;
}

git_clone_options *make_clone_options() {
  return (git_clone_options *)malloc(sizeof(git_clone_options));
}

git_strarray *get_checkout_options_paths(git_checkout_options *opts) {
  if (opts == NULL) {
    return NULL;
  }

  return &opts->paths;
}

void *get_strarray_string(git_strarray *strs, int i) {
  if (strs == NULL || strs->count >= i || i < 0) {
    return NULL;
  }
  return strs->strings[i];
}

char *get_string(void *strptr) {
  char *str = (char *)strptr;
  const int bytes = strlen(str) + 1;
  char *copy = (char *)(malloc(bytes));
  memcpy(copy, str, bytes);
  return str;
}