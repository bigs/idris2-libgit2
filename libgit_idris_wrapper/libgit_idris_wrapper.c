#include <stdio.h>
#include <string.h>
#include <git2.h>

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

void *make_string(char *str) {
  return (void *)str;
}

int is_null_string(char *str) {
  return str == NULL;
}

void *null_string() {
  return NULL;
}

char *get_string(void *strptr) {
  char *str = (char *)strptr;
  const int bytes = strlen(str) + 1;
  char *copy = (char *)(malloc(bytes));
  memcpy(copy, str, bytes);
  return str;
}

void apply_clone_options(git_clone_options *opts, char *branch, int bare) {
  opts->checkout_branch = branch;
  opts->bare = bare;
}

char *clone_options_branch(git_clone_options *opts) {
  if (opts->checkout_branch == NULL) {
    printf("wat is goign on\n");
  }
  return get_string((void *)opts->checkout_branch);
}