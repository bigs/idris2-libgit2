#include <stdio.h>
#include <string.h>
#include <git2.h>

int git_clone_options_version() {
  return GIT_CLONE_OPTIONS_VERSION;
}

typedef struct {
  void *obj;
  int result;
} git_result;

void *identity(void *ptr) {
  return ptr;
}

git_result *git_clone_repository(const char *url, const char *local_path, git_clone_options *options) {
  git_repository *repo = NULL;
  int result = git_clone(&repo, url, local_path, options);
  git_result *out = malloc(sizeof(git_result));
  out->obj = repo;
  out->result = result;
  return out;
}

git_result *git_open_repository(const char *path) {
  git_repository *repo = NULL;
  int result = git_repository_open(&repo, path);
  git_result *out = malloc(sizeof(git_result));
  out->obj = repo;
  out->result = result;
  return out;
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
