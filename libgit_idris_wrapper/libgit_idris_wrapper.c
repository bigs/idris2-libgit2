#include <stdio.h>
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

int is_null(void *ptr) {
  return ptr == NULL;
}
