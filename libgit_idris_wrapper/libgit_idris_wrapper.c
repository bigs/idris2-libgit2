#include <stdio.h>
#include <git2.h>

git_repository **mk_git_repository() {
  git_repository **ptr = (git_repository **)malloc(sizeof(git_repository *));
  *ptr = NULL;
  return ptr;
}

git_repository *get_git_repository(git_repository **ptr) {
  return *ptr;
}

git_clone_options *mk_clone_options() {
  return (git_clone_options *)malloc(sizeof(git_clone_options));
}

int is_null(void *ptr) {
  return ptr == NULL;
}
