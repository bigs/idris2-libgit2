#include <stdio.h>
#include <git2.h>

git_repository *mk_git_repository() {
  return NULL;
}

git_clone_options *mk_clone_options() {
  return (git_clone_options *)malloc(sizeof(git_clone_options));
}

int is_null(void *ptr) {
  return ptr == NULL;
}
