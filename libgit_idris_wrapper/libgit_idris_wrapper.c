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

git_result *git_oid_from_string(const char *oid_str) {
  git_oid *oid = (git_oid *)malloc(sizeof(git_oid));
  int result = git_oid_fromstrp(oid, oid_str);
  git_result *out = malloc(sizeof(git_result));
  out->obj = oid;
  out->result = result;
  return out;
}

char *git_oid_to_string(git_oid *oid) {
  size_t len = GIT_OID_HEXSZ+1;
  char *oid_str = (char *)malloc(len);
  return git_oid_tostr(oid_str, len, oid);
}

git_result *git_lookup_object(git_repository *repo, const git_oid *oid, git_otype type) {
  git_object *obj = NULL;
  int result = git_object_lookup(&obj, repo, oid, type);
  git_result *out = malloc(sizeof(git_result));
  out->obj = obj;
  out->result = result;
  return out;
}

git_result *git_checkout_options_init() {
  git_checkout_options *opts = malloc(sizeof(git_checkout_options));
  int result = git_checkout_init_options(opts, GIT_CHECKOUT_OPTIONS_VERSION);
  git_result *out = malloc(sizeof(git_result));
  out->obj = opts;
  out->result = result;
  return out;
}

git_result *git_single_revparse(git_repository *repo, const char *spec) {
  git_object *obj = NULL;
  int result = git_revparse_single(&obj, repo, spec);
  git_result *out = malloc(sizeof(git_result));
  out->obj = obj;
  out->result = result;
  return out;
}
