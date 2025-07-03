

namespace daedalus {
namespace inputs {

void read_response(cpp11::list args, size_t len, real_type *dest,
                   const char *name, bool required) {
  cp11::sexp value = args[name];
  if (value == R_NilValue) {
    if (required) {
      cpp11::stop("A value is expected for '%s'", name);
    }
  } else if (!Rf_inherits(value, "daedalus_response")) {
    cpp11::stop("'%s' must inherit from `<daedalus_response>`", name);
  } else {
    check_length(value, len, name);
  }
}

}  // namespace inputs
}  // namespace daedalus
