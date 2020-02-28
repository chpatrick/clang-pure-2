#include "clang-c/Index.h"
#include <memory>
#include <vector>

typedef void IndexImpl;

/*
  The general principle here is that libclang objects that are accessible from Haskell
  (CXTranslationUnit, CXCursor, etc.) are wrapped in a struct that also contains
  a shared pointer to their parent object. This ensures that the parent object isn't destroyed
  even if all Haskell references to it go out of scope.
*/
template<typename Parent, typename T>
struct Wrapped {
  std::shared_ptr<Parent> parent;
  T obj;

  Wrapped(std::shared_ptr<Parent>& parent, T obj) : parent(parent), obj(std::move(obj)) {
  }

  Wrapped(const Wrapped<Parent, T>& other) = delete;
  Wrapped(const Wrapped<Parent, T>&& other) = delete;

  // Wrap an object with the same parent.
  template<typename Other>
  inline Wrapped<Parent, Other>* sibling(Other otherObj) {
    return new Wrapped<Parent, Other>(parent, otherObj);
  }

  // Pass the wrapped object to a function, return the result on the heap.
  template<typename Callable, class ...Args>
  inline auto call(Callable f, Args... args) {
    return alloc(f(obj, args...));
  }

  // Pass the wrapped object to a function, wrap the result with the same parent.
  template<typename Callable, class ...Args>
  inline auto callSibling(Callable f, Args... args) {
    return sibling(f(obj, args...));
  }

private:
  template<typename Other>
  static inline Other* alloc(const Other& obj) {
    return new Other(obj);
  }
};

typedef std::shared_ptr<IndexImpl> SharedIndex;
typedef Wrapped<IndexImpl, std::unique_ptr<CXTranslationUnitImpl, decltype(clang_disposeTranslationUnit)*>> WrappedTranslationUnit;
typedef std::shared_ptr<WrappedTranslationUnit> SharedTranslationUnit;
typedef Wrapped<WrappedTranslationUnit, CXCursor> WrappedCursor;
typedef Wrapped<WrappedTranslationUnit, CXType> WrappedType;
typedef Wrapped<WrappedTranslationUnit, CXSourceRange> WrappedSourceRange;
typedef Wrapped<WrappedTranslationUnit, CXSourceLocation> WrappedSourceLocation;
typedef Wrapped<WrappedTranslationUnit, CXFile> WrappedFile;
typedef Wrapped<WrappedTranslationUnit, CXToken> WrappedToken;

typedef std::vector<WrappedCursor*> ChildrenVector;

typedef std::pair<WrappedFile*, WrappedSourceLocation*> Inclusion;
typedef std::vector<Inclusion*> InclusionVector;