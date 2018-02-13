set(LLVM_LINK_COMPONENTS support)

add_clang_executable(ASTMatcher
  ASTMatcher.cpp
  )
target_link_libraries(ASTMatcher PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
