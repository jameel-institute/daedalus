#!/usr/bin/env bash
# Format and lint C++ code

# Formatting with clang-format
if command -v clang-format &> /dev/null; then
    echo "Formatting C++ code..."
    find inst/include -name "*.h" ! -name "daedalus.cpp" -exec clang-format -style=google -i {} \;
    clang-format -style=google -i inst/dust/daedalus.cpp
    echo "✓ C++ code formatting complete"
else
    echo "!!! clang-format not found. Skipping C++ formatting."
fi

# Linting with cpplint
if command -v cpplint &> /dev/null; then
    echo "Linting C++ code with Cpplint..."
    cpplint --filter="-build/c++11, -build/include_subdir" inst/dust/*.cpp
    cpplint --filter="-build/c++11, -build/include_subdir" --exclude="inst/include/daedalus.h" inst/include/*.h
    echo "✓ Cpplint check complete"
else
    echo "!!! cpplint not found. Skipping cpplint checks, GHA CI may not pass!"
fi

# Linting with cppcheck
if command -v cppcheck &> /dev/null; then
    echo "Checking C++ code with Cppcheck..."
    cppcheck --std=c++14 --enable=performance,portability,warning,style --inline-suppr --error-exitcode=1 inst/dust
    cppcheck --std=c++14 --language=c++ --enable=performance,portability,warning,style --inline-suppr --error-exitcode=1 inst/include/*.h
    echo "✓ Cppcheck check complete"
else
    echo "!!! cppcheck not found. Skipping cppcheck checks, GHA CI may not pass!"
fi
