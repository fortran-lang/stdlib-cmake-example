# SPDX-Identifier: MIT
# Configuration options for stdlib dependency
set(BUILD_SHARED_LIBS OFF)
# Set maximum supported rank for stdlib procedures
set(CMAKE_MAXIMUM_RANK 7)

# Include the stdlib project
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/stdlib/CMakeLists.txt)
  add_subdirectory("stdlib")
else()
  set("stdlib-url" "https://github.com/fortran-lang/stdlib")
  message(STATUS "Retrieving stdlib from ${stdlib-url}")
  include(FetchContent)
  FetchContent_Declare(
    "stdlib"
    GIT_REPOSITORY "${stdlib-url}"
    GIT_TAG "HEAD"
  )
  FetchContent_MakeAvailable("stdlib")
endif()

list(
  APPEND lib-deps
  "fortran_stdlib"
)
set(lib-deps "${lib-deps}" PARENT_SCOPE)
