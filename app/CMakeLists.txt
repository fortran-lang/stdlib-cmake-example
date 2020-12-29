# SPDX-Identifier: MIT
set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(
  prog
  "${dir}/main.f90"
)

# Example application target
add_executable(
  "${PROJECT_NAME}-exe"
  "${prog}"
)
set_target_properties(
  "${PROJECT_NAME}-exe"
  PROPERTIES
  OUTPUT_NAME "${PROJECT_NAME}"
)
target_link_libraries(
  "${PROJECT_NAME}-exe"
  PRIVATE
  "${PROJECT_NAME}-lib"
)

# Only install executable in main project
if(NOT is-subprojects)
  install(
    TARGETS
    "${PROJECT_NAME}-exe"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  )
endif()
