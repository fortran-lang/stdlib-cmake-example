<!-- Remove this introduction after creating a new project from this template -->
# Using stdlib in your project

The Fortran standard library ([stdlib](https://github.com/fortran-lang/stdlib)) is developed by the [Fortran-lang community](https://github.com/fortran-lang).
This projects shows how to integrate stdlib in your CMake project.

For a quick start you can include stdlib as git submodule in your projects by

```
git submodule https://github.com/fortran-lang/stdlib subprojects/stdlib
```

Users must initialize the submodule themselves when building your project, unless you let CMake perform this operation on demand.

Alternatively, you can use the `FetchContent` module of CMake to retrieve the git repository while configuring the project.
A CMake snippet supporting both approaches is given here

```cmake
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
```

You can configure stdlib by setting the appropriate options before including the subproject.
Important options are

- `BUILD_SHARED_LIBS` should be set to off if you want to link statically against stdlib
- `CMAKE_MAXIMUM_RANK` determines the maximum rank of arrays supported in stdlib, the default value is 15.
  To save compile time you can reduce this value to the maximum rank needed in your application.

This project offers a ready to use integration of stdlib for CMake, including an exported library and a binary application.
Additionally, some boilerplate text in the README is available below as well as a testing setup with GitHub actions for GCC.
The general CMake style should allow you to reuse most of the CMake build files *without* modification, just change the project name, add your source files and you are ready to go.

You can [just *use this template* to create new project](https://github.com/fortran-lang/stdlib-cmake-example/generate).
Remove this introduction from the README afterwards and fill in your project details.

For more information on stdlib visit its [documentation](https://stdlib.fortran-lang.org).


<!-- Boilerplate README starting after this line -->
## Installation

To build this project you need

- A Fortran compiler supporting Fortran 2008 or later (`gfortran` or `ifort`)
- CMake version 3.14 or later
- The [fypp](https://github.com/aradi/fypp) preprocessor
- A build backend, ninja (version 1.10 or newer) or make
<!-- Add other prerequisites from your project to this list -->

Configure the build with (set the `CMAKE_INSTALL_PREFIX` to your preferred install location)

```
cmake -B _build -G Ninja -DCMAKE_INSTALL_PREFIX=$HOME/.local
```

To build the project run

```
cmake --build _build
```

Finally, you can install the project with

```
cmake --install _build
```


<!-- Do not forget to update the LICENSE file with your name! -->
## License

This project is free software: you can redistribute it and/or modify it under the terms of the [MIT license](LICENSE).
