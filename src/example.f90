! SPDX-Identifier: MIT

!> Example module to interact with stdlib
module example
  use stdlib_logger
  implicit none
  public


contains


subroutine hello_stdlib

  call global_logger%log_information("This project uses the Fortran standard library!")

end subroutine hello_stdlib


end module example
