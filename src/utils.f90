!------------------------------------------------------------------------------
!   Module of general utilities.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


module utils

implicit none

integer(kind=8) :: dummy_int64

integer, parameter :: dp = kind(0.d0)  ! double precision real
integer, parameter :: intdp = kind(dummy_int64)  ! double precision integer

private :: dummy_int64
public  :: dp, intdp


contains

! The function below is adapted from combining code at 
! http://fortranwiki.org/fortran/show/newunit and code at
! http://stackoverflow.com/questions/7876075/getting-free-unit-number-in-fortran

! This is a simple function to search for an available unit.
! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
! The UNIT value is returned by the function, and also by the optional
! argument. This allows the function to be used directly in an OPEN
! statement, and optionally save the result in a local variable.
! If no units are available, -1 is returned.
integer function newunit(unit)
  integer, intent(out), optional :: unit
! local
  integer, parameter :: LUN_MIN=10, LUN_MAX=1000
  logical :: opened
  integer :: lun
  integer :: iostat
! begin
  newunit=-1
  do lun=LUN_MIN,LUN_MAX
    inquire(unit=lun,opened=opened, iostat=iostat)
    if (iostat .ne. 0) cycle
    if (.not. opened) then
      newunit=lun
      exit
    end if
  end do
  if (present(unit)) unit=newunit
end function newunit


end module utils