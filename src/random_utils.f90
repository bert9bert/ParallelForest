

module random_utils

use utils

implicit none

contains


subroutine init_random_seed()
! The code for this subroutine is adapted from
! https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html#RANDOM_005fSEED

    implicit none
    integer, allocatable :: seed(:)
    integer :: i, n, un, istat, read_iostat, dt(8), pid
    integer(kind=4) :: getpid  ! function return types
    integer(intdp) :: t
    logical :: fallback

    fallback = .false.

    call random_seed(size = n)
    allocate(seed(n))
    ! First try if the OS provides a random number generator
    open(unit=newunit(un), file="/dev/urandom", access="stream", &
         form="unformatted", action="read", status="old", iostat=istat)
    if (istat == 0) then
      read(un, iostat=read_iostat) seed
      if(read_iostat /= 0) fallback = .true.
      close(un)
    else
      fallback = .true.
    endif

    if(fallback) then
       ! Fallback to XOR:ing the current time and pid. The PID is
       ! useful in case one launches multiple instances of the same
       ! program in parallel.
       call system_clock(t)
       if (t == 0) then
          call date_and_time(values=dt)
          t = (dt(1) - 1970) * 365_intdp * 24 * 60 * 60 * 1000 &
               + dt(2) * 31_intdp * 24 * 60 * 60 * 1000 &
               + dt(3) * 24_intdp * 60 * 60 * 1000 &
               + dt(5) * 60 * 60 * 1000 &
               + dt(6) * 60 * 1000 + dt(7) * 1000 &
               + dt(8)
       endif
       pid = getpid()
       t = ieor(t, int(pid, kind(t)))
       do i = 1, n
          seed(i) = lcg(t)
       end do
    end if
    call random_seed(put=seed)
contains
    ! This simple PRNG might not be good enough for real work, but is
    ! sufficient for seeding a better PRNG.
    function lcg(s)
        integer :: lcg
        integer(intdp) :: s
        if (s == 0) then
            s = 104729
        else
            s = mod(s, 4294967296_intdp)
        end if
        s = mod(s * 279470273_intdp, 4294967291_intdp)
        lcg = int(mod(s, int(huge(0), intdp)), kind(0))
    end function lcg
end subroutine init_random_seed


end module random_utils
